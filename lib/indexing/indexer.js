/*jshint -W083 */ //makes jslint overlook functions in lodash for-loops

var _ = require('lodash');
var async = require('async');
var calibrater = require('./calibrater.js')();
var deleter = require('./deleter.js')();
var hash = require('object-hash');
var tv = require('term-vector');
var skeleton = require('log-skeleton');

module.exports = function (options) {
  var log = skeleton((options) ? options.log : undefined);
  var indexer = {};

  var reduceTF = function (tf) {
    return _(tf).map(function (item, key) {
      return _.map(item, function (token) {
        return {
          token: token[0],
          KeyID: key,
          ReverseIndex: [token[1], key]
        };
      });
    })
      .flatten()
      .groupBy('token')
      .reduce(function (memo, item, key) {
        memo.reducedTF['TF￮' + key] = _.pluck(item, 'KeyID');
        memo.reducedTFSortOnTF['RI￮' + key] = _.pluck(item, 'ReverseIndex');
        return memo;
      }, {
        reducedTF: {},
        reducedTFSortOnTF: {}
      });
  };

  var removeInvalidFields = function (doc) {
    for (var fieldKey in doc) {
      if (_.isArray(doc[fieldKey])) continue;
      else if (doc[fieldKey] === null) {
        delete doc[fieldKey];
        log.warn(doc.id + ': ' + fieldKey + ' field is null, SKIPPING');
      }
      //only index fields that are strings or numbers
      else if (!(_.isString(doc[fieldKey]) || _.isNumber(doc[fieldKey]))) {
        delete doc[fieldKey];
        log.warn(doc.id + ': ' + fieldKey +
                 ' field not string or array, SKIPPING');
      }
    }
    return doc;
  };

  var createCompositeField = function (doc, batchOptions) {
    var searchableFields = getSearchableFields(Object.keys(doc), batchOptions);
    var compositeField = _.flatten(_.values(_.pick(doc, searchableFields)));
    _.map(compositeField, function (fieldItem) {
      if (_.isArray(fieldItem.field))
        fieldItem.field = fieldItem.field.join();
      return fieldItem;
    });
    return compositeField;
  };

  var arrayifyFilters = function (doc, filters) {
    var facets = {};
    _.forEach(filters, function (facet) {
      if (doc[facet]) {
        if (!_.isArray(doc[facet]))
          facets[facet] = [doc[facet]];
        facets[facet] = _.uniq(doc[facet]);
      }
    });
    return facets;
  };

  var getSearchableFields = function (docKeys, batchOptions) {
    var searchableFields = [];
    var searchableDefault = batchOptions.defaultFieldOptions.searchable;
    var specifiedSearchable = _.compact(_.map(batchOptions.fieldOptions, function (item) {
      if (item.searchable) return item.fieldName;
    }));
    var specifiedNonSearchable = _.compact(_.map(batchOptions.fieldOptions, function (item) {
      if (!item.searchable) return item.fieldName;
    }));
    _.forEach(docKeys, function (key) {
      if ((searchableDefault === true) && (specifiedNonSearchable.indexOf(key) == -1))
        searchableFields.push(key);
      else if (specifiedSearchable.indexOf(key) != -1)
        searchableFields.push(key);
    });
    return searchableFields;
  };

  var weightFields = function (doc, batchOptions) {
    var weightedDoc = {};
    _.forEach(Object.keys(doc), function (fieldKey) {
      var fieldOptions = _.defaults(_.find(batchOptions.fieldOptions, 'fieldName', fieldKey) || {}, batchOptions.defaultFieldOptions);
      weightedDoc[fieldKey] = [{}];
      _.assign(weightedDoc[fieldKey][0], fieldOptions);
      weightedDoc[fieldKey][0].field = doc[fieldKey];
      // weightedDoc[fieldKey][0].weight = fieldOptions.weight;
    });
    weightedDoc['*'] = createCompositeField(weightedDoc, batchOptions);
    return weightedDoc;
  };

  var generateKeys = function (docVector, searchableField, facet, docFacet) {
    var returnObject = {};
    returnObject.deleteKeys = [];
    returnObject.tfValues = [];
    _.forEach(docVector, function (vecElem) {
      vecElem.field.push(['*', 1]);
      var highestFrequencyCount = _.max(vecElem.field, 1)[1];
      var weight = vecElem.weight;
      _.forEach(vecElem.field, function (vecField) {
        var entrykey = searchableField + '￮' + vecField[0] + '￮' + facet + '￮' + docFacet;
        var tfValue = +weight + (vecField[1] / highestFrequencyCount / docVector.length).toFixed(10);
        var existingEntry = _.findIndex(returnObject.tfValues, function (chr) { return chr[0] == entrykey;});
        if (existingEntry != -1)
          returnObject.tfValues[existingEntry] = [entrykey, ((+returnObject.tfValues[existingEntry][1]) + (+tfValue))];
        else
          returnObject.tfValues.push([entrykey, tfValue]);
        returnObject.deleteKeys.push(entrykey);
      });
    });
    return returnObject;
  };

  //TODO: clean up confusion between filters and factets
  var indexDoc = function (reverseIndex, doc, batchOptions, callback) {
    //use key if found, if no key is found set filename to be key.
    var fieldBatch = [];
    var facets = arrayifyFilters(doc, batchOptions.filters);
    var docToBeStored = removeInvalidFields(doc);
    var docToBeIndexed = weightFields(docToBeStored, batchOptions);
    var searchableFields = getSearchableFields(Object.keys(docToBeIndexed), batchOptions);
    var tfValues = [];
    _.forEach(searchableFields, function (searchableField) {
      if (!docToBeIndexed[searchableField][0].fieldedSearch && searchableField != '*') return;
      var docVector = [];
      _.forEach(docToBeIndexed[searchableField], function (docField) {
        var vec = {};
        try {
          vec.field = tv.getVector(docField.field + '', {
            separator: batchOptions.separator,
            stopwords: batchOptions.stopwords,
            nGramLength: docField.nGramLength
          });
          vec.weight = docField.weight;
          docVector.push(vec);
        }
        catch (e) {
          log.error('Term vector errored out when getting vector: ' + e);
        }
      });
      var deleteKeys = [];
      _.forEach(batchOptions.filters, function (facet) {
        _.forEach(facets[facet], function (docFacet) {
          var obj = generateKeys(docVector, searchableField, facet, docFacet);
          deleteKeys = deleteKeys.concat(obj.deleteKeys);
          tfValues = tfValues.concat(obj.tfValues);
        });
      });
      var obj = generateKeys(docVector, searchableField, '', '');
      deleteKeys = deleteKeys.concat(obj.deleteKeys);
      tfValues = tfValues.concat(obj.tfValues);
      if (options.deletable) {
        fieldBatch.push({
          type: 'put',
          key: 'DELETE-DOCUMENT￮' + docToBeIndexed.id[0].field + '￮' + searchableField,
          value: _.uniq(deleteKeys.sort(), true)
        });
      }
    });
    //document
    if (batchOptions.fieldsToStore == 'all')
      batchOptions.fieldsToStore = Object.keys(docToBeStored);
    fieldBatch.push({
      type: 'put',
      key: 'DOCUMENT￮' + doc.id + '￮',
      value: _.pick(docToBeStored, batchOptions.fieldsToStore)
    });

    //put key-values into database
    reverseIndex.batch(fieldBatch, function (err) {
      if (err) {
        log.error(err);
        return callback(err);
      }
      return callback(null, tfValues);
    });
  };

  indexer.addDocToIndex = function (indexes, batch, batchOptions, callbacky) {
    var salt = 0;
    if (!_.isArray(batch) && _.isPlainObject(batch)) {
      batch = [batch];
    }

    var deleteBatch = _.pluck(_.filter(batch, 'id'), 'id');
    deleter.deleteBatch(deleteBatch, indexes, function (err) {
      if (err) log.warn(err);
      var tf = {};
      async.eachSeries(batch, function (item, callback) {
        if (!_.isPlainObject(item))
          return callbacky(new Error('Malformed document'), {});
        if (!item.id)
          item.id = (++salt) + '-' + hash(item);
        batchOptions = _.defaults(batchOptions, options);
        log.info('indexing ' + item.id);
        indexDoc(indexes, item, batchOptions, function (err, tfValues) {
          tf[item.id] = tfValues;
          return callback(err);
        });
      }, function (err) {
        if (err) log.warn(err);
        if (_.keys(tf).length > 0) {
          calibrater.incrementallyCalibrate(indexes, reduceTF(tf), function (err) {
            log.info('indexed ' + batchOptions.batchName);
            return callbacky(err);
          });
        }
        else {callbacky(new Error('No docs to add'));}
      });
    });
  };

  return indexer;
};
