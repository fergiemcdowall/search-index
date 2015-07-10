/*jshint -W083 */ //makes jslint overlook functions in lodash for-loops

var _ = require('lodash');
var async = require('async');
var calibrater = require('./calibrater.js')();
var deleter = require('./deleter.js')();
var hash = require('object-hash');
var tv = require('term-vector');
var shadower = require('bunyan-shadower');

module.exports = function (options) {
  var log = shadower((options) ? options.log : undefined);
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
        memo.reducedTF['TF~' + key] = _.pluck(item, 'KeyID');
        memo.reducedTFSortOnTF['RI~' + key] = _.pluck(item, 'ReverseIndex');
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
    for (var i = 0; i < Object.keys(doc).length; i++)
      if (searchableFields.indexOf(Object.keys(doc)[i]) == -1)
        delete doc[Object.keys(doc)[i]]
    var compositeField = _.flatten(_.values(doc));
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
    var specifiedSearchable = _.compact(_.map(batchOptions.fieldOptions, function(item){
      if (item.searchable) return item.fieldName;
    }))
    var specifiedNonSearchable = _.compact(_.map(batchOptions.fieldOptions, function(item){
      if (!item.searchable) return item.fieldName;
    }))
    for (var i = 0; i < docKeys.length; i++) {
      var key = docKeys[i]
      if ((searchableDefault == true) && (specifiedNonSearchable.indexOf(key) == -1)) {
        searchableFields.push(key)
      }
      else if (specifiedSearchable.indexOf(key) != -1)
        searchableFields.push(key)
    }
    return searchableFields;
  }

  var weightFields = function (doc, batchOptions) {
    var weightedDoc = {};
    for (var fieldKey in doc) {
      var fieldOptions = _.defaults(_.find(batchOptions.fieldOptions, 'fieldName', fieldKey) || {}, batchOptions.defaultFieldOptions);
      weightedDoc[fieldKey] = [{}]
      weightedDoc[fieldKey][0]['field'] = doc[fieldKey];
      weightedDoc[fieldKey][0]['weight'] = fieldOptions.weight;
    }
    return weightedDoc;
  }
 
  //TODO: clean up confusion between filters and factets
  function indexDoc (reverseIndex, doc, batchOptions, callback) {
    //use key if found, if no key is found set filename to be key.
    var fieldBatch = [];
    var facets = arrayifyFilters(doc, batchOptions.filters);
    doc = removeInvalidFields(doc);
    doc = weightFields(doc, batchOptions);
    doc['*'] = createCompositeField(doc, batchOptions);
    var searchableFields = getSearchableFields(Object.keys(doc), batchOptions)
    var tfValues = [];
    for (var j in searchableFields) {
      var fieldKey = searchableFields[j];
      if (!doc.hasOwnProperty(fieldKey)) continue;
      var fieldOptions = batchOptions.defaultFieldOptions;
      fieldOptions.fieldName = fieldKey;
      fieldOptions = _.defaults(_.find(batchOptions.fieldOptions, 'fieldName', fieldKey) || {}, batchOptions.defaultFieldOptions);
      if (!fieldOptions.fieldedSearch && fieldOptions.fieldName != '*') continue;
      var docVector = [];
      for (var i = 0; i < doc[fieldKey].length; i++) {
        docVector[i] = {};
        try {
          docVector[i]['field'] = tv.getVector(doc[fieldKey][i].field + '', {
            separator:/[\|' \.,\-|(\n)]+/,
            stopwords: options.stopwords,
            nGramLength: batchOptions.nGramLength
          });
          docVector[i]['weight'] = doc[fieldKey][i].weight;
        }
        catch (e) {
          log.error('Term vector errored out when getting vector: ' + e);
        }
      };

      var deleteKeys = [];
      var tfValue;
      _.forEach(batchOptions.filters, function (facet) {
        _.forEach(facets[facet], function (docFacet) {
          // _.forEach(docVector, function (vecElem) {
          //   var bloomEntry = fieldKey + '~' + vecElem[0] + '~' + facet + '~' + docFacet;
          //   tfValue = (vecElem[1] / highestFrequencyCount / docVector.length).toFixed(10);
          //   tfValues.push([bloomEntry, tfValue]);
          //   deleteKeys.push(bloomEntry);
          // });
           _.forEach(docVector, function (vecElem) {
             vecElem.field.push(['*', 1]);
             var highestFrequencyCount = _.max(vecElem.field, 1)[1];
             var weight = vecElem.weight;
             _.forEach(vecElem.field, function (vecField) {
               var entrykey = fieldKey + '~' + vecField[0] + '~' + facet + '~' + docFacet;
               tfValue = +weight + (vecField[1] / highestFrequencyCount / docVector.length).toFixed(10);
               var existingEntry = _.findIndex(tfValues, function(chr) { return chr[0] == entrykey;})
               if (existingEntry != -1)
                 tfValues[existingEntry] = [entrykey, (+tfValues[existingEntry][1] + +tfValue)]
               else
                 tfValues.push([entrykey, tfValue]);
               deleteKeys.push(entrykey);
             });
           })
        });
      });
      //no facets


//      console.log(JSON.stringify(docVector, null, 2))

      _.forEach(docVector, function (vecElem) {
        var weight = vecElem.weight;
        var highestFrequencyCount = _.max(vecElem.field, 1)[1];
        vecElem.field.push(['*', 1]);
        _.forEach(vecElem.field, function (vecField) {
          var entrykey = fieldKey + '~' + vecField[0] + '~~';
          tfValue = +weight + (vecField[1] / highestFrequencyCount / docVector.length).toFixed(10);
          var existingEntry = _.findIndex(tfValues, function(chr) { return chr[0] == entrykey;})
          if (existingEntry != -1)
            tfValues[existingEntry] = [entrykey, (+tfValues[existingEntry][1] + +tfValue)]
          else
            tfValues.push([entrykey, tfValue]);
          // if (entrykey == '*~stock~~') console.log(weight)
          // if (entrykey == '*~stock~~') console.log([entrykey, tfValue])
          deleteKeys.push(entrykey);
        });
      });

      fieldBatch.push({
        type: 'put',
        key: 'DELETE-DOCUMENT~' + doc.id[0].field + '~' + fieldKey,
        value: deleteKeys
      });
    }

    //document
    delete doc['*'];
    fieldBatch.push({
      type: 'put',
      key: 'DOCUMENT~' + doc.id[0].field + '~',
      value: doc
    });


//    console.log(tfValues)

    //put key-values into database
    reverseIndex.batch(fieldBatch, function (err) {
      if (err) {
        log.error(err);
        return callback(err);
      }
      return callback(null, tfValues);
    });
  }

  indexer.addDocToIndex = function (indexes, batch, batchOptions, callbacky) {
    var salt = 0;
    var deleteBatch = _.pluck(_.filter(batch, 'id'), 'id');
    deleter.deleteBatch(deleteBatch, indexes, function (err) {
      if (err) log.warn(err);
      var tf = {};
      async.eachSeries(batch, function (item, callback) {
        if (!_.isPlainObject(item))
          return callbacky(new Error('Malformed document'), {});
        if (!item.id)
          item.id = (++salt) + '-' + hash(item);
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
