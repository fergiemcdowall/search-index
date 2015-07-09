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
    var compositeField = '';
    for (var fieldKey in doc) {
      if (batchOptions.nonSearchableFields.indexOf(fieldKey) != -1) continue;
      else if (_.isArray(doc[fieldKey])){
        //add the individual metadata tokens to the composite field,
        //such that the document is retrievable for each metadata token
        for (var k = 0; k < doc[fieldKey].length; k++)
          compositeField += ' ' + doc[fieldKey][k] + ' ';
      }
      else {
        //field is OK- add it to composite
        compositeField +=  ' ' + doc[fieldKey] + ' ';
      }
    }
    return compositeField;
  };

  var getCanDoFieldedSearchOn = function (docKeys, batchOptions) {
    if (options.fieldedSearchOnAllFieldsByDefault)
      return _.xor(docKeys, batchOptions.nonSearchableFields);
    else
      return _.without(batchOptions.canDoFieldedSearchOn, batchOptions.nonSearchableFields);
  };

  var arrayifyFilters = function (doc, filters) {
    _.forEach(filters, function (facet) {
      if (doc[facet]) {
        if (!_.isArray(doc[facet]))
          doc[facet] = [doc[facet]];
        doc[facet] = _.uniq(doc[facet]);
      }
    });
    return doc;
  };

  //TODO: clean up confusion between filters and factets
  function indexDoc (reverseIndex, doc, batchOptions, callback) {
    //use key if found, if no key is found set filename to be key.
    var fieldBatch = [],
    doc = arrayifyFilters(doc, batchOptions.filters);
    doc = removeInvalidFields(doc);
    doc['*'] = createCompositeField(doc, batchOptions);
    var canDoFieldedSearchOn = getCanDoFieldedSearchOn(Object.keys(doc), batchOptions);
    var tfValues = [];
    for (var i = 0; i < canDoFieldedSearchOn.length; i++) {
      var fieldKey = canDoFieldedSearchOn[i];
      var docVector = [];
      try {
        docVector = tv.getVector(doc[fieldKey] + '', {
          separator:/[\|' \.,\-|(\n)]+/,
          stopwords: options.stopwords,
          nGramLength: batchOptions.nGramLength
        });
      }
      catch (e) {
        log.error('Term vector errored out when getting vector: ' + e);
      }
      //wildcard char
      docVector.push(['*', 1]);
      var highestFrequencyCount = _.max(docVector, 1)[1];
      var deleteKeys = [];
      var tfValue;
      _.forEach(batchOptions.filters, function (facet) {
        _.forEach(doc[facet], function (docFacet) {
          _.forEach(docVector, function (vecElem) {
            var bloomEntry = fieldKey + '~' + vecElem[0] + '~' + facet + '~' + docFacet;
            tfValue = (vecElem[1] / highestFrequencyCount / docVector.length).toFixed(10);
            tfValues.push([bloomEntry, tfValue]);
            deleteKeys.push(bloomEntry);
          });
        });
      });
      //no facets
      _.forEach(docVector, function (vecElem) {
        var entrykey = fieldKey + '~' + vecElem[0] + '~~';
        tfValue = (vecElem[1] / highestFrequencyCount / docVector.length).toFixed(10);
        tfValues.push([entrykey, tfValue]);
        deleteKeys.push(entrykey);
      });
      fieldBatch.push({
        type: 'put',
        key: 'DELETE-DOCUMENT~' + doc.id + '~' + fieldKey,
        value: deleteKeys
      });
    }

    //document
    delete doc['*'];
    fieldBatch.push({
      type: 'put',
      key: 'DOCUMENT~' + doc.id + '~',
      value: doc
    });

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
