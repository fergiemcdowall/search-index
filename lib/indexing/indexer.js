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
  function reduceTF (tf) {
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
  }

  //TODO: clean up confusion between filters and factets
  function indexDoc (reverseIndex, doc, batchOptions, callback) {
    //use key if found, if no key is found set filename to be key.
    var fieldBatch = [],
    fieldKey,
    highestFrequencyCount,
    deleteKeys;

    var facety = [];
    if (batchOptions.filters) {
      for (var i = 0; i < batchOptions.filters.length; i++) {
        //doc has some facet values
        if (doc[batchOptions.filters[i]]) {
          //arrayify non array filter fields
          if (Object.prototype.toString.call(doc[batchOptions.filters[i]]) !== '[object Array]')
            doc[batchOptions.filters[i]] = [doc[batchOptions.filters[i]]];
          //loop though this facet field
          for (var j = 0; j < doc[batchOptions.filters[i]].length; j++) {
            facety.push(batchOptions.filters[i] + '~' + doc[batchOptions.filters[i]][j]);
          }
          doc[batchOptions.filters[i]] = _.uniq(doc[batchOptions.filters[i]]);
        }
      }
    }

    var compositeField = '';
    for (fieldKey in doc) {
      if (Object.prototype.toString.call(doc[fieldKey]) === '[object Array]') {
        //add the individual metadata tokens to the composite field,
        //such that the document is retrievable for each metadata token
        for (var k = 0; k < doc[fieldKey].length; k++)
          compositeField += ' ' + doc[fieldKey][k] + ' ';
      }
      //throw away fields that have null value
      else if (doc[fieldKey] === null) {
        delete doc[fieldKey];
        log.warn(doc.id + ': ' + fieldKey + ' field is null, SKIPPING');
      }
      //only index fields that are strings or numbers
      else if (!(((typeof doc[fieldKey]) == 'string') ||
                 ((typeof doc[fieldKey]) == 'number'))) {
        delete doc[fieldKey];
        log.warn(doc.id + ': ' + fieldKey +
                 ' field not string or array, SKIPPING');
      }
      else {
        //field is OK- add it to composite
        compositeField +=  ' ' + doc[fieldKey] + ' ';
      }
    }
    doc['*'] = compositeField;
    var tfValues = [];
    for (fieldKey in doc) {
      if (!doc.hasOwnProperty(fieldKey)) continue;
      var reverseIndexValue = {};
      reverseIndexValue.filters = facety;
      var docVector = [];
      try {
        docVector = tv.getVector(doc[fieldKey] + '', {
          separator:/[\|' \.,\-|(\n)]+/,
          stopwords: options.stopwords,
          nGramLength: batchOptions.nGramLength
        });
      }
      catch (e) {log.error(e)}
      //wildcard char
      docVector.push(['*', 1]);
      highestFrequencyCount = 0;
      for (var iii = 0; iii < docVector.length; iii++)
        if (docVector[iii][1] > highestFrequencyCount) highestFrequencyCount = docVector[iii][1];
      deleteKeys = [];
      //work out facet keys for bloom filter
      var tfValue = 0;
      for (var ii = 0; ii < batchOptions.filters.length; ii++) {
        //doc has some facet values
        if (doc[batchOptions.filters[ii]]) {
          //loop though this facet field
          for (var jj = 0; jj < doc[batchOptions.filters[ii]].length; jj++) {
            for (var kk = 0; kk < docVector.length; kk++) {
              var bloomEntry = fieldKey + '~' + docVector[kk][0] + '~' +
                batchOptions.filters[ii] + '~' + doc[batchOptions.filters[ii]][jj];
              tfValue = (docVector[kk][1] / highestFrequencyCount / docVector.length).toFixed(10);
              tfValues.push([bloomEntry, tfValue]);
              deleteKeys.push(bloomEntry);
            }
          }
        }
      }
      //no facets
      for (var kkk = 0; kkk < docVector.length; kkk++) {
        var entrykey = fieldKey + '~' + docVector[kkk][0] + '~~';
        tfValue = (docVector[kkk][1] / highestFrequencyCount / docVector.length).toFixed(10);
        tfValues.push([entrykey, tfValue]);
        deleteKeys.push(entrykey);
      }
      //    deleteKeys.push(docDeleteIndexKey);
      fieldBatch.push({
        type: 'put',
        key: 'DELETE-DOCUMENT~' + doc.id + '~' + fieldKey,
        value: deleteKeys
      });
    }
    //generate fielded document vector for weighting

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
    var deleteBatch = [];
    var salt = 0;
    for (var i = 0; i < batch.length; i++)
      if (batch[i].id) deleteBatch.push(batch[i].id);
    deleter.deleteBatch(deleteBatch, indexes, function (err) {
      if (err) log.warn(err);
      var tf = {};
      async.eachSeries(batch, function (item, callback) {
        if (Object.prototype.toString.call(item) != '[object Object]')
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
        if (Object.keys(tf).length > 0) {
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
