/**
 * search-index module.
 * @module search-index
 */

var bunyan = require('bunyan');
var _ = require('lodash');
var level = require('levelup');
var tv = require('term-vector');

/**
get an instance of search-index
@param {Object=} options
@param {boolean=} [options.deletable=true] Can documents be deleted, or re-added?
@param {boolean=} [options.fieldedSearch=true] Can documents fields be searched individually?
@param {string=} [options.indexPath="si"] Name of (path to) levelDB store
@param {string=} [options.logLevel="error"] Bunyan.js log level
@param {(number|number[]|Object)=} [options.nGramLength=1] xooooooooooooooooooooooooooooooox The length of ngrams to return, can be either a number (single length), and array of numbers (mulitple lengths), or an object containing a number value for 'gte' and 'lte' (a range of lengths)
@param {(string[])=} [options.stopwords=require('stopword').getStopwords()] An array of words that will be ignored
@param {string=} [options.fieldsToStore="all"] Which fields to store (you may wish to only store links/references/IDs instead of whole documents)
*/
module.exports = function (options) {
  var SearchIndex = {};
  //------------libs
  var defaults = {
    deletable: true,
    fieldedSearch: true,
    indexPath: 'si',
    logLevel: 'error',
    nGramLength: 1,
    stopwords: tv.getStopwords('en').sort(),
    separator: /[\|' \.,\-|(\n)]+/,
    fieldsToStore: 'all'
  };
  //initialize defaults options
  SearchIndex.options = _.clone(_.defaults(options || {}, defaults));
  var log = SearchIndex.options.log || bunyan.createLogger({
    name: 'search-index',
    level: SearchIndex.options.logLevel
  });
  var docGetter = require('./search/docGetter.js');
  var calibrater = require('./indexing/calibrater.js')({
    log: log.child({component: 'calibrater'})
  });
  var deleter = require('./indexing/deleter.js')({
    log: log.child({component: 'deleter'})
  });
  var indexPeek = require('./indexing/indexPeek.js')({
    log: log.child({component: 'indexPeek'})
  });
  var indexer = require('./indexing/indexer.js')({
    deletable: SearchIndex.options.deletable,
    fieldedSearch: SearchIndex.options.fieldedSearch,
    fieldsToStore: SearchIndex.options.fieldsToStore,
    log: log.child({component: 'indexer'}),
    stopwords: SearchIndex.options.stopwords,
    separator: SearchIndex.options.separator
  });
  var matcher = require('./matchers/matcher.js')({
    log: log.child({component: 'matcher'})
  });
  var replicator = require('./indexing/replicator.js')({
    log: log.child({component: 'replicator'})
  });
  var searcher = require('./search/searcher.js')({
    log: log.child({component: 'searcher'}),
    stopwords: SearchIndex.options.stopwords
  });
  var processBatchOptions = function (batchOptions) {
    var defaultFieldOptions = {
      filter: false,
      nGramLength: SearchIndex.options.nGramLength,
      searchable: true,
      weight: 1,
      fieldedSearch: SearchIndex.options.fieldedSearch
    };
    var defaults = {
      batchName: 'Batch at ' + new Date().toISOString(),
      fieldOptions: [],
      fieldsToStore: SearchIndex.options.fieldsToStore,
      defaultFieldOptions: defaultFieldOptions
    };
    batchOptions = _.defaults(batchOptions || {}, defaults);
    batchOptions.filters = _.pluck(_.filter(batchOptions.fieldOptions, 'filter'), 'fieldName');
    if (_.find(batchOptions.fieldOptions, 'fieldName', '*') == -1)
      batchOptions.fieldOptions.push(defaultFieldOptions('*'));
    return batchOptions;
  };

  SearchIndex.indexes = level(SearchIndex.options.indexPath, {
    valueEncoding: 'json',
    db: SearchIndex.options.db
  });

  calibrater.getTotalDocs(SearchIndex.indexes, function (err, totalDocs) {
    searcher.setTotalDocs(totalDocs);
  });

  /**
     Returns an array of stopwords
     @function getStopwords
     @param {string} [lang=en] a language code
     @returns {string[]} The stopword list
  */
  SearchIndex.getStopwords = function (lang) {
    return tv.getStopwords(lang).sort();
  };

  /**
     Adds a document to the index
     @function add
     @param {Object[]} batch a batch of documents to be indexed
     @param {Object=} batchOptions
     @param {string=} [batchOptions.batchName="my batch"] Name of batch
     @param {Object[]} [batchOptions.fieldOptions=[]] each object can be as defaultFieldOptions with an extra field "fieldName" to identify fields
     @param {string[]} [batchOptions.fieldsToStore="all"] array of fieldnames to be stored, defaults to all fields
     @param {Object=} batchOptions.defaultFieldOptions
     @param [batchOptions.defaultFieldOptions.filter=true] this field can be used for facets and filters
     @param [batchOptions.defaultFieldOptions.nGramLength=set at startup] phrase search
     @param [batchOptions.defaultFieldOptions.searchable=true] is this field searchable?
     @param [batchOptions.defaultFieldOptions.weight=1] weight that this field has for relevancy
     @param [batchOptions.defaultFieldOptions.fieldedSearch=set at startup] if this field can be searched on individually
     @param {callback(err)} callback - A callback to run.
  */
  SearchIndex.add = function (batch, batchOptions, callback) {
    if (arguments.length == 2 && _.isFunction(arguments[1])) {
      callback = batchOptions;
      batchOptions = undefined;
    }
    indexer.addDocToIndex(SearchIndex.indexes,
                          batch,
                          processBatchOptions(batchOptions),
                          callback);
  };

  /**
     Delete a document with the given ID
     @function del
     @param {string} docID - A document ID.
     @param {callback(err)} callback - A callback to run.
  */
  SearchIndex.del = function (docID, callback) {
    if (SearchIndex.options.deletable)
      deleter.deleteBatch([docID], SearchIndex.indexes, callback);
    else
      callback(new Error('this index is non-deleteable- set "deletable: true" in startup options'));
  };

  /**
     Delete documents with the given IDs
     @function deleteBatch
     @param {string[]} docID - An array of document IDs.
     @param {callback(err)} callback - A callback to run.
  */
  SearchIndex.deleteBatch = function (batch, callback) {
    deleter.deleteBatch(batch, SearchIndex.indexes, callback);
  };

  /**
     Empty the index
     @function empty
     @param {callback(err)} callback - A callback to run.
  */
  SearchIndex.empty = function (callback) {
    var deleteOps = [];
    SearchIndex.indexes.createKeyStream({gte: '0', lte: '￮'})
      .on('data', function (data) {
        deleteOps.push({type: 'del', key: data});
      })
      .on('error', function (err) {
        log.error(err, ' failed to empty index');
      })
      .on('end', function () {
        SearchIndex.indexes.batch(deleteOps, callback);
      });
  };

  /**
     Get document with the given ID
     @function get
     @param {string} docID - An document ID.
     @param {callback(err, result)} callback - A callback to run.
  */
  SearchIndex.get = function (docID, callback) {
    docGetter.getDoc(SearchIndex.indexes, docID, callback);
  };

  /**
     Get words from the index that match the input string. Return them weighted by frequency
     @function match
     @param {Object=} options
     @param {string=} [options.beginsWith=""] The string to match.
     @param {string=} [options.field="*"] The field to match on.
     @param {number=} [options.threshold=3] All strings shorter that this number are ignored.
     @param {string=} [options.limit=10] The maximum limit of suggestions.
     @param {string=} [options.type="simple"] Can be 'simple', 'ID', or 'count'.
     @param {callback(err, result)} callback - A callback to run.
  */
  SearchIndex.match = function (options, callback) {
    matcher.matcher(SearchIndex.indexes, options, callback);
  };

  /**
     Feed another index into this (empty) index
     @function replicate
     @param {readStream} readStream - A readStream from a saved search-index.
     @param {callback(err)} callback - A callback to run.
  */
  SearchIndex.replicate = function (readStream, callback) {
    replicator.replicateFromSnapShotStream(readStream, SearchIndex.indexes, callback);
  };

  /**
     Feed another index into this (empty) index
     @function replicateBatch
     @param {Object[]} serializedDB - A serialized levelDB.
     @param {callback(err)} callback - A callback to run.
  */
  SearchIndex.replicateBatch = function (serializedDB, callback) {
    replicator.replicateFromSnapShotBatch(serializedDB, SearchIndex.indexes, callback);
  };

  /**
     Search the index
     @function search
     @param {Object[]} q - a query object TODO: add a good explanation here.
     @param {callback(err, result)} callback - A callback to run that returns the results or an error.
  */
  SearchIndex.search = function (q, callback) {
    searcher.search(SearchIndex.indexes, q, function (results) {
      //TODO: make error throwing real
      callback(null, results);
    });
  };

  /**
     Create a snapshot of this index
     @function snapShotBatch
     @param {callback(snapshot)} callback - returns snapshot as an array.
  */
  SearchIndex.snapShotBatch = function (callback) {
    replicator.createSnapShotBatch(SearchIndex.indexes, callback);
  };

  /**
     Create a snapshot of this index
     @function snapShot
     @param {callback(snapshot)} callback - returns snapshot as a stream.
  */
  SearchIndex.snapShot = function (callback) {
    replicator.createSnapShot(SearchIndex.indexes, callback);
  };

  /**
     Create a snapshot of this index
     @function tellMeAboutMySearchIndex
     @param {callback(info)} callback - returns information about the search-index
  */
  SearchIndex.tellMeAboutMySearchIndex = function (callback) {
    calibrater.getTotalDocs(SearchIndex.indexes, function (err, totalDocs) {
      callback(
        {
          totalDocs: totalDocs,
          options: SearchIndex.options
        }
      );
    });
  };

  /**
     Close this index
     @function close
     @param {callback(err)} callback - Did it error?
  */
  SearchIndex.close = function (callback) {
    SearchIndex.indexes.close(function () {
      while (SearchIndex.indexes.isOpen()) {
        log.info('closing...');
      }
      callback(null);
    });
  };

  //utility methods for testing and devlopment
  //******************************************
  SearchIndex.indexRange = function (options, callback) {
    indexPeek.indexRange(options.start, options.stop, SearchIndex.indexes, callback);
  };

  SearchIndex.indexValue = function (options, callback) {
    indexPeek.indexValue(options.key, SearchIndex.indexes, callback);
  };

  //do a full recalibration of the index
  SearchIndex.calibrate = function (callback) {
    calibrater.calibrate(SearchIndex.indexes, callback);
  };

  SearchIndex.log = log;
  return SearchIndex;
};
