var bunyan = require('bunyan');
var _ = require('lodash');
var level = require('levelup');
var tv = require('term-vector');

module.exports = function (options) {
  var SearchIndex = {};
  SearchIndex.getStopwords = function (lang) {
    return tv.getStopwords(lang).sort();
  };

  //------------libs
  var defaults = {
    deletable: true,
    fieldedSearch: true,
    indexPath: 'si',
    logLevel: 'error',
    nGramLength: 1,
    stopwords: SearchIndex.getStopwords('en'),
    fieldsToStore: 'all'
  };
  //initialize defaults options
  SearchIndex.options = _.defaults(options || {}, defaults);
  var log = SearchIndex.options.log || bunyan.createLogger({name: 'search-index'});
  log.level(SearchIndex.options.logLevel);
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
    stopwords: SearchIndex.options.stopwords
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

  SearchIndex.indexes = level(SearchIndex.options.indexPath, {
    valueEncoding: 'json',
    db: SearchIndex.options.db
  });

  calibrater.getTotalDocs(SearchIndex.indexes, function (err, totalDocs) {
    searcher.setTotalDocs(totalDocs);
  });

  SearchIndex.getOptions = function () {
    return SearchIndex.options;
  };

  var processBatchOptions = function (batchOptions) {
    var defaultFieldOptions = {
      filter: false,
      nGramLength: SearchIndex.options.nGramLength,
      searchable: true,
      weight: 1,
      fieldedSearch: SearchIndex.options.fieldedSearch
    };
    var defaults = {
      batchName: 'my batch',
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

  SearchIndex.add = function (batchOptions, batch, callback) {
    indexer.addDocToIndex(SearchIndex.indexes,
                          batch,
                          processBatchOptions(batchOptions),
                          function (err) {
                            callback(err);
                          });
  };

  SearchIndex.del = function (docID, callback) {
    if (SearchIndex.options.deletable)
      deleter.deleteBatch([docID], SearchIndex.indexes, callback);
    else
      callback(new Error('this index is non-deleteable- set "deletable: true" in startup options'));
  };

  SearchIndex.deleteBatch = function (batch, callback) {
    deleter.deleteBatch(batch, SearchIndex.indexes, callback);
  };

  SearchIndex.empty = function (callback) {
    var deleteOps = [];
    SearchIndex.indexes.createKeyStream({gte: '0', lte: '~~~~~~~~~~~~~~~~~~~'})
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

  SearchIndex.get = function (docID, callback) {
    docGetter.getDoc(SearchIndex.indexes, docID, callback);
  };

  SearchIndex.match = function (beginsWith, callback) {
    matcher.matcher(SearchIndex.indexes, beginsWith, callback);
  };

  //create a pipey and non-pipey version here. (non-pipey is for bundling with browserify)
  SearchIndex.replicate = function (readStream, callback) {
    replicator.replicateFromSnapShotStream(readStream, SearchIndex.indexes, callback);
  };

  SearchIndex.replicateBatch = function (serializedDB, callback) {
    replicator.replicateFromSnapShotBatch(serializedDB, SearchIndex.indexes, callback);
  };

  SearchIndex.search = function (q, callback) {
    searcher.search(SearchIndex.indexes, q, function (results) {
      //TODO: make error throwing real
      callback(null, results);
    });
  };

  SearchIndex.snapShotBatch = function (callback) {
    replicator.createSnapShotBatch(SearchIndex.indexes, callback);
  };

  SearchIndex.snapShot = function (callback) {
    replicator.createSnapShot(SearchIndex.indexes, callback);
  };

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

  return SearchIndex;
};
