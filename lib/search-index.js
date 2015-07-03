var bunyan = require('bunyan');
var _ = require('lodash');
var level = require('levelup');
var levelMultiply = require('level-multiply');
var log = bunyan.createLogger({name: 'search-index'});

module.exports = function (options) {
  var SearchIndex = {};
  //------------libs
  var defaults = {
    indexPath: 'si',
    logLevel: 'warn'
  };
  //initialize defaults options
  SearchIndex.options = _.defaults(options || {}, defaults);
  var calibrater = require('./indexing/calibrater.js');
  var deleter = require('./indexing/deleter.js');
  var docGetter = require('./search/docGetter.js');
  var indexPeek = require('./indexing/indexPeek.js');
  var indexer = require('./indexing/indexer.js');
  var matcher = require('./matchers/matcher.js');
  var replicator = require('./indexing/replicator.js');
  var searcher = require('./search/searcher.js');
  SearchIndex.indexes = (function () {
    if (SearchIndex.options && SearchIndex.options.db) {
    return level(SearchIndex.options.indexPath, 
          {valueEncoding: 'json', db: SearchIndex.options.db}
          )
  } else {
    return level(SearchIndex.options.indexPath, {valueEncoding: 'json'});
  }
  })();
  var indexesMultiply = levelMultiply(SearchIndex.indexes);
  // TODO: get rid of this.
  indexer.setLogLevel(options.logLevel);
  deleter.setLogLevel(options.logLevel);
  calibrater.setLogLevel(options.logLevel);
  searcher.setLogLevel(options.logLevel);
  indexPeek.setLogLevel(options.logLevel);
  matcher.setLogLevel(options.logLevel);
  replicator.setLogLevel(options.logLevel);
  //is there a better way of doing this?
  calibrater.getTotalDocs(SearchIndex.indexes, function (err, totalDocs) {
    searcher.setTotalDocs(totalDocs);
  });

  SearchIndex.getOptions = function () {
    return SearchIndex.options;
  };

  SearchIndex.add = function (options, batch, callback) {
    var filters = options.filters || {};
    indexer.addDocToIndex(SearchIndex.indexes, batch, options.batchName,
                          filters, callback);
  };

  SearchIndex.del = function (docID, callback) {
    deleter.deleteBatch([docID], SearchIndex.indexes, callback);
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
      callback({totalDocs: totalDocs});
    });
  };

  SearchIndex.close = function (callback) {
    SearchIndex.indexes.close(function () {
      while (SearchIndex.indexes.isOpen()) {
        console.log('closing...');
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
