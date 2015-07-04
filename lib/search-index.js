/*var level = require('leveljs');*/
var calibrater = require('./indexing/calibrater.js');
var deleter = require('./indexing/deleter.js');
var docGetter = require('./search/docGetter.js');
var indexPeek = require('./indexing/indexPeek.js');
var indexer = require('./indexing/indexer.js');
var matcher = require('./matchers/matcher.js');
var replicator = require('./indexing/replicator.js');
var searcher = require('./search/searcher.js');
var indexes = null;
var _ = require('lodash');
var level = require('levelup');
var tv = require('term-vector');
var log = null;

var SearchIndex = module.exports = function SearchIndex (options) {
  if (!(this instanceof SearchIndex))
    return new SearchIndex(options);
  var defaults = {
    indexPath: 'si',
    logLevel: 'error',
    stopwords: this.getStopwords('en')
  };
  //initialize defaults options
  options = _.defaults(options || {}, defaults);
  indexes = (options && options.db) ?
    level(options.indexPath, {valueEncoding: 'json', db: options.db})
    : level(options.indexPath, {valueEncoding: 'json'});
  options.log = log = require('bunyan').createLogger({name: 'search-index',
                                                      level: options.logLevel});
  indexer.setOptions(options);
  deleter.setOptions(options);
  calibrater.setOptions(options);
  searcher.setOptions(options);
  indexPeek.setOptions(options);
  matcher.setOptions(options);
  replicator.setOptions(options);

  calibrater.getTotalDocs(indexes, function (err, totalDocs) {
    searcher.setTotalDocs(totalDocs);
  });
};

SearchIndex.prototype.add = function (options, batch, callback) {
  var filters = options.filters || {};
  indexer.addDocToIndex(indexes,
                             batch,
                             options.batchName,
                             filters,
                             function (err) {
                               callback(err);
                             });
};

SearchIndex.prototype.del = function (docID, callback) {
  deleter.deleteBatch([docID], indexes, function (err) {
    return callback(err);
  });
};

SearchIndex.prototype.deleteBatch = function (batch, callback) {
  deleter.deleteBatch(batch, indexes, function (err) {
    return callback(err);
  });
};

SearchIndex.prototype.getStopwords = function (lang) {
  return tv.getStopwords(lang);
};

SearchIndex.prototype.empty = function (callbacky) {
  var deleteOptions = [];
  indexes.createKeyStream({gte: '0', lte: '~~~~~~~~~~~~~~~~~~~'})
    .on('data', function (data) {
      deleteOptions.push({type: 'del', key: data});
    })
    .on('error', function (err) {
      log.error(err, ' failed to empty index');
    })
    .on('end', function () {
      indexes.batch(deleteOptions, function (err) {
        callbacky(err);
      });
    });
};

SearchIndex.prototype.get = function (docID, callback) {
  docGetter.getDoc(indexes, docID, function (err, doc) {
    callback(err, doc);
  });
};

SearchIndex.prototype.match = function (beginsWith, callback) {
  matcher.matcher(indexes, beginsWith, function (err, match) {
    callback(err, match);
  });
};

//create a pipey and non-pipey version here. (non-pipey is for bundling with browserify)
SearchIndex.prototype.replicate = function (readStream, callback) {
  replicator.replicateFromSnapShotStream(readStream, indexes, function (err) {
    callback(err);
  });
};

SearchIndex.prototype.replicateBatch = function (serializedDB, callback) {
  replicator.replicateFromSnapShotBatch(serializedDB, indexes, function (err) {
    callback(err);
  });
};

SearchIndex.prototype.search = function (q, callback) {
  searcher.search(indexes, q, function (results) {
    //TODO: make error throwing real
    callback(null, results);
  });
};

SearchIndex.prototype.snapShotBatch = function (callback) {
  replicator.createSnapShotBatch(indexes, function (msg) {
    callback(msg);
  });
};

SearchIndex.prototype.snapShot = function (callback) {
  replicator.createSnapShot(indexes, function (msg) {
    callback(msg);
  });
};

SearchIndex.prototype.tellMeAboutMySearchIndex = function (callback) {
  calibrater.getTotalDocs(indexes, function (err, totalDocs) {
    var metadata = {};
    metadata.totalDocs = totalDocs;
    callback(metadata);
  });
};

SearchIndex.prototype.close = function (callback) {
  indexes.close(function () {
    while (indexes.isOpen()) {
      console.log('closing...');
    }
    callback(null);
  });
};

//utility methods for testing and devlopment
//******************************************
SearchIndex.prototype.indexRange = function (options, callback) {
  indexPeek.indexRange(options.start, options.stop, indexes, function (err, dump) {
    callback(err, dump);
  });
};

SearchIndex.prototype.indexValue = function (options, callback) {
  indexPeek.indexValue(options.key, indexes, function (err, value) {
    callback(err, value);
  });
};

//do a full recalibration of the index
SearchIndex.prototype.calibrate = function (callback) {
  calibrater.calibrate(indexes, function (err, msg) {
    callback(err, msg);
  });
};
