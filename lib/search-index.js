
var level = require('levelup'),
  levelMultiply = require('level-multiply'),
  indexPeek = require('./indexing/indexPeek.js'),
  matcher = require('./matchers/matcher.js'),
  calibrater = require('./indexing/calibrater.js'),
  indexer = require('./indexing/indexer.js'),
  deleter = require('./indexing/deleter.js'),
  replicator = require('./indexing/replicator.js'),
  searcher = require('./search/searcher.js'),
  docGetter = require('./search/docGetter.js'),
  rmdir = require('rimraf'),
  _ = require('lodash')
  searchIndexLogger = require('./logger/searchIndexLogger'),
  winston = require('winston');

var indexes = null, indexesMultiply = null;

var defaults = {
  indexPath: 'si',
  logLevel: 'info',
  logSilent: false
};

var SearchIndex = module.exports = function (options) {

  //initialize defaults options
  options = _.defaults(options || {}, defaults);

  //for further use
  this.options = options;

  indexes = (options && options.db)
    ? level(options.indexPath, {valueEncoding: 'json', db: options.db})
    : level(options.indexPath, {valueEncoding: 'json'});

  indexesMultiply = levelMultiply(indexes);

  searchIndexLogger.remove(winston.transports.Console);
  searchIndexLogger.add(winston.transports.Console, {silent: options.logSilent, level: options.logLevel});

  calibrater.getTotalDocs(indexes, function(err, totalDocs) {
    searcher.setTotalDocs(totalDocs);
  });

  return SearchIndex;
};

SearchIndex.del = function(docID, callback) {
  if (!indexes) SearchIndex();
  deleter.deleteDoc(docID, indexes, indexesMultiply, function(err) {
    callback(err);
  });
};


SearchIndex.get = function(docID, callback) {
  if (!indexes) SearchIndex();
  docGetter.getDoc(indexes, docID, function(err, doc) {
    callback(err, doc);
  });
};


SearchIndex.add = function(options, batch, callback) {
  if (!indexes) SearchIndex();
  indexer.addDocToIndex(indexes,
                        indexesMultiply,
                        batch,
                        options.batchName,
                        options.filters,
                        function(err) {
                          callback(err);
                        });
};

SearchIndex.empty = function( callback) {
  var self = this;
  if (!indexes) SearchIndex();
  var err = false;
  indexes.close(function(){
    rmdir(self.options.indexPath, function(err){
      indexes = level(self.options.indexPath, {valueEncoding: 'json'});
      indexesMultiply = levelMultiply(indexes);
      callback(err);
    });
  });
};


SearchIndex.search = function (q, callback) {
  if (!indexes) SearchIndex();
  searcher.search(indexes, indexesMultiply, q, function(results){
    //TODO: make error throwing real
    callback(false, results);
  });
};


SearchIndex.match = function(beginsWith, callback) {
  if (!indexes) SearchIndex();
  matcher.matcher(indexes, beginsWith, function(err, match) {
    callback(err, match);
  });
};

SearchIndex.tellMeAboutMySearchIndex = function(callback) {
  if (!indexes) SearchIndex();

  calibrater.getTotalDocs(indexes, function(err, totalDocs) {
    var metadata = {};
    metadata['totalDocs'] = totalDocs;
    callback(metadata);
  });
};


SearchIndex.replicate = function(readStream, callback) {
  if (!indexes) SearchIndex();
  replicator.replicateFromSnapShot(readStream, indexes, function(err) {
    callback(err);
  });
};

SearchIndex.snapShot = function(callback) {
  if (!indexes) SearchIndex();

  replicator.createSnapShot(indexes, function(msg) {
    callback(msg);
  });
};


//utility methods for testing and devlopment
//******************************************
SearchIndex.indexRange = function(options, callback) {
  if (!indexes) SearchIndex();
  indexPeek.indexRange(options.start, options.stop, indexes, function(err, dump) {
    callback(err, dump);
  });
};
SearchIndex.indexValue = function(options, callback) {
  if (!indexes) SearchIndex();
  indexPeek.indexValue(options.key, indexes, function(err, value) {
    callback(err, value);
  });
};

//do a full recalibration of the index
SearchIndex.calibrate = function(callback) {
  if (!indexes) SearchIndex();

  calibrater.calibrate(indexes, function(err, msg) {
    callback(err, msg);
  });
};
