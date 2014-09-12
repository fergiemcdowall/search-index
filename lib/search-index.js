var fs = require('fs'),
  level = require('level'),
  levelMultiply = require('level-multiply'),
  indexPeek = require('./indexing/indexPeek.js'),
  matcher = require('./matchers/matcher.js'),
  calibrater = require('./indexing/calibrater.js'),
  indexer = require('./indexing/indexer.js'),
  deleter = require('./indexing/deleter.js'),
  replicator = require('./indexing/replicator.js'),
  searcher = require('./mapreduce/searcher.js'),
  docGetter = require('./mapreduce/docGetter.js'),
  rmdir = require('rimraf');

var indexes = null, indexesMultiply = null;

var defaults = {
  indexPath: 'si'
};

var SearchIndex = module.exports = function (options) {
  options = options || defaults;

  indexes = level(options.indexPath, {valueEncoding: 'json'}),
  indexesMultiply = levelMultiply(indexes);

  calibrater.getTotalDocs(indexes, function(totalDocs) {
    searcher.setTotalDocs(totalDocs);
  });
};

SearchIndex.del = function(docID, callback) {
  if (!indexes) SearchIndex();

  deleter.deleteDoc(docID, indexes, indexesMultiply, function(msg) {
    callback(msg);
  });
};


SearchIndex.get = function(docID, callback) {
  if (!indexes) SearchIndex();

  docGetter.getDoc(indexes, indexesMultiply, docID, function(msg) {
    callback(msg);
  });
};


SearchIndex.add = function(batchString, batchName, filters, callback) {
  if (!indexes) SearchIndex();

  indexer.addDocToIndex(indexes, indexesMultiply, batchString, batchName, filters, function(msg) {
    callback(msg);
  });
};

SearchIndex.empty = function( callback) {
  if (!indexes) SearchIndex();

  indexes.close(function(){
    rmdir('si', function(error){
      indexes = level('si', {valueEncoding: 'json'});
      indexesMultiply = levelMultiply(indexes);
      callback('emptied');
    });
  });
};


SearchIndex.search = function (q, callback) {
  if (!indexes) SearchIndex();

  searcher.search(indexes, indexesMultiply, q, function(msg){
    callback(msg);
  });
};


SearchIndex.match = function(beginsWith, callback) {
  if (!indexes) SearchIndex();

  matcher.matcher(indexes, beginsWith, function(msg) {
    callback(msg);
  });
};

SearchIndex.tellMeAboutMySearchIndex = function(callback) {
  if (!indexes) SearchIndex();

  calibrater.getTotalDocs(indexes, function(totalDocs) {
    var metadata = {};
    metadata['totalDocs'] = totalDocs;
    callback(metadata);
  });
};


SearchIndex.replicate = function(readStream, callback) {
  if (!indexes) SearchIndex();

  replicator.replicateFromSnapShot(readStream, indexes, function(msg) {
    callback(msg);
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
SearchIndex.indexRange = function(start, stop, callback) {
  if (!indexes) SearchIndex();

  indexPeek.indexRange(start, stop, indexes, function(msg) {
    callback(msg);
  });
};
SearchIndex.indexValue = function(key, callback) {
  if (!indexes) SearchIndex();

  indexPeek.indexValue(key, indexes, function(msg) {
    callback(msg);
  });
};
//do a full recalibration of the index
SearchIndex.calibrate = function(callback) {
  if (!indexes) SearchIndex();

  calibrater.calibrate(indexes, function(msg) {
    callback(msg);
  });
};


