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

exports.configure = function (options) {
  options = options || defaults;

  indexes = level(options.indexPath, {valueEncoding: 'json'}),
  indexesMultiply = levelMultiply(indexes);

  calibrater.getTotalDocs(indexes, function(totalDocs) {
    searcher.setTotalDocs(totalDocs);
  });
};

exports.del = function(docID, callback) {
  if (!indexes) exports.configure();

  deleter.deleteDoc(docID, indexes, indexesMultiply, function(msg) {
    callback(msg);
  });
};


exports.get = function(docID, callback) {
  if (!indexes) exports.configure();

  docGetter.getDoc(indexes, indexesMultiply, docID, function(msg) {
    callback(msg);
  });
};


exports.add = function(batchString, batchName, filters, callback) {
  if (!indexes) exports.configure();

  indexer.addDocToIndex(indexes, indexesMultiply, batchString, batchName, filters, function(msg) {
    callback(msg);
  });
};

exports.empty = function( callback) {
  if (!indexes) exports.configure();

  indexes.close(function(){
    rmdir('si', function(error){
      indexes = level('si', {valueEncoding: 'json'});
      indexesMultiply = levelMultiply(indexes);
      callback('emptied');
    });
  });
};


exports.search = function (q, callback) {
  if (!indexes) exports.configure();

  searcher.search(indexes, indexesMultiply, q, function(msg){
    callback(msg);
  });
};


exports.match = function(beginsWith, callback) {
  if (!indexes) exports.configure();

  matcher.matcher(indexes, beginsWith, function(msg) {
    callback(msg);
  });
};

exports.tellMeAboutMySearchIndex = function(callback) {
  if (!indexes) exports.configure();

  calibrater.getTotalDocs(indexes, function(totalDocs) {
    var metadata = {};
    metadata['totalDocs'] = totalDocs;
    callback(metadata);
  });
};


exports.replicate = function(readStream, callback) {
  if (!indexes) exports.configure();

  replicator.replicateFromSnapShot(readStream, indexes, function(msg) {
    callback(msg);
  });
};

exports.snapShot = function(callback) {
  if (!indexes) exports.configure();

  replicator.createSnapShot(indexes, function(msg) {
    callback(msg);
  });
};


//utility methods for testing and devlopment
//******************************************
exports.indexRange = function(start, stop, callback) {
  if (!indexes) exports.configure();

  indexPeek.indexRange(start, stop, indexes, function(msg) {
    callback(msg);
  });
};
exports.indexValue = function(key, callback) {
  if (!indexes) exports.configure();

  indexPeek.indexValue(key, indexes, function(msg) {
    callback(msg);
  });
};
//do a full recalibration of the index
exports.calibrate = function(callback) {
  if (!indexes) exports.configure();

  calibrater.calibrate(indexes, function(msg) {
    callback(msg);
  });
};


