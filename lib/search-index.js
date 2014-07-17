var fs = require('fs'),
level = require('level'),
levelMultiply = require('level-multiply'),
indexPeek = require('./indexing/indexPeek.js'),
matcher = require('./matchers/matcher.js'),
calibrater = require('./indexing/calibrater.js'),
indexer = require('./indexing/indexer.js'),
deleter = require('./indexing/deleter.js'),
searcher = require('./mapreduce/searcher.js'),
docGetter = require('./mapreduce/docGetter.js'),
indexes = level('si', {valueEncoding: 'json'}),
indexesMultiply = levelMultiply(indexes);
//docFreqIndex = level('df', {valueEncoding: 'json'});
//init functions
calibrater.getTotalDocs(indexes, function(totalDocs) {
  searcher.setTotalDocs(totalDocs);
});
//end init


exports.del = function(docID, callback) {
  deleter.deleteDoc(docID, indexes, indexesMultiply, function(msg) {
    callback(msg);
  });
};


exports.get = function(docID, callback) {
  docGetter.getDoc(indexes, indexesMultiply, docID, function(msg) {
    callback(msg);
  });
};


exports.add = function(batchString, batchName, filters, callback) {
  indexer.addDocToIndex(indexes, indexesMultiply, batchString, batchName, filters, function(msg) {
    callback(msg);
  });
};


exports.search = function (q, callback) {
  searcher.search(indexes, indexesMultiply, q, function(msg){
    callback(msg);
  });
};


exports.matcher = function(beginsWith, callback) {
  matcher.matcher(indexes, beginsWith, function(msg) {
    callback(msg);
  });
}

exports.getIndexMetadata = function(callback) {
  calibrater.getTotalDocs(indexes, function(totalDocs) {
    var metadata = {};
    metadata['totalDocs'] = totalDocs;
    callback(metadata);
  });
};


//utility methods for testing and devlopment
//******************************************
exports.indexRange = function(start, stop, callback) {
  indexPeek.indexRange(start, stop, indexes, function(msg) {
    callback(msg);
  });
};
exports.indexValue = function(key, callback) {
  indexPeek.indexValue(key, indexes, function(msg) {
    callback(msg);
  });
};
//do a full recalibration of the index
exports.calibrate = function(callback) {
  calibrater.calibrate(indexes, function(msg) {
    callback(msg);
  });
};


