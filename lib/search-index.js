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

exports.indexData = function(callback) {
  calibrater.getTotalDocs(indexes, function(totalDocs) {
    var metadata = {};
    metadata['totalDocs'] = totalDocs;
    callback(metadata);
  });
};


exports.indexPeek = function(start, stop, callback) {
  indexPeek.indexPeek(start, stop, indexes, function(msg) {
    callback(msg);
  });
};


exports.generateMatcher = function(callback) {
  matcher.generateMatcher(indexes, function(msg) {
    callback(msg);
  });
}


exports.matcher = function(beginsWith, callback) {
  matcher.matcher(beginsWith, function(msg) {
    callback(msg);
  });
}


exports.calibrate = function(callback) {
  calibrater.calibrate(indexes, function(msg) {
    callback(msg);
  });
};


exports.deleteDoc = function(docID, callback) {
  deleter.deleteDoc(docID, indexes, indexesMultiply, function(msg) {
    callback(msg);
  });
};


exports.getDoc = function(docID, callback) {
  docGetter.getDoc(indexes, indexesMultiply, docID, function(msg) {
    callback(msg);
  });
};


exports.addDoc = function(batchString, batchName, filters, callback) {
  indexer.addDocToIndex(indexes, indexesMultiply, batchString, batchName, filters, function(msg) {
    callback(msg);
  });
};


exports.search = function (q, callback) {
  searcher.search(indexes, indexesMultiply, q, function(msg){
    callback(msg);
  });
};

