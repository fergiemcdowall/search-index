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
reverseIndex = level('si', {valueEncoding: 'json'}),
reverseIndexMultiply = levelMultiply(reverseIndex);
docFreqIndex = level('df', {valueEncoding: 'json'});

//init functions
calibrater.getTotalDocs(docFreqIndex, function(totalDocs) {
  searcher.setTotalDocs(totalDocs);
});
//end init

exports.indexData = function(callback) {
  calibrater.getTotalDocs(docFreqIndex, function(totalDocs) {
    var metadata = {};
    metadata['totalDocs'] = totalDocs;
    callback(metadata);
  });
};


exports.indexPeek = function(start, stop, callback) {
  indexPeek.indexPeek(start, stop, reverseIndex, function(msg) {
    callback(msg);
  });
};


exports.generateMatcher = function(callback) {
  matcher.generateMatcher(reverseIndex, function(msg) {
    callback(msg);
  });
}


exports.matcher = function(beginsWith, callback) {
  matcher.matcher(beginsWith, function(msg) {
    callback(msg);
  });
}


exports.calibrate = function(callback) {
  calibrater.calibrate(reverseIndex, docFreqIndex, function(msg) {
    callback(msg);
  });
};


exports.deleteDoc = function(docID, callback) {
  deleter.deleteDoc(docID, reverseIndex, reverseIndexMultiply, function(msg) {
    callback(msg);
  });
};


exports.getDoc = function(docID, callback) {
  docGetter.getDoc(reverseIndex, reverseIndexMultiply, docID, function(msg) {
    callback(msg);
  });
};


exports.index = function(batchString, batchName, filters, callback) {
  indexer.index(reverseIndex, batchString, batchName, filters, function(msg) {
    callback(msg);
  });
};


exports.search = function (q, callback) {
  searcher.search(reverseIndex, reverseIndexMultiply, docFreqIndex, q, function(msg){
    callback(msg);
  });
};

