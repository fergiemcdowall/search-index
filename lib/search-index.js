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
  docGetter.getDoc(indexes, indexesMultiply, docID, function(err, doc) {
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
SearchIndex.indexRange = function(options, callback) {
  if (!indexes) SearchIndex();
  indexPeek.indexRange(options.start, options.stop, indexes, function(err) {
    callback(err);
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

  calibrater.calibrate(indexes, function(msg) {
    callback(msg);
  });
};


