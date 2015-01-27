var that;

var SearchIndex = module.exports = function (options) {

  that = this;

  this.level = require('levelup');
  this.levelMultiply = require('level-multiply');
  this.indexPeek = require('./indexing/indexPeek.js');
  this.matcher = require('./matchers/matcher.js');
  this.calibrater = require('./indexing/calibrater.js');
  this.indexer = require('./indexing/indexer.js');
  this.deleter = require('./indexing/deleter.js');
  this.replicator = require('./indexing/replicator.js');
  this.searcher = require('./search/searcher.js');
  this.docGetter = require('./search/docGetter.js');
  this.rmdir = require('rimraf');
  _ = require('lodash');
  this.searchIndexLogger = require('./logger/searchIndexLogger');
  this.winston = require('winston');

  this.indexes = null
  this.indexesMultiply = null;

  this.defaults = {
    indexPath: 'si',
    logLevel: 'info',
    logSilent: false
  };




  //initialize defaults options
  options = _.defaults(options || {}, this.defaults);

  //for further use
  this.options = options;

  this.indexes = (options && options.db)
    ? this.level(this.options.indexPath, {valueEncoding: 'json', db: options.db})
    : this.level(this.options.indexPath, {valueEncoding: 'json'});

  this.indexesMultiply = this.levelMultiply(this.indexes);

  this.searchIndexLogger.remove(this.winston.transports.Console);
  this.searchIndexLogger.add(this.winston.transports.Console, {silent: this.options.logSilent, level: this.options.logLevel});

  this.calibrater.getTotalDocs(this.indexes, function(err, totalDocs) {
    that.searcher.setTotalDocs(totalDocs);
  });

  return SearchIndex;
};

SearchIndex.del = function(docID, callback) {
  if (!that) SearchIndex();
  that.deleter.deleteDoc(docID, that.indexes, that.indexesMultiply, function(err) {
    callback(err);
  });
};


SearchIndex.get = function(docID, callback) {
  if (!that) SearchIndex();
  that.docGetter.getDoc(indexes, docID, function(err, doc) {
    callback(err, doc);
  });
};


SearchIndex.add = function(options, batch, callback) {
  if (!that) SearchIndex();
  that.indexer.addDocToIndex(that.indexes,
                             that.indexesMultiply,
                             batch,
                             options.batchName,
                             options.filters,
                             function(err) {
                               callback(err);
                             });
};

SearchIndex.empty = function(callback) {
  if (!that) SearchIndex();
  var err = false;
  that.indexes.close(function(){
    that.rmdir(that.options.indexPath, function(err){
      that.indexes = that.level(that.options.indexPath, {valueEncoding: 'json'});
      that.indexesMultiply = that.levelMultiply(that.indexes);
      callback(err);
    });
  });
};


SearchIndex.search = function (q, callback) {
  if (!that) SearchIndex();
  that.searcher.search(that.indexes, that.indexesMultiply, q, function(results){
    //TODO: make error throwing real
    callback(false, results);
  });
};


SearchIndex.match = function(beginsWith, callback) {
  if (!that) SearchIndex();
  that.matcher.matcher(that.indexes, beginsWith, function(err, match) {
    callback(err, match);
  });
};

SearchIndex.tellMeAboutMySearchIndex = function(callback) {
  if (!that) SearchIndex();
  that.calibrater.getTotalDocs(that.indexes, function(err, totalDocs) {
    var metadata = {};
    metadata['totalDocs'] = totalDocs;
    callback(metadata);
  });
};


SearchIndex.replicate = function(readStream, callback) {
  if (!that) SearchIndex();
  that.replicator.replicateFromSnapShot(readStream, that.indexes, function(err) {
    callback(err);
  });
};

SearchIndex.snapShot = function(callback) {
  if (!that) SearchIndex();

  that.replicator.createSnapShot(that.indexes, function(msg) {
    callback(msg);
  });
};


//utility methods for testing and devlopment
//******************************************
SearchIndex.indexRange = function(options, callback) {
  if (!that) SearchIndex();
  that.indexPeek.indexRange(options.start, options.stop, that.indexes, function(err, dump) {
    callback(err, dump);
  });
};
SearchIndex.indexValue = function(options, callback) {
  if (!that) SearchIndex();
  that.indexPeek.indexValue(options.key, that.indexes, function(err, value) {
    callback(err, value);
  });
};

//do a full recalibration of the index
SearchIndex.calibrate = function(callback) {
  if (!that) SearchIndex();
  that.calibrater.calibrate(that.indexes, function(err, msg) {
    callback(err, msg);
  });
};
