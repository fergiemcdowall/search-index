

var SearchIndex = module.exports = function SearchIndex(options) {
  if (!(this instanceof SearchIndex))
    return new SearchIndex(options);
  //------------libs
  this.level = require('levelup');
  this.levelMultiply = require('level-multiply');
  this.rmdir = require('rimraf');
  this.winston = require('winston');
  _ = require('lodash');
  this.defaults = {
    indexPath: 'si',
    logLevel: 'info',
    logSilent: false
  };
  //initialize defaults options
  this.options = _.defaults(options || {}, this.defaults);
  this.calibrater = require('./indexing/calibrater.js');
  this.deleter = require('./indexing/deleter.js');
  this.docGetter = require('./search/docGetter.js');
  this.indexPeek = require('./indexing/indexPeek.js');
  this.indexer = require('./indexing/indexer.js');
  this.matcher = require('./matchers/matcher.js');
  this.replicator = require('./indexing/replicator.js');
  this.searcher = require('./search/searcher.js');
  this.searchIndexLogger = require('./logger/searchIndexLogger');
  this.indexes = (this.options && this.options.db)
    ? this.level(this.options.indexPath, {valueEncoding: 'json', db: this.options.db})
    : this.level(this.options.indexPath, {valueEncoding: 'json'});
  this.indexesMultiply = this.levelMultiply(this.indexes);
  this.searchIndexLogger.remove(this.winston.transports.Console);
  this.searchIndexLogger.add(this.winston.transports.Console, {silent: this.options.logSilent, level: this.options.logLevel});
  //is there a better way of doing this?
  var searcher = this.searcher;
  this.calibrater.getTotalDocs(this.indexes, function(err, totalDocs) {
    searcher.setTotalDocs(totalDocs);
  });
};


SearchIndex.prototype.getOptions = function() {
  return this.options;
}


SearchIndex.prototype.add = function(options, batch, callback) {
  var filters = options.filters || {};
  console.log(this.indexer.addDocToIndex);
  this.indexer.addDocToIndex(this.indexes,
                             this.indexesMultiply,
                             batch,
                             options.batchName,
                             filters,
                             function(err) {
                               callback(err);
                             });
};


SearchIndex.prototype.del = function(docID, callback) {
  this.deleter.deleteDoc(docID, this.indexes, this.indexesMultiply, function(err) {
    callback(err);
  });
};

SearchIndex.prototype.empty = function(callback) {
  var err = false;
  this.indexes.close(function(){
    this.rmdir(this.options.indexPath, function(err){
      this.indexes = this.level(this.options.indexPath, {valueEncoding: 'json'});
      this.indexesMultiply = this.levelMultiply(this.indexes);
      callback(err);
    });
  });
};

SearchIndex.prototype.get = function(docID, callback) {
  this.docGetter.getDoc(this.indexes, docID, function(err, doc) {
    callback(err, doc);
  });
};

SearchIndex.prototype.match = function(beginsWith, callback) {
  this.matcher.matcher(this.indexes, beginsWith, function(err, match) {
    callback(err, match);
  });
};


SearchIndex.prototype.replicate = function(readStream, callback) {
  this.replicator.replicateFromSnapShot(readStream, this.indexes, function(err) {
    callback(err);
  });
};


SearchIndex.prototype.search = function (q, callback) {
  this.searcher.search(this.indexes, q, function(results){
    //TODO: make error throwing real
    callback(false, results);
  });
};

SearchIndex.prototype.snapShot = function(callback) {
  this.replicator.createSnapShot(this.indexes, function(msg) {
    callback(msg);
  });
};

SearchIndex.prototype.tellMeAboutMySearchIndex = function(callback) {
  this.calibrater.getTotalDocs(this.indexes, function(err, totalDocs) {
    var metadata = {};
    metadata['totalDocs'] = totalDocs;
    callback(metadata);
  });
};


//utility methods for testing and devlopment
//******************************************
SearchIndex.prototype.indexRange = function(options, callback) {
  this.indexPeek.indexRange(options.start, options.stop, this.indexes, function(err, dump) {
    callback(err, dump);
  });
};

SearchIndex.prototype.indexValue = function(options, callback) {
  this.indexPeek.indexValue(options.key, this.indexes, function(err, value) {
    callback(err, value);
  });
};

//do a full recalibration of the index
SearchIndex.prototype.calibrate = function(callback) {
  this.calibrater.calibrate(this.indexes, function(err, msg) {
    callback(err, msg);
  });
};
