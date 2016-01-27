module.exports = function (options) {
  var SearchIndex = {}
  SearchIndex.options = getOptions(options)

  const docGetter = require('search-index-getter')(SearchIndex.options)
  const deleter = require('search-index-deleter')(SearchIndex.options)
  const indexer = require('search-index-adder')(SearchIndex.options)
  const matcher = require('search-index-matcher')(SearchIndex.options)
  const replicator = require('search-index-replicator')(SearchIndex.options)
  const searcher = require('search-index-searcher')(SearchIndex.options)
  const siUtil = require('./siUtil.js')(indexer, SearchIndex.options)

  //API
  SearchIndex.add = siUtil.add
  SearchIndex.close = siUtil.close
  SearchIndex.del = deleter.deleteBatch
  SearchIndex.flush = deleter.flush
  SearchIndex.get = docGetter.getDoc
  SearchIndex.match = matcher.matcher
  SearchIndex.replicate = replicator.replicateFromSnapShotStream
  SearchIndex.search = searcher.search;
  SearchIndex.snapShot = replicator.createSnapShot
  SearchIndex.tellMeAboutMySearchIndex = siUtil.tellMeAboutMySearchIndex

  //experimental API
  SearchIndex.replicateBatch = function (serializedDB, callback) {
    replicator.replicateFromSnapShotBatch(serializedDB, SearchIndex.options.indexes, callback)
  }
  SearchIndex.snapShotBatch = function (callback) {
    replicator.createSnapShotBatch(SearchIndex.options.indexes, callback)
  }

  SearchIndex.log = SearchIndex.options.log
  return SearchIndex
}


var getOptions = function(options) {
  const _ = require('lodash')
  const bunyan = require('bunyan')
  const level = require('levelup')
  const tv = require('term-vector')

  var newOptions = {}
  var defaults = {
    deletable: true,
    fieldedSearch: true,
    fieldsToStore: 'all',
    indexPath: 'si',
    logLevel: 'error',
    nGramLength: 1,
    separator: /[\|' \.,\-|(\n)]+/,
    stopwords: tv.getStopwords('en').sort(),
  }
  // initialize defaults options
  newOptions = _.clone(_.defaults(options || {}, defaults))
  newOptions.log = options.log || bunyan.createLogger({
    name: 'search-index',
    level: newOptions.logLevel
  })
  newOptions.indexes = level(newOptions.indexPath, {
    valueEncoding: 'json',
    db: newOptions.db
  })
  return newOptions;
}
