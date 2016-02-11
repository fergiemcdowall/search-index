module.exports = function (givenOptions, callback) {
  var SearchIndex = {}
  getOptions(givenOptions, function(err, options) {
    SearchIndex.options = options
    // console.log(SearchIndex.options)
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
    return callback(err, SearchIndex)
  })
}


var getOptions = function(options, callback) {
  const _ = require('lodash')
  const bunyan = require('bunyan')
  var levelup = require('levelup')
  var leveldown = require('leveldown')
  const tv = require('term-vector')
  options = options || {}
  levelup(options.indexPath || 'si', {
    valueEncoding: 'json'
  }, function (err, db) {
    var defaultOps = {}
    defaultOps.deletable = true
    defaultOps.fieldedSearch = true
    defaultOps.fieldsToStore = 'all'
    defaultOps.indexPath = 'si'
    defaultOps.logLevel = 'error'
    defaultOps.nGramLength = 1
    defaultOps.separator = /[\|' \.,\-|(\n)]+/
    defaultOps.stopwords = tv.getStopwords('en').sort()
    defaultOps.log = bunyan.createLogger({
      name: 'search-index',
      level: options.logLevel || defaultOps.logLevel
    })
    defaultOps.indexes = db
    return callback(err, _.defaults(options, defaultOps))
  })
}
