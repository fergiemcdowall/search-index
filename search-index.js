const _ = require('lodash')
const async = require('async')
const bunyan = require('bunyan')
const levelup = require('levelup')
const leveldown = require('leveldown')
const tv = require('term-vector')

module.exports = function (givenOptions, callbacky) {
  var SearchIndex = {}
  getOptions(givenOptions, function(err, options) {
    SearchIndex.options = options
    // console.log(SearchIndex.options)


    async.parallel([
      function(callback){
        require('search-index-adder')(SearchIndex.options, callback)
      }
    ], function(err, results){
      
      const searchIndexAdder = results[0]
      const searchIndexGetter = require('search-index-getter')(SearchIndex.options)
      const searchIndexDeleter = require('search-index-deleter')(SearchIndex.options)
      const searchIndexMatcher = require('search-index-matcher')(SearchIndex.options)
      const searchIndexReplicator = require('search-index-replicator')(SearchIndex.options)
      const searchIndexSearcher = require('search-index-searcher')(SearchIndex.options)
      const siUtil = require('./siUtil.js')(SearchIndex.options)

      //API
      SearchIndex.add = searchIndexAdder.add
      SearchIndex.close = siUtil.close
      SearchIndex.del = searchIndexDeleter.deleteBatch
      SearchIndex.flush = searchIndexDeleter.flush
      SearchIndex.get = searchIndexGetter.getDoc
      SearchIndex.match = searchIndexMatcher.matcher
      SearchIndex.replicate = searchIndexReplicator.replicateFromSnapShotStream
      SearchIndex.search = searchIndexSearcher.search;
      SearchIndex.snapShot = searchIndexReplicator.createSnapShot
      SearchIndex.tellMeAboutMySearchIndex = siUtil.tellMeAboutMySearchIndex

      //experimental API
      SearchIndex.replicateBatch = function (serializedDB, callback) {
        replicator.replicateFromSnapShotBatch(serializedDB, SearchIndex.options.indexes, callback)
      }
      SearchIndex.snapShotBatch = function (callback) {
        replicator.createSnapShotBatch(SearchIndex.options.indexes, callback)
      }

      SearchIndex.log = SearchIndex.options.log
      return callbacky(err, SearchIndex)


    });
  })
}


var getOptions = function(options, callback) {
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
