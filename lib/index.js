const _ = require('lodash')
const async = require('async')
const bunyan = require('bunyan')
const levelup = require('levelup')
const tv = require('term-vector')

module.exports = function (givenOptions, callbacky) {
  var SearchIndex = {}
  getOptions(givenOptions, function(err, options) {
    SearchIndex.options = options

    async.series([
      function(callback) {
        require('search-index-adder')(SearchIndex.options, callback)
      },
      function(callback) {
        require('search-index-getter')(SearchIndex.options, callback)
      },
      function(callback) {
        require('search-index-deleter')(SearchIndex.options, callback)
      },
      function(callback) {
        require('search-index-matcher')(SearchIndex.options, callback)
      },
      function(callback) {
        require('search-index-replicator')(SearchIndex.options, callback)
      },
      function(callback) {
        require('search-index-searcher')(SearchIndex.options, callback)
      }     
    ], function(err, results){
      
      const searchIndexAdder = results[0]
      const searchIndexGetter = results[1]
      const searchIndexDeleter = results[2]
      const searchIndexMatcher = results[3]
      const searchIndexReplicator = results[4]
      const searchIndexSearcher = results[5]
      const siUtil = require('./siUtil.js')(SearchIndex.options)

      //API
      SearchIndex.add = searchIndexAdder.add
      SearchIndex.close = siUtil.close
      SearchIndex.del = searchIndexDeleter.deleteBatch
      SearchIndex.flush = searchIndexDeleter.flush
      SearchIndex.get = searchIndexGetter.getDoc
      SearchIndex.match = searchIndexMatcher.match
      SearchIndex.readStream = searchIndexReplicator.readStream
      SearchIndex.replicate = searchIndexReplicator.replicateFromSnapShotStream
      SearchIndex.search = searchIndexSearcher.search;
      SearchIndex.snapShot = searchIndexReplicator.createSnapShot
      SearchIndex.tellMeAboutMySearchIndex = siUtil.tellMeAboutMySearchIndex
      SearchIndex.writeStream = searchIndexReplicator.writeStream

      SearchIndex.log = SearchIndex.options.log
      return callbacky(err, SearchIndex)
    })
  })
}



var getOptions = function(givenOptions, callbacky) {
  givenOptions = givenOptions || {}
  async.parallel([
    function(callback) {
      var defaultOps = {}
      defaultOps.deletable = true
      defaultOps.fieldedSearch = true
      defaultOps.fieldsToStore = 'all'
      defaultOps.indexPath = 'si'
      defaultOps.logLevel = 'error'
      defaultOps.nGramLength = 1
      defaultOps.nGramSeparator = ' '
      defaultOps.separator = /[\|' \.,\-|(\n)]+/
      defaultOps.stopwords = tv.getStopwords('en').sort()
      defaultOps.log = bunyan.createLogger({
        name: 'search-index',
        level: givenOptions.logLevel || defaultOps.logLevel
      })
      callback(null, defaultOps)
    },
    function(callback){
      if (!givenOptions.indexes) {
        levelup(givenOptions.indexPath || 'si', {
          valueEncoding: 'json'
        }, function(err, db) {
          callback(null, db)          
        })
      }
      else {
        callback(null, null)
      }
    }
  ], function(err, results){
    var options = _.defaults(givenOptions, results[0])
    if (results[1] != null) {
      options.indexes = results[1]
    }
    return callbacky(err, options)
  })
}
