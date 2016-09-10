const _defaults = require('lodash.defaults')
const async = require('async')
const bunyan = require('bunyan')
const levelup = require('levelup')
const sw = require('stopword')

module.exports = function (givenOptions, callbacky) {
  var SearchIndex = {}
  getOptions(givenOptions, function (err, options) {
    if (err) return callbacky(err)

    SearchIndex.options = options

    async.series([
      function (callback) {
        require('search-index-adder')(SearchIndex.options, callback)
      },
      function (callback) {
        require('search-index-getter')(SearchIndex.options, callback)
      },
      function (callback) {
        require('search-index-deleter')(SearchIndex.options, callback)
      },
      function (callback) {
        require('search-index-replicator')(SearchIndex.options, callback)
      },
      function (callback) {
        require('search-index-searcher')(SearchIndex.options, callback)
      }
    ], function (err, results) {
      if (err) return callbacky(err)

      const searchIndexAdder = results[0]
      const searchIndexGetter = results[1]
      const searchIndexDeleter = results[2]
      const searchIndexReplicator = results[3]
      const searchIndexSearcher = results[4]
      const siUtil = require('./siUtil.js')(SearchIndex.options)

      // API
      SearchIndex.add = searchIndexAdder.add
      SearchIndex.buckets = searchIndexSearcher.bucketStream
      SearchIndex.categorize = searchIndexSearcher.categoryStream
      SearchIndex.close = siUtil.close
      SearchIndex.createWriteStream = searchIndexAdder.createWriteStream
      SearchIndex.DBReadStream = searchIndexReplicator.DBReadStream
      SearchIndex.DBWriteStream = searchIndexReplicator.DBWriteStream
      SearchIndex.del = searchIndexAdder.deleter
      SearchIndex.defaultPipeline = searchIndexAdder.defaultPipeline
      SearchIndex.flush = searchIndexDeleter.flush
      SearchIndex.get = searchIndexGetter.getDoc
      SearchIndex.match = searchIndexSearcher.match
      SearchIndex.search = searchIndexSearcher.search
      SearchIndex.scan = searchIndexSearcher.scan
      SearchIndex.tellMeAboutMySearchIndex = siUtil.tellMeAboutMySearchIndex

      SearchIndex.log = SearchIndex.options.log
      return callbacky(err, SearchIndex)
    })
  })
}

const getOptions = function (options, done) {
  options = _defaults(options, {
    deletable: true,
    fieldedSearch: true,
    store: true,
    indexPath: 'si',
    log: bunyan.createLogger({
      name: 'search-index',
      level: this.logLevel
    }),
    logLevel: 'error',
    nGramLength: 1,
    nGramSeparator: ' ',
    separator: /[\|' \.,\-|(\n)]+/,
    stopwords: sw.en
  })
  if (!options.indexes) {
    levelup(options.indexPath || 'si', {
      valueEncoding: 'json'
    }, function (err, db) {
      options.indexes = db
      done(err, options)
    })
  } else {
    done(null, options)
  }
}
