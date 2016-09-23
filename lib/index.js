const _defaults = require('lodash.defaults')
const bunyan = require('bunyan')
const levelup = require('levelup')
const SearchIndexAdder = require('search-index-adder')
const SearchIndexSearcher = require('search-index-searcher')

module.exports = function (givenOptions, moduleReady) {
  const optionsLoaded = function (err, SearchIndex) {
    const siUtil = require('./siUtil.js')(SearchIndex.options)
    if (err) return moduleReady(err)
    SearchIndex.tellMeAboutMySearchIndex = siUtil.tellMeAboutMySearchIndex
    SearchIndex.close = siUtil.close
    getAdder(SearchIndex, adderLoaded)
  }

  const adderLoaded = function (err, SearchIndex) {
    if (err) return moduleReady(err)
    getSearcher(SearchIndex, searcherLoaded)
  }

  const searcherLoaded = function (err, SearchIndex) {
    if (err) return moduleReady(err)
    return moduleReady(err, SearchIndex)
  }

  getOptions(givenOptions, optionsLoaded)
}

const getAdder = function (SearchIndex, done) {
  SearchIndexAdder(SearchIndex.options, function (err, searchIndexAdder) {
    SearchIndex.add = searchIndexAdder.add
    SearchIndex.createWriteStream = searchIndexAdder.createWriteStream
    SearchIndex.dbWriteStream = searchIndexAdder.dbWriteStream
    SearchIndex.defaultPipeline = searchIndexAdder.defaultPipeline
    SearchIndex.del = searchIndexAdder.deleter
    SearchIndex.flush = searchIndexAdder.flush
    done(err, SearchIndex)
  })
}

const getSearcher = function (SearchIndex, done) {
  SearchIndexSearcher(SearchIndex.options, function (err, searchIndexSearcher) {
    SearchIndex.buckets = searchIndexSearcher.bucketStream
    SearchIndex.categorize = searchIndexSearcher.categoryStream
    SearchIndex.dbReadStream = searchIndexSearcher.dbReadStream
    SearchIndex.get = searchIndexSearcher.get
    SearchIndex.match = searchIndexSearcher.match
    SearchIndex.scan = searchIndexSearcher.scan
    SearchIndex.search = searchIndexSearcher.search
    done(err, SearchIndex)
  })
}

const getOptions = function (options, done) {
  var SearchIndex = {}
  SearchIndex.options = _defaults(options, {
    indexPath: 'si',
    logLevel: 'error'
  })
  options.log = bunyan.createLogger({
    name: 'search-index',
    level: options.logLevel
  })
  if (!options.indexes) {
    levelup(SearchIndex.options.indexPath || 'si', {
      valueEncoding: 'json'
    }, function (err, db) {
      SearchIndex.options.indexes = db
      return done(err, SearchIndex)
    })
  } else {
    return done(null, SearchIndex)
  }
}
