var _ = require('lodash')
var bunyan = require('bunyan')
var level = require('levelup')
var tv = require('term-vector')

module.exports = function (options) {
  var SearchIndex = {}
  var defaults = {
    deletable: true,
    fieldedSearch: true,
    fieldsToStore: 'all',
    indexPath: 'si',
    logLevel: 'error',
    nGramLength: 1,
    separator: /[\|' \.,\-|(\n)]+/,
    stopwords: tv.getStopwords('en').sort()
  }
  // initialize defaults options
  SearchIndex.options = _.clone(_.defaults(options || {}, defaults))
  var indexes = SearchIndex.indexes = level(SearchIndex.options.indexPath, {
    valueEncoding: 'json',
    db: SearchIndex.options.db
  })
  var log = SearchIndex.options.log || bunyan.createLogger({
    name: 'search-index',
    level: SearchIndex.options.logLevel
  })
  var docGetter = require('search-index-getter')({
    indexes: SearchIndex.indexes
  })
  var deleter = require('search-index-deleter')({
    log: log.child({component: 'deleter'})
  })
  var indexer = require('search-index-adder')(
    _.assign(SearchIndex.options, {log: log.child({component: 'indexer'})}))
  var matcher = require('search-index-matcher')({
    log: log.child({component: 'matcher'}),
    indexes: SearchIndex.indexes
  })
  var replicator = require('search-index-replicator')({
    log: log.child({component: 'replicator'}),
    indexes: SearchIndex.indexes
  })
  var searcher = require('search-index-searcher')({
    log: log.child({component: 'searcher'}),
    stopwords: SearchIndex.options.stopwords
  })
  var processBatchOptions = function (batchOptions) {
    var defaultFieldOptions = {
      filter: false,
      nGramLength: SearchIndex.options.nGramLength,
      searchable: true,
      weight: 0,
      fieldedSearch: SearchIndex.options.fieldedSearch
    }
    var defaultBatchOptions = {
      batchName: 'Batch at ' + new Date().toISOString(),
      fieldOptions: SearchIndex.options.fieldOptions || defaultFieldOptions,
      fieldsToStore: SearchIndex.options.fieldsToStore,
      defaultFieldOptions: defaultFieldOptions
    }
    batchOptions = _.defaults(batchOptions || {}, defaultBatchOptions)
    batchOptions.filters = _.pluck(_.filter(batchOptions.fieldOptions, 'filter'), 'fieldName')
    if (_.find(batchOptions.fieldOptions, 'fieldName', '*') === -1) {
      batchOptions.fieldOptions.push(defaultFieldOptions('*'))
    }
    return batchOptions
  }


  SearchIndex.add = function (batch, batchOptions, callback) {
    if (arguments.length === 2 && _.isFunction(arguments[1])) {
      callback = batchOptions
      batchOptions = undefined
    }
    indexer.addBatchToIndex(SearchIndex.indexes,
      batch,
      processBatchOptions(batchOptions),
      callback)
  }

  SearchIndex.close = function (callback) {
    SearchIndex.indexes.close(function () {
      while (SearchIndex.indexes.isOpen()) {
        log.info('closing...')
      }
      callback(null)
    })
  }

  SearchIndex.del = function (docID, callback) {
    if (SearchIndex.options.deletable) {
      if (!_.isArray(docID)) {
        docID = [docID]
      }
      deleter.deleteBatch(docID, SearchIndex.indexes, callback)
    } else {
      callback(new Error('this index is non-deleteable- set "deletable: true" in startup options'))
    }
  }

  SearchIndex.empty = function (callback) {
    var deleteOps = []
    SearchIndex.indexes.createKeyStream({gte: '0', lte: '￮'})
      .on('data', function (data) {
        deleteOps.push({type: 'del', key: data})
      })
      .on('error', function (err) {
        log.error(err, ' failed to empty index')
      })
      .on('end', function () {
        SearchIndex.indexes.batch(deleteOps, callback)
      })
  }

  SearchIndex.get = docGetter.getDoc
  SearchIndex.match = matcher.matcher
  SearchIndex.replicate = replicator.replicateFromSnapShotStream

  SearchIndex.replicateBatch = function (serializedDB, callback) {
    replicator.replicateFromSnapShotBatch(serializedDB, SearchIndex.indexes, callback)
  }

  SearchIndex.search = function (q, callback) {
    searcher.search(SearchIndex.indexes, q, function (results) {
      // TODO: make error throwing real
      callback(null, results)
    })
  }

  SearchIndex.snapShotBatch = function (callback) {
    replicator.createSnapShotBatch(SearchIndex.indexes, callback)
  }

  SearchIndex.snapShot = replicator.createSnapShot

  SearchIndex.tellMeAboutMySearchIndex = function (callback) {
    SearchIndex.indexes.get('DOCUMENT-COUNT', function (err, value) {
      var td = value || 0
      var obj = {}
      obj.totalDocs = td
      obj.options = SearchIndex.options
      delete obj.options.log // causes woe for norch if not deleted
      callback(err, obj)
    })
  }

  SearchIndex.log = log
  return SearchIndex
}
