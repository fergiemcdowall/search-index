var _ = require('lodash')
var bunyan = require('bunyan')
var level = require('levelup')
var tv = require('term-vector')
var log




module.exports = function (options) {
  var SearchIndex = {}
  SearchIndex.options = getOptions(options)
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


  var docGetter = require('search-index-getter')(SearchIndex.options)
  var deleter = require('search-index-deleter')(SearchIndex.options)
  var indexer = require('search-index-adder')(SearchIndex.options)
  var matcher = require('search-index-matcher')(SearchIndex.options)
  var replicator = require('search-index-replicator')(SearchIndex.options)
  var searcher = require('search-index-searcher')(SearchIndex.options)


//experimental API calls
  SearchIndex.replicateBatch = function (serializedDB, callback) {
    replicator.replicateFromSnapShotBatch(serializedDB, SearchIndex.options.indexes, callback)
  }
  SearchIndex.snapShotBatch = function (callback) {
    replicator.createSnapShotBatch(SearchIndex.options.indexes, callback)
  }

  var tellMeAboutMySearchIndex = function (callback) {
    SearchIndex.options.indexes.get('DOCUMENT-COUNT', function (err, value) {
      var td = value || 0
      var obj = {}
      obj.totalDocs = td
      obj.options = SearchIndex.options
      delete obj.options.log // causes woe for norch if not deleted
      callback(err, obj)
    })
  }

  var close = function (callback) {
    SearchIndex.options.indexes.close(function () {
      while (SearchIndex.options.indexes.isOpen()) {
        log.info('closing...')
      }
      callback(null)
    })
  }

  var add = function (batch, batchOptions, callback) {
    if (arguments.length === 2 && _.isFunction(arguments[1])) {
      callback = batchOptions
      batchOptions = undefined
    }
    indexer.addBatchToIndex(batch,
                            processBatchOptions(batchOptions),
                            callback)
  }

  SearchIndex.add = add
  SearchIndex.close = close
  SearchIndex.del = deleter.deleteBatch
  SearchIndex.empty = deleter.flush
  SearchIndex.get = docGetter.getDoc
  SearchIndex.match = matcher.matcher
  SearchIndex.replicate = replicator.replicateFromSnapShotStream
  SearchIndex.search = searcher.search;
  SearchIndex.snapShot = replicator.createSnapShot
  SearchIndex.tellMeAboutMySearchIndex = tellMeAboutMySearchIndex

  SearchIndex.log = SearchIndex.options.log

  return SearchIndex
}


var getOptions = function(options) {
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
