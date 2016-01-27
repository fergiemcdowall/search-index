module.exports = function(indexer, siOptions) {
  var siUtil = {}
  var _ = require('lodash')

  var processBatchOptions = function (batchOptions) {
    var defaultFieldOptions = {
      filter: false,
      nGramLength: siOptions.nGramLength,
      searchable: true,
      weight: 0,
      fieldedSearch: siOptions.fieldedSearch
    }
    var defaultBatchOptions = {
      batchName: 'Batch at ' + new Date().toISOString(),
      fieldOptions: siOptions.fieldOptions || defaultFieldOptions,
      fieldsToStore: siOptions.fieldsToStore,
      defaultFieldOptions: defaultFieldOptions
    }
    batchOptions = _.defaults(batchOptions || {}, defaultBatchOptions)
    batchOptions.filters = _.pluck(_.filter(batchOptions.fieldOptions, 'filter'), 'fieldName')
    if (_.find(batchOptions.fieldOptions, 'fieldName', '*') === -1) {
      batchOptions.fieldOptions.push(defaultFieldOptions('*'))
    }
    return batchOptions
  }


  siUtil.tellMeAboutMySearchIndex = function (callback) {
    siOptions.indexes.get('DOCUMENT-COUNT', function (err, value) {
      var td = value || 0
      var obj = {}
      obj.totalDocs = td
      obj.options = siOptions
      delete obj.options.log // causes woe for norch if not deleted
      callback(err, obj)
    })
  }

  siUtil.close = function (callback) {
    siOptions.indexes.close(function () {
      while (siOptions.indexes.isOpen()) {
        log.info('closing...')
      }
      callback(null)
    })
  }

  siUtil.add = function (batch, batchOptions, callback) {
    if (arguments.length === 2 && _.isFunction(arguments[1])) {
      callback = batchOptions
      batchOptions = undefined
    }
    indexer.addBatchToIndex(batch,
                            processBatchOptions(batchOptions),
                            callback)
  }

  return siUtil
}
