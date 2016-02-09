module.exports = function(indexer, siOptions) {
  var siUtil = {}
  var _ = require('lodash')

  siUtil.tellMeAboutMySearchIndex = function (callback) {
    siOptions.indexes.get('DOCUMENT-COUNT', function (err, value) {
      var td = value || 0
      var obj = {}
      obj.totalDocs = td
      obj.options = siOptions
      delete obj.options.log // causes woe for norch if not deleted
      callback(null, obj)
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
                            batchOptions,
                            callback)
  }

  return siUtil
}
