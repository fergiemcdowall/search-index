module.exports = function(siOptions) {


  var siUtil = {}
  var _ = require('lodash')

  siUtil.tellMeAboutMySearchIndex = function (callback) {
    siOptions.indexes.get('DOCUMENT-COUNT', function (err, docCount) {
      siOptions.indexes.get('LAST-UPDATE-TIMESTAMP', function (err, lastUpdate) {
        var obj = {}
        obj.totalDocs = docCount || 0
        obj.lastUpdate = lastUpdate || 0
        obj.options = siOptions
        delete obj.options.log // causes woe for norch if not deleted
        callback(null, obj)
      })
    })
  }

  siUtil.close = function (callback) {
    siOptions.indexes.close(function (err) {
      while (!siOptions.indexes.isClosed()) {
        //log not always working here- investigate
//        siOptions.log.info('closing...')
      }
      if (siOptions.indexes.isClosed()) {
//        siOptions.log.info('closed...')
        callback(err)
      }
    })
  }

  return siUtil
}
