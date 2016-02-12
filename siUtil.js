module.exports = function(siOptions) {


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
