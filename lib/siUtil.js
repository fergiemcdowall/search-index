module.exports = function(siOptions) {
  var siUtil = {}

  siUtil.tellMeAboutMySearchIndex = function (callback) {
    siOptions.indexes.get('DOCUMENT-COUNT', function (err, docCount) {
      siOptions.indexes.get('LAST-UPDATE-TIMESTAMP', function (err, lastUpdate) {
        var obj = {}
        obj.totalDocs = docCount || 0
        obj.lastUpdate = lastUpdate || 0

        obj.indexPath = siOptions.indexPath
        obj.deletable = siOptions.deletable
        obj.fieldedSearch = siOptions.fieldedSearch
        obj.store = siOptions.store 
        obj.logLevel = siOptions.logLevel
        obj.nGramLength = siOptions.nGramLength
        obj.nGramSeparator = siOptions.nGramSeparator
        obj.separator = siOptions.separator
        obj.stopwords = siOptions.stopwords

        callback(null, obj)
      })
    })
  }

  siUtil.close = function (callback) {
    siOptions.indexes.close(function (err) {
      while (!siOptions.indexes.isClosed()) {
        //log not always working here- investigate
       siOptions.log.info('closing...')
      }
      if (siOptions.indexes.isClosed()) {
       siOptions.log.info('closed...')
        callback(err)
      }
    })
  }

  return siUtil
}
