var leveljs = require('level-js');
var searchIndex = require('./search-index');

module.exports = function(options) {
  options = options || {}
  // use a longer default name because we're sharing namespace in indexedDB.
  var indexPath = options.indexPath || 'search-index';
  options.db = leveljs(indexPath);
  return searchIndex(options);
}
