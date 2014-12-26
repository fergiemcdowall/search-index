var leveljs = require('level-js');
var searchIndex = require('./lib/search-index');

// hackity hack hack
winston = false;
searchIndexLogger = console;

module.exports = function(options) {
  options = options || {}
  // use a longer default name because we're sharing namespace in indexedDB.
  options.indexPath = options.indexPath || 'search-index';
  options.db = leveljs;
  return searchIndex(options);
}
