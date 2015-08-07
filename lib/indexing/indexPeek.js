var skeleton = require('log-skeleton');
var H = require('highland');

module.exports = function (options) {

  var log = skeleton((options) ? options.log : undefined);
  var indexPeek = {};

  indexPeek.indexRange = function (start, stop, index, callback) {
    log.info('peeking between ' + start + ' and ' + stop);

    H(index.createReadStream({
      start:start,
      end:stop
    }))
    .collect()
    .pull(callback);
  };

  indexPeek.indexValue = function (key, index, callback) {
    index.get(key, function (err, value) {
      if (err)
        return callback(err, null);
      else
        return callback(null, value);
    });
  };

  return indexPeek;
};
