var shadower = require('bunyan-shadower');

module.exports = function (options) {
  var log = shadower((options) ? options.log : undefined);
  var indexPeek = {};
  indexPeek.setLogLevel = function (level) {
    log.level(level);
  };

  indexPeek.indexRange = function (start, stop, index, callback) {
    log.info('peeking between ' + start + ' and ' + stop);
    var dump = [];
    index.createReadStream({
      start:start,
      end:stop})
      .on('data', function (data) {
        dump.push(data);
      })
      .on('error', function (err) {
        callback(err);
      })
      .on('end', function () {
        callback(null, dump);
      })
      .on('close', function () {
        callback(null, dump);
      });
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
