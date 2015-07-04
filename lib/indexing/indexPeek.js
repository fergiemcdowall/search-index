var options = null;

exports.setOptions = function (ops) {
  options = ops;
};

exports.indexRange = function (start, stop, index, callback) {
  options.log.info('peeking between ' + start + ' and ' + stop);
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

exports.indexValue = function (key, index, callback) {
  index.get(key, function (err, value) {
    if (err)
      return callback(err, null);
    else
      return callback(null, value);
  });
};
