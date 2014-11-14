exports.indexRange = function (start, stop, index, callback) {
  searchIndexLogger.info('peeking between ' + start + ' and ' + stop);
  var dump = [];
  index.createReadStream({
    start:start,
    end:stop})
    .on('data', function(data) {
      dump.push(data);
    })
    .on('error', function(err) {
      callback(err);
    })
    .on('end', function() {
      callback(dump);
    })
    .on('close', function() {
      callback(dump);
    });
}

exports.indexValue = function (key, index, callback) {
  index.get(key, function (err, value) {
    if (err) {
      if (err.notFound)
        return callback(true, false)
    }
    return callback(false, value);
  });
}
