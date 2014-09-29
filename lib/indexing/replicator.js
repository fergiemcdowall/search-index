var level = require('level'),
JSONStream = require('JSONStream'),
fs = require('fs'),
zlib = require('zlib'),
inflater = zlib.createGunzip(),
gzip = zlib.createGzip();


exports.replicateFromSnapShot = function(readStream, indexes, callback) {
  var err = false;
  readStream.pipe(inflater)
    .pipe(JSONStream.parse())
    .pipe(indexes.createWriteStream())
    .on('close', function() {
      callback(err);
    });
}


exports.createSnapShot = function(indexes, callback) {
  callback(indexes.createReadStream()
    .pipe(JSONStream.stringify('','\n',''))
    .pipe(gzip));
}

