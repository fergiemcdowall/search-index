var JSONStream = require('JSONStream');
var zlib = require('zlib');
var level = require('levelup');
var searchIndexLogger = require('../logger/searchIndexLogger');

var gzip = zlib.createGzip();
var inflater = zlib.createGunzip();


exports.replicateFromSnapShot = function (readStream, indexes, callback) {
  readStream.pipe(inflater)
    .pipe(JSONStream.parse())
    .pipe(indexes.createWriteStream())
    .on('close', callback);
}


exports.createSnapShot = function (indexes, callback) {
//has to be like this in order for norch snapshotting to work
  callback
  (indexes.createReadStream()
   .pipe(JSONStream.stringify('','\n',''))
   .pipe(gzip)
  );
}
