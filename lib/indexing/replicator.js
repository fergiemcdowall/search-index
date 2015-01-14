var level = require('levelup'),
    JSONStream = require('JSONStream'),
    zlib = require('zlib'),
    inflater = zlib.createGunzip(),
    gzip = zlib.createGzip();


exports.replicateFromSnapShot = function (readStream, indexes, callback) {
    readStream.pipe(inflater)
        .pipe(JSONStream.parse())
        .pipe(indexes.createWriteStream())
        .on('close', callback);
}


exports.createSnapShot = function (indexes, callback) {
    indexes.createReadStream()
        .on('finish', callback)
        .pipe(JSONStream.stringify('', '\n', ''))
        .pipe(gzip);
}
