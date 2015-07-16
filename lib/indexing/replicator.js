var levelws = require('level-ws');
var JSONStream = require('JSONStream');
var skeleton = require('log-skeleton');
var zlib = require('zlib');
var gzip = zlib.createGzip();
var inflater = zlib.createGunzip();

module.exports = function (options) {
  var log = skeleton((options) ? options.log : undefined);
  var replicator = {};

  //ReplicateFromStream
  replicator.replicateFromSnapShotStream = function (readStream, indexes, callback) {
    var indexesws = levelws(indexes);
    readStream.pipe(inflater)
      .pipe(JSONStream.parse())
      .pipe(indexesws.createWriteStream())
      .on('close', callback);
  };

  //ReplicateFromBatch
  replicator.replicateFromSnapShotBatch = function (serializedDB, indexes, callback) {
    for (var i = 0; i < serializedDB.length; i++)
      serializedDB.i.type = 'put';
    indexes.batch(serializedDB, function (err) {
      if (err) return log.warn('Ooops!', err);
      log.info('Great success dear leader!');
      callback(err);
    });
  };

  //createSnapShotForStream
  replicator.createSnapShot = function (indexes, callback) {
    //has to be like this in order for norch snapshotting to work
    callback
    (indexes.createReadStream()
     .pipe(JSONStream.stringify('', '\n', ''))
     .pipe(gzip)
    );
  };

  //createSnapShotForBatch
  replicator.createSnapShotBatch = function (indexes, callback) {
    //has to be like this in order for norch snapshotting to work
    callback
    (indexes.createReadStream()
     .pipe(JSONStream.stringify('[', ',', ']'))
     .pipe(gzip)
    );
  };
  return replicator;
};
