var JSONStream = require('JSONStream');
var zlib = require('zlib');
var level = require('levelup');

var gzip = zlib.createGzip();
var inflater = zlib.createGunzip();


//ReplicateFromStream
exports.replicateFromSnapShotStream = function (readStream, indexes, callback) {
  readStream.pipe(inflater)
    .pipe(JSONStream.parse())
    .pipe(indexes.createWriteStream())
    .on('close', callback);
}

//ReplicateFromBatch
exports.replicateFromSnapShotBatch = function(serializedDB, indexes, callback) {
  for (var i = 0; i < serializedDB.length; i++)
    serializedDB[i]["type"] = "put"
  indexes.batch(serializedDB, function (err) {
    if (err) return console.log('Ooops!', err)
    console.log('Great success dear leader!')
  })
};

//createSnapShotForStream
exports.createSnapShot = function (indexes, callback) {
//has to be like this in order for norch snapshotting to work
  callback
  (indexes.createReadStream()
   .pipe(JSONStream.stringify('','\n',''))
   .pipe(gzip)
  );
}

//createSnapShotForBatch
exports.createSnapShotBatch = function (indexes, callback) {
//has to be like this in order for norch snapshotting to work
  callback
  (indexes.createReadStream()
   .pipe(JSONStream.stringify('[',',',']'))
   .pipe(gzip)
  );
}
