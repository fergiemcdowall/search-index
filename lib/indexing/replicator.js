var level = require('level'),
JSONStream = require('JSONStream'),
fstream = require('fstream'),
fs = require('fs');

exports.replicateFromSnapShot = function(readStream, indexes, callback) {
    readStream.pipe(JSONStream.parse())
    .pipe(indexes.createWriteStream())
    .on('close', function() {
      callback('done');
    });
}


exports.createSnapShot = function(indexes, callback) {
  callback(indexes.createReadStream()
           .pipe(JSONStream.stringify('','','')));
}
