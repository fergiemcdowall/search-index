var level = require('level'),
JSONStream = require('JSONStream'),
fstream = require('fstream'),
fs = require('fs');
//newindexes = level('newsi', {valueEncoding: 'json'});


exports.replicateFromSnapShot = function(indexes, callback) {
  fs.createReadStream('backup.json')
    .pipe(JSONStream.parse())
    .pipe(indexes.createWriteStream())
    .on('close', function() {
      callback('done');
    });
}


exports.createSnapShot = function(indexes, callback) {
  indexes.createReadStream()
    .pipe(JSONStream.stringify('','',''))
    .pipe(fs.createWriteStream('backup.json'))
    .on('close', function() {
      callback('done');
    });
}
