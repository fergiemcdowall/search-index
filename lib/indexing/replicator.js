var level = require('level'),
JSONStream = require('JSONStream'),
fstream = require('fstream'),
fs = require('fs'),
AdmZip = require('adm-zip')
zip = new AdmZip();

exports.replicateFromSnapShot = function(indexes, callback) {
  var zip = new AdmZip('backup.json.zip');
  zip.extractAllTo('./');
  fs.createReadStream('backup.json')
    .pipe(JSONStream.parse())
    .pipe(indexes.createWriteStream())
    .on('close', function() {
      callback('done');
    });
}


exports.createSnapShot = function(indexes, callback) {
  callback(indexes.createReadStream()
           .pipe(JSONStream.stringify('','','')));
}
