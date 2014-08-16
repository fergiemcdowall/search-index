var level = require('level'),
fs = require('fs');
//newindexes = level('newsi', {valueEncoding: 'json'});

exports.createSnapShot = function(indexes, callback) {
  callback(indexes.createReadStream());
}

exports.replicateFromSnapShot = function(readStream, indexes, callback) {
  callback('replicated');
}
