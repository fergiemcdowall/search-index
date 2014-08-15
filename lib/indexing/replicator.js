var level = require('level'),
fs = require('fs');
//newindexes = level('newsi', {valueEncoding: 'json'});

exports.createSnapShot = function(indexes, callback) {
  callback(indexes.createReadStream());
}
