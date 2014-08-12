var level = require('level'),
fs = require('fs');
//newindexes = level('newsi', {valueEncoding: 'json'});

exports.createReadStream = function(indexes, callback) {
/*
  var rs = indexes.createReadStream();
  var ws = newindexes.createWriteStream();
  rs.pipe(ws).on('close', function(){
    callback('index piped');
  });
*/
  callback(indexes.createReadStream());
}
