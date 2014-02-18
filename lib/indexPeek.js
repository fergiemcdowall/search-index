exports.indexPeek = function (start, stop, reverseIndex, callback) {
  console.log('peeking between ' + start + ' and ' + stop);
  var dump = [];
  reverseIndex.createReadStream({
    start:start,
    end:stop})
    .on('data', function(data) {
      dump.push(data);
    })
    .on('end', function() {
//      console.log(dump);
      callback(dump);
    });
}
