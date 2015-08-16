var skeleton = require('log-skeleton');
var H = require('highland');

module.exports = function (options) {
  var log = skeleton((options) ? options.log : undefined);
  var matcher = {};
  var threshold = 3; //only give matches for strings longer or equal to than this number

  matcher.matcher = function (reverseIndex, beginsWith, callback) {
    //sggestion string is too short
    if (beginsWith.length < threshold)
      return callback(new Error('string below threshold length (' +
                                threshold + ')'), []);
    H(reverseIndex.createReadStream({
      start: 'TF~*~' + beginsWith,
      end: 'TF~*~' + beginsWith + '􏿿'
    }))
    .on('error', function (err) {
      log.error('Oh my!', err);
    })
    .filter(function(data){
      return data.key.substring(data.key.length, data.key.length - 2) == '~~';
    })
    .map(function(data){
      return [data.key.split('~')[2], data.value.length]; //suggestions
    })
    .sortBy(function(a, b){
      return b[1] - a[1]; //sortedSuggestions
    })
    .map(function(data){
      return data[0]; //simpleSortedSuggestions
    })
    .take(10)
    .collect()
    .pull(callback);
  };

  return matcher;
};
