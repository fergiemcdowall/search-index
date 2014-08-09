var level = require('level');
var threshold = 3; //only give matches for strings longer or equal to than this number

exports.matcher = function(reverseIndex, beginsWith, callback) {
  //sggestion string is too short
  var suggestions = [];
  if (beginsWith.length < threshold) {
    return callback(suggestions);
  }
  else {
    reverseIndex.createReadStream({
      start: 'TF~*~' + beginsWith,
      end: 'TF~*~' + beginsWith + '~~~'})
      .on('data', function(data) {
        if (data.key.substring(data.key.length, data.key.length - 2) == '~~')
          suggestions.push([data.key.split('~')[2], data.value.length]);
      })
      .on('error', function (err) {
        console.log('Oh my!', err)
      })
      .on('close', function() {
        var sortedSuggestions = suggestions.sort(function(a, b) {
          return b[1] - a[1]
        }).splice(0, 10);
        var simpleSortedSuggestions = [];
        for (var i = 0; i < sortedSuggestions.length; i++) {
          //        console.log(sortedSuggestions[i][0]);
          simpleSortedSuggestions.push(sortedSuggestions[i][0]);
        }
        callback(simpleSortedSuggestions);
      });
  }
}

