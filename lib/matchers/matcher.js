var level = require('level');

exports.matcher = function(reverseIndex, beginsWith, callback) {
  var suggestions = [];
  reverseIndex.createReadStream({
    start: 'TF~' + beginsWith,
    end: 'TF~' + beginsWith + '~~~~'})
    .on('data', function(data) {
//      console.log(data);
      if ((data.key.split('~')[4] == '*') && (data.key.split('~')[3] == '')) {
        suggestions.push([data.key.split('~')[1], data.value.length]);
      }
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

