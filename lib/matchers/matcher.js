var level = require('level');
//matcherIndex = level('matcher', {valueEncoding: 'json'});


exports.generateMatcher = function(reverseIndex, callback) {
  var lastToken;
  var lastTokenTally = 0;
  var totalMatcherTokens = 0;
  console.log('[info] generating matcher');
  reverseIndex.createReadStream({
    start: 'REVERSEINDEX~',
    end: 'REVERSEINDEX~~'})
    .on('data', function(data) {
      //only generate from non facet fields (denoted by *)
      if (data.key.split('~')[2]) return; 
      var thisTokenTf = parseFloat(data.key.split('~')[7]);
      var thisToken = data.key.split('~')[1];
      //      console.log(data.key + ' --- ' + thisTokenTf);
      if (!lastToken) {
        //first pass
        lastToken = thisToken;
      }
      else if (lastToken != thisToken) {
//        console.log(lastToken + ' : ' + lastTokenTally);
        reverseIndex.put('MATCHER~' + lastToken,
                         lastTokenTally, {'sync': true});
        totalMatcherTokens++;
        if (totalMatcherTokens % 1000 == 0)
          console.log('[info] ' + totalMatcherTokens + ' matcher tokens generated'); 
        lastToken = thisToken;
        lastTokenTally = thisTokenTf;
      }
      else {
        lastTokenTally += thisTokenTf;
      }
    })
    .on('close', function() {
      callback('[success] finished generating matcher with ' + totalMatcherTokens + ' tokens\n');
    });  
}


exports.matcher = function(reverseIndex, beginsWith, callback) {
  var suggestions = [];
  reverseIndex.createReadStream({
    start: 'MATCHER~' + beginsWith,
    end: 'MATCHER~' + beginsWith + '~'})
    .on('data', function(data) {
      suggestions.push([data.key.split('~')[1], data.value]);
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
