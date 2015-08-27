var fs = require('fs');
var si = require('../../../')({indexPath: 'testindex2'});

var serializedDB = JSON.parse(fs.readFileSync('backup', 'utf8'));

si.replicateBatch(serializedDB, function(callback){
  console.log('db serialized');
});

document.getElementById('query').addEventListener('keyup', function() {
  var query = {
    "query": {
      "*": this.value.split(' ')
    }
  };
  si.search(query, function(err, results) {
    var resultHTML = '<b>total hits: </b>' + results.totalHits+ '<p>';
    for (var i = 0; i < results.hits.length; i++)
      resultHTML += '<div>' + JSON.stringify(results.hits[i]) + '</div><hr>';
    document.getElementById('results').innerHTML = resultHTML;
  });
});
