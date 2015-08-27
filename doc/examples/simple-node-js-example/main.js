
var si = require('../../../')({indexPath: 'testindex'});
var dataset = require('../../../node_modules/reuters-21578-json/data/justTen/justTen.json');

si.add({'batchName': 'reuters'}, dataset, function(err) {
  if (!err) console.log('indexed!');
  console.log('doing a test search...');
  si.search({"query": {"*":["usa", "reuter"]}}, function(err, results) {
    console.log('total hits: ' + results.totalHits);
    for (var i = 0; i < results.hits.length; i++) {
      console.log();
      console.log('* ' + results.hits[i].document.title + ' *');
      console.log('* ' + results.hits[i].document.date + ' *');
      console.log();
      console.log(results.hits[i].document.body);
      console.log('--------------------');
    }
  });
});


