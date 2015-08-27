var si = require('../../../')({indexPath:'reutersFromPouch'});
var _ = require('../../../node_modules/lodash');

//Set up a pouchDB
var dataset = require('./justTenPouchFormat.json');
var db = new PouchDB('reuters10');
for (var i = 0; i < dataset.length; i++)
  db.put(dataset[i]);
//End set up a pouchDB


//index pouch into search-index
var processDoc = function(datum) {
  pdoc = {};
  pdoc['id'] = datum.doc._id;
  pdoc['title'] = datum.doc.title;
  pdoc['body'] = datum.doc.body;
  pdoc['date'] = datum.doc.date;
  pdoc['places'] = datum.doc.places;
  pdoc['topics'] = datum.doc.topics;
  return pdoc;
}
db.allDocs({include_docs: true}).then(function(datasetFromPouch){
  console.log(datasetFromPouch);
  si.add({'batchName': 'pouchyData'}, _.map(datasetFromPouch.rows, processDoc), function(err) {
    if (!err) console.log('indexed!');
  });
})
//end index pouch into search-index

//interface
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
