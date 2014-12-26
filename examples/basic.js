var si = require('../')({indexPath: 'si-example'});

// grab "documents" from the DOM.
var docs = Array.prototype.map.call(document.querySelectorAll('.document'),
function(doc) {
  return {
    'id': doc.id,
    'title': doc.querySelector('h2').textContent,
    'body': doc.querySelector('p').textContent,
    'tags': doc.getAttribute('data-tags').split(/\s|,/)
  }
});

console.log(docs);

si.add({ batchName: 'initial', filters: ['tags'] }, docs, function(err) {
  if (err) throw err;
  console.log('indexed!');
})

var queryElement = document.querySelector('[name="query"]');
var tagsElement = document.querySelector('[name="tags"]')
var timeout;
[queryElement, tagsElement].forEach(function(el) {
  el.addEventListener('keyup', function() {
    if(timeout) clearTimeout(timeout);
    timeout = setTimeout(search,500)
  });
})

function search() {
  var query = {
    "query": {
      "*": queryElement.value.split(/[^\w]+/)
    },
    "offset": "0",
    "pageSize": "20",
    "weight": {
      "title": ["10"]
    }
  };
  var filter = tagsElement.value.trim();
  if(filter.length > 0) query.filter = { "tags": filter.split(/[^\w-]+/) }

  si.search(query, function(err, results) {
    results = results.hits.sort(function(a, b) {
      return b.score - a.score;
    });
    var summary = results.map(function(res) {
      return res.document.title + ': search score='+res.score;
    });
    document.querySelector('#search-results').innerHTML = summary.map(
      function(item) { return '<li>' + item + '</li>'}
    ).join('');
  })
}
