var work = require('webworkify');

var w = work(require('./index.js'));

w.addEventListener('message', function (ev) {
     
    if (ev.data.hits) { 
        var results = ev.data;
        var resultHTML = '<b>total hits: </b>' + results.totalHits+ '<p>';
        for (var i = 0; i < results.hits.length; i++)
        resultHTML += '<div>' + JSON.stringify(results.hits[i]) + '</div><hr>';
        document.getElementById('results').innerHTML = resultHTML;
    } else {
        console.log(ev.data);
    }
});

document.getElementById('query').addEventListener('keyup', function() {
    var query = {
        "query": {
        "*": this.value.split(' ')
        }
    };
    w.postMessage(query);
});

