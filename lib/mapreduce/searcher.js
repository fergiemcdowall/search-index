var stopwords = require('natural').stopwords;
var mapreduce = require('./mapreduce.js');
var logger = require('../logger.js');
var totalDocs = 0;



//used on startup
exports.setTotalDocs = function (td) {
  totalDocs = td;
}

exports.search = function (indexes, indexesMultiply, q, callback) {
  var cleanQuery = {};
  var canSearch = true;
  var keySet = [];

  var getDocumentFreqencies = function(indexesMultiply, callback) {
    //Check document frequencies
    var docFreqs = {};
    var tfsets = [];
    var totalHits = 0;
    indexesMultiply.get(keySet, function(err, data) {
      for (k in data) {
        if (data[k] == null) {
          //return a zero result
          docFreqs[keySet[i]] = 0;
          logger.debug('KEY ' + k + ' NOT FOUND IN DICTIONARY');
        }
        else {
      //    totalHits = data[k].length;
          docFreqs[k] = data[k].length;
          tfsets.push(data[k]);
        }
      }
      var intersection = tfsets[0];
      for (var i = 1; i < tfsets.length; i++) {
        intersection = intersectionDestructive(intersection, tfsets[i]);
      }

      if(intersection === undefined)
        totalHits = 0;
      else
        totalHits = intersection.length;

      console.log('totalHits: ' + totalHits);
      //    console.log('tntersection: ' + intersection);
      //    pickSearch(docFreqs);
      callback({docFreqs:docFreqs, allDocsIDsInResultSet:intersection});
    });
  };

  var getFacets = function(q, indexes, callback) {
    //if no facet request callback and return
    if (!q.facets){callback({});return;}

    var facetKeySet = [];
    for (var searchField in q.query) {
      for (var h = 0; h < q.query[searchField].length; h++) {
        var queryToken = q.query[searchField][h];
        for (var i = 0; i < q.facets.length; i++) {
          facetKeySet.push('TF~' + searchField + '~' + queryToken + '~' + q.facets[i]);
        }
      }
    }
    console.log('facetKeySet: ' + facetKeySet);
    console.log('facet: ' + JSON.stringify(q.facets));
    
    var facetsSoFar = {};
    for (var i = 0; i < q.facets.length; i++)
      facetsSoFar[q.facets[i]] = {};

    //could be speeded up by always taking the least frequent keys first?
    var getFacetSets = function(fks, counter) {
      var thisFacetCat = fks[counter].split('~')[3];
      indexes.createReadStream({
        valueEncoding: 'json',
        limit: -1,
        start: fks[counter] + '~',
        end: fks[counter] + '~~'})
        .on('data', function (data) {
          var thisFacetName = data.key.split('~')[4];
          if (!facetsSoFar[thisFacetCat][thisFacetName]) {
            facetsSoFar[thisFacetCat][thisFacetName] = data.value;
          }
          else {
            var intersection = intersectionDestructive(facetsSoFar[thisFacetCat][thisFacetName], data.value);
            if (intersection.length > 0)
              facetsSoFar[thisFacetCat][thisFacetName] = intersection;
            else
              delete facetsSoFar[thisFacetCat][thisFacetName];
          }
        })
        .on('end', function () {
          if (++counter < fks.length) {getFacetSets(facetKeySet, counter);}
          else {
            //flatten sets to counts
            var finalFacets = {};
            for (k in facetsSoFar) {
              finalFacets[k] = [];
              for (kk in facetsSoFar[k]) {
                finalFacets[k].push({'key':kk,'value':facetsSoFar[k][kk].length});
              }
              finalFacets[k].sort(function(a,b) {
                if (a.value < b.value)
                  return 1;
                if (a.value > b.value)
                  return -1;
                return 0;
              });
            }
            //finished!
            callback(finalFacets);
          }
        });
    }
    getFacetSets(facetKeySet, 0);
  };

  //remove stopwords
  cleanQuery['query'] = {};
  for (var searchField in q['query']) {
    var cleanQueryFieldToken = searchField;
//    if (searchField == '*') cleanQueryFieldToken = 'forage.composite'
    cleanQuery['query'][cleanQueryFieldToken] = [];
    for (var k = 0; k < q['query'][searchField].length; k++) {
      cleanQuery['query'][cleanQueryFieldToken].push(q['query'][searchField][k]);
    }
    //TODO: does this make sense?
    if (cleanQuery['query'][cleanQueryFieldToken].length == 0) {
      canSearch = false;
    }
  }
  
  if (q.weight) {
    cleanQuery['weight'] = q.weight;
  }

  logger.debug('cleanQuery: ' + JSON.stringify(cleanQuery));


  var filterKeySet = [];
  for (k in q.filter) {
    for (var i = 0; i < q.filter[k].length; i++) {
      filterKeySet.push(k + '~' + q.filter[k][i]);
    }
  }
//  logger.debug('filterKeySet: ' + filterKeySet);
//  logger.debug('filter: ' + JSON.stringify(q.filter));

  //generate keyset
  for (var queryField in cleanQuery.query) {
    for (var j = 0; j < cleanQuery.query[queryField].length; j++) {
      if (q.filter) {
        for (k in q.filter) {
//          logger.debug('filter[k]: ' + q.filter[k]);
          for (var i = 0; i < q.filter[k].length; i++) {
            keySet.push('TF~' + queryField + '~' + cleanQuery.query[queryField][j] + '~'
                        + k + '~' + q.filter[k][i]);
          }
        }
      }
      else {
        keySet.push('TF~' + queryField + '~' + cleanQuery.query[queryField][j] + '~~');
      }
    }
  }
  logger.debug('keySet: ' + keySet);

  getDocumentFreqencies(indexesMultiply, function(frequencies) {
//    console.log('docFreqs: ' + JSON.stringify(frequencies));
    getFacets(q, indexes, function(facets) {
//      console.log('facets: ' + JSON.stringify(facets))
      mapreduce.search(indexes,
                       indexesMultiply,
                       frequencies.docFreqs,
                       q,
                       cleanQuery,
                       filterKeySet,
                       frequencies.allDocsIDsInResultSet,
                       totalDocs,
                       facets,
                       function(msg) {
                         callback(msg);
                       });
    });
  });
};


function intersectionDestructive(a, b) {
  var result = new Array();
  while( a.length > 0 && b.length > 0 ) {
     if      (a[0] < b[0]){ a.shift(); }
     else if (a[0] > b[0]){ b.shift(); }
     else /* they're equal */ {
       result.push(a.shift());
       b.shift();
     }
  }
  return result;
}
