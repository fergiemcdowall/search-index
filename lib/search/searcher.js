var scontext = require('search-context');
var searchIndexLogger = require('../logger/searchIndexLogger');
var stopwords = require('natural').stopwords;
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
    var err = false;
    var docFreqs = {};
    var tfsets = [];
    var totalHits = 0;
    if (!q.pageSize)
      q.pageSize = 100; //default
    if (!q.offset)
      q.offset = 0;     //default
    indexesMultiply.get(keySet, function(err, data) {
      for (k in data) {
        if (data[k] == null) {
          //return a zero result
          docFreqs[keySet[i]] = 0;
          searchIndexLogger.debug('KEY ' + k + ' NOT FOUND IN DICTIONARY');
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
        err = true;
      else
        totalHits = intersection.length;
      callback(err, {docFreqs:docFreqs, allDocsIDsInResultSet:intersection});
    });
  };

  var getFacets = function(q, indexes, callback) {
    //if no facet request callback and return
    if (!q.facets){callback({});return;}


    var facetLength = 10;
    if (q.facetLength) facetLength = q.facetLength;

    var sortOnValueDesc = function(a,b) {
      if (a.value < b.value)
        return 1;
      if (a.value > b.value)
        return -1;
      return 0;
    }
    var sortOnValueAsc = function(a,b) {
      if (a.value < b.value)
        return -1;
      if (a.value > b.value)
        return 1;
      return 0;
    }
    var sortOnKeyDesc = function(a,b) {
      if (a.key < b.key)
        return 1;
      if (a.key > b.key)
        return -1;
      return 0;
    }
    var sortOnKeyAsc = function(a,b) {
      if (a.key < b.key)
        return -1;
      if (a.key > b.key)
        return 1;
      return 0;
    }


    var facetKeySet = [];
    var filterKeySet = [];
    for (var searchField in q.query) {
      for (var h = 0; h < q.query[searchField].length; h++) {
        var queryToken = q.query[searchField][h];
        for (var i = 0; i < q.facets.length; i++) {
          var facetName = q.facets[i];
          facetKeySet.push('TF~'
                           + searchField + '~'
                           + queryToken + '~'
                           + facetName + '~');
          if (q.filter) {
            if (q.filter[facetName]) {
              thisFilterValues = q.filter[facetName];
              for (var l = 0; l < thisFilterValues.length; l++) {
                filterKeySet.push('TF~'
                                  + searchField + '~'
                                  + queryToken + '~'
                                  + facetName + '~' + thisFilterValues[l]);
              }
            }
          }
        }
      }
    }
    
    var facetsSoFar = {};
    for (var i = 0; i < q.facets.length; i++)
      facetsSoFar[q.facets[i]] = {};


    //could be speeded up by always taking the least frequent keys first?
    var getFacetSets = function(facetks, filterks, counter) {

      var sortOnValueDesc = function(a,b) {
        if (a.value < b.value)
          return 1;
        if (a.value > b.value)
          return -1;
        return 0;
      }
      var sortOnValueAsc = function(a,b) {
        if (a.value < b.value)
          return -1;
        if (a.value > b.value)
          return 1;
        return 0;
      }
      var sortOnKeyDesc = function(a,b) {
        if (a.key < b.key)
          return 1;
        if (a.key > b.key)
          return -1;
        return 0;
      }
      var sortOnKeyAsc = function(a,b) {
        if (a.key < b.key)
          return -1;
        if (a.key > b.key)
          return 1;
        return 0;
      }

      var thisFacetCat = facetks[counter].split('~')[3];
      indexes.createReadStream({
        valueEncoding: 'json',
        limit: -1,
        start: facetks[counter],
        end: facetks[counter] + '~'})
        .on('data', function (data) {
          var thisFacetName = data.key.split('~')[4];

          var filteredFacetSet = data.value; //its actually unfiltered at this point
          //handle intersections for filters
          for (var i = 0; i < filterks.length; i++) {
            var filters = filterks[i].slice(0);
            filteredFacetSet = intersectionDestructive(filteredFacetSet, filters);
          }
          if (!facetsSoFar[thisFacetCat][thisFacetName]) {
            if (filteredFacetSet.length > 0)
              facetsSoFar[thisFacetCat][thisFacetName] = filteredFacetSet;
          }
          else {
            //handle intersections for multiword queries
            var intersection = intersectionDestructive(facetsSoFar[thisFacetCat][thisFacetName], filteredFacetSet);
            if (intersection.length > 0)
              facetsSoFar[thisFacetCat][thisFacetName] = intersection;
            else
              delete facetsSoFar[thisFacetCat][thisFacetName];
          }
        })
        .on('end', function () {
          if (++counter < facetks.length) {getFacetSets(facetKeySet, filterks, counter);}
          else {
            //flatten sets to counts
            var finalFacets = {};
            for (k in facetsSoFar) {
              finalFacets[k] = [];
              for (kk in facetsSoFar[k]) {
                
                //if facet is active add flag
                var thisFacetEntry = {'key':kk,'value':facetsSoFar[k][kk].length};
                if (q['filter']) if (q['filter'][k]) if (q['filter'][k].indexOf(kk) != -1)
                  thisFacetEntry['active'] = true;
                
                finalFacets[k].push(thisFacetEntry);
              }
              if (q.facetSort) {
                if (q.facetSort == 'keyAsc') finalFacets[k].sort(sortOnKeyAsc);
                else if (q.facetSort == 'keyDesc') finalFacets[k].sort(sortOnKeyDesc);
                else if (q.facetSort == 'valueAsc') finalFacets[k].sort(sortOnValueAsc);
                else if (q.facetSort == 'valueDesc') finalFacets[k].sort(sortOnValueDesc);
              }
              else {
                finalFacets[k].sort(sortOnValueDesc);
              }
              finalFacets[k] = finalFacets[k].slice(0, facetLength);
            }
            //finished!
            callback(finalFacets);
          }
        });
    }



    indexesMultiply.get(filterKeySet, function (err, data) {
      var filters = [];
      for (var i in data)
        filters.push(data[i]);
      getFacetSets(facetKeySet, filters, 0);
    });


  };

  //remove stopwords
  cleanQuery['query'] = {};
  for (var searchField in q['query']) {
    var cleanQueryFieldToken = searchField;
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

  var filterKeySet = [];
  for (k in q.filter) {
    for (var i = 0; i < q.filter[k].length; i++) {
      filterKeySet.push(k + '~' + q.filter[k][i]);
    }
  }

  //generate keyset
//  var RIKeySet = [];
  for (var queryField in cleanQuery.query) {
    for (var j = 0; j < cleanQuery.query[queryField].length; j++) {
      if (q.filter) {
        for (k in q.filter) {
          for (var i = 0; i < q.filter[k].length; i++) {
            keySet.push('TF~' + queryField + '~' + cleanQuery.query[queryField][j] + '~'
                        + k + '~' + q.filter[k][i]);
          }
        }
      }
      else {
        keySet.push('TF~' + queryField + '~' + cleanQuery.query[queryField][j] + '~~');
//        RIKeySet.push('RI~' + queryField + '~' + cleanQuery.query[queryField][j] + '~~');
      }
    }
  }


  var getEmptyResultSet = function() {
    var resultSet = {};
    resultSet['hits'] = [];
    return resultSet;
  }


  getDocumentFreqencies(indexesMultiply, function(err, frequencies) {
    if (err) return callback(getEmptyResultSet());
    getFacets(q, indexes, function(facets) {
      var response = {};
      response['totalDocs'] = frequencies.allDocsIDsInResultSet.length;
      response['facets'] = facets;
      response['hits'] = [];
      //Key an RIKeyset in descending order of document frequency
      var RIKeySet = [];
      for (var k in frequencies.docFreqs)
        RIKeySet.push(k.replace(/^TF~/,'RI~'));
      var unsortedHits = {};
      indexesMultiply.get(RIKeySet, function(err, data) {
        //preserve the order of frequency (least frequent first)
        for (var j = 0; j < RIKeySet.length; j++) {
          var thisKey = RIKeySet[j];
          if (!data[thisKey]) return callback(getEmptyResultSet());
          var firstPass = false;
          if (j == 0) firstPass = true; 
          var lastPass = false;
          if (j == (RIKeySet.length - 1)) lastPass = true; 
          //account for weighting
          var fieldWeight = 1;
          var fieldName = thisKey.split('~')[1];
          if (q.weight)
            if (q.weight[fieldName])
              fieldWeight = q.weight[fieldName];
          
          //How far down the frequency set you want to look.
          //Bigger == more accurate, smaller == faster.
          var seekCutOff = 500;

          //if there is only one key in the set, and only one query term
          //set cutoff to be the same length as the page
          if (RIKeySet.length == 1) seekCutOff = (q.pageSize + q.offset);

          var thisSeekCounter = 0;
          for (var i = 0; ((i < data[thisKey].length) && (thisSeekCounter < seekCutOff)); i++) {
            //insert all of this stuff into a sorted array so that
            //subsequent passes can 
            var docID = data[thisKey][i][1];
            if (frequencies.allDocsIDsInResultSet.indexOf(docID) != -1) {              
              thisSeekCounter++;
              var tuple = [thisKey, (fieldWeight*data[thisKey][i][0])];
              if (firstPass) {
                unsortedHits[docID] = [tuple];
              }
              else { 
                if (unsortedHits[docID]) {
                  if (unsortedHits[docID].length == j)
                    unsortedHits[docID].push(tuple);
                  else
                    delete unsortedHits[docID];
                }
              }
            }
//            if ((lastPass) && (Object.keys(unsortedHits).length > maxUnsortedHits)) break;
          }
        }
        //Iterate through object, calculating scores, account for weighting
        var sortedHits = [];
        for (var k in unsortedHits) {
          var score = 0;
          for (var i = 0; i < unsortedHits[k].length; i++)
            score += unsortedHits[k][i][1];
          sortedHits.push([k, score]);
        }
        sortedHits = sortedHits.sort(function(a,b) {
          if (a[1] < b[1])
            return 1;
          if (a[1] > b[1])
            return -1;
          return 0;
        });
          
        //set offset and pagesize
        var offset = 0;
        if (q.offset) offset = q.offset;
        var pageSize = 100;
        if (q.pageSize) pageSize = q.pageSize;
        sortedHits = sortedHits.slice(offset, (offset + pageSize));

        var IDsToBeGlued = [];
        var IDsToBeGluedWithScores = {};
        //get ids of results to glue
        for (var i = 0; i < sortedHits.length; i++) {
          IDsToBeGlued.push('DOCUMENT~' + sortedHits[i][0] + '~');
          IDsToBeGluedWithScores['DOCUMENT~' + sortedHits[i][0] + '~'] = sortedHits[i][1];
        }

        //Glue docs to IDs and send resultset

        indexesMultiply.get(IDsToBeGlued, function(err, data) {
          for (var k in data) {
            var hit = {};
            hit['id'] = k.split('~')[1];
            hit['score'] = IDsToBeGluedWithScores[k];
            hit['document'] = JSON.parse(data[k]);
            var terms = q['query']['*'];
            if (q['query'][q.teaser])
              terms = q['query'][q.teaser];
            if (q.teaser && hit.document[q.teaser]) {
              try {
                hit.document['teaser'] = 
                  scontext(hit.document[q.teaser],
                           terms, 400,
                           function hi (string) {
                             return '<span class="sc-em">' + string + '</span>'
                           });
              } catch (e) {
                searchIndexLogger.error('error with teaser generation: ' + e)
              }
            }
            delete hit['document']['*'];
            response["hits"].push(hit);
          }

          response['hits'] = response.hits.sort(function(a,b) {
            if (a.score < b.score)
              return 1;
            if (a.score > b.score)
              return -1;
            return 0;
          });
          response['totalHits'] = frequencies.allDocsIDsInResultSet.length;
          response['query'] = q;

          callback(response);

        });
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
