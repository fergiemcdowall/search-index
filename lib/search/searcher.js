var scontext = require('search-context');
var async = require('async');
var searchIndexLogger = require('../logger/searchIndexLogger');
var totalDocs = 0;
var lo = require('lodash');
//var hi = require('highland');
var queryDefaults = {
  'pageSize':100,
  'offset':0,
  'facetLength':10,
  'maxFacetLimit':100
}
var sortOnValueDesc = function(a,b) {if (a.value < b.value) return 1;
                                     if (a.value > b.value) return -1; return 0;}
var sortOnValueAsc = function(a,b) {if (a.value < b.value) return -1;
                                    if (a.value > b.value) return 1; return 0;}
var sortOnKeyDesc = function(a,b) {if (a.key < b.key) return 1;
                                   if (a.key > b.key) return -1; return 0;}
var sortOnKeyAsc = function(a,b) {if (a.key < b.key) return -1;
                                  if (a.key > b.key) return 1; return 0;}

//used on startup
exports.setTotalDocs = function (td) {
  totalDocs = td;
}


exports.search = function (indexesMultiply, q, callback) {
  lo.defaults(q, queryDefaults);
  var keySet = getKeySet(q);
  getDocumentFreqencies(indexesMultiply, q, keySet, function(err, frequencies) {
    if (err) return callback(getEmptyResultSet());
    async.parallel([
      function(callback) {
        getResults(q, frequencies, indexesMultiply, function(hits) {
          callback(null, hits);
        });
      },
      function(callback) {
        getFacets(q, indexesMultiply, function(facets){
          callback(null, facets);
        });
      },
      function(callback) {
        getFacetRanges(q, indexesMultiply, function(facetRanges){
          callback(null, facetRanges);
        });
      }],
      function (err, results) {
        var response = {};
        response['totalHits'] = frequencies.allDocsIDsInResultSet.length;
        response['query'] = q;
        response['facets'] = results[1]
        response['facetRanges'] = results[2]
        response['hits'] = results[0]
        callback(response);
      });
  });
};


var getSearchFieldQueryTokens = function(q) {
  var searchFieldQueryTokens = [];
  for (var queryField in q.query)
    for (var i = 0; i < q.query[queryField].length; i++)
      searchFieldQueryTokens.push(queryField + '~' + q.query[queryField][i]);
  return searchFieldQueryTokens;
}

var sortFacet = function(unsortedFacet, sortType) {
  if (sortType == 'keyAsc') unsortedFacet.sort(sortOnKeyAsc);
  else if (sortType == 'keyDesc') unsortedFacet.sort(sortOnKeyDesc);
  else if (sortType == 'valueAsc') unsortedFacet.sort(sortOnValueAsc);
  else unsortedFacet.sort(sortOnValueDesc);
  return unsortedFacet;
}

var getBucketedFacet = function(index, facetRangeKeys, callbacky) {
  async.reduce(facetRangeKeys, [], function(memo, item, callback){
    var key = item.start.split('~')[4] + '-' + item.end.split('~')[4];
    var thisSet = [];
    index.createReadStream({gte:item.start, lte:item.end})
      .on('data', function (data) {
        thisSet = thisSet.concat(data.value)
      })
      .on('end', function () {
        memo.push({"key":key, "value": uniqFast(thisSet).sort()});
        callback(null, memo);
      })
  }, function(err, result){
    //intersect IDs for every query token to enable multi-word faceting
    result.reduce(function(a, b, i, arr){
      if (a.key == b.key) {
        b.value = intersectionDestructive(a.value, b.value);
        delete arr[i - 1];
      }
      return b;
    });
    result = lo.compact(result);
    //TODO: to return sets instead of totals- do something here.
    for (var i in result)
      result[i].value = result[i].value.length;
    callbacky(result);
  });
}


var getNonRangedFacet = function(totalQueryTokens, index, facetRangeKeys, callbacky) {
  async.reduce(facetRangeKeys, [], function(memo, item, callback){
    index.createReadStream({gte:item.start, lte:item.end})
      .on('data', function (data) {
        memo.push({key: data.key.split('~')[4], value:uniqFast(data.value)})
      })
      .on('end', function () {
        callback(null, memo);
      })
  }, function(err, result){
    //intersect IDs for every query token to enable multi-word faceting
    result.sort(sortOnKeyAsc).reduce(function(a, b, i, arr){
      if (a.key == b.key) {
        b.counter = (a.counter + 1);
        b.value = intersectionDestructive(a.value, b.value);
        delete arr[i - 1];
      }
      else {
        if (a.counter < totalQueryTokens) delete arr[i - 1];
        if (a.value.length == 0) delete arr[i - 1];
        delete a.counter;
        b.counter = 1;
      }
      return b;
    });
    if (result[result.length -1].counter < totalQueryTokens)
      delete result[result.length -1];
    else if (result[result.length -1].counter == totalQueryTokens)
      delete result[result.length -1].counter;
    result = lo.compact(result);
    for (var i in result)
      result[i].value = result[i].value.length;
    callbacky(result);
  });
}

var getFacetRanges = function(q, index, callbacky) {
  if (!q.facetRanges) return callbacky({});
  var searchFieldQueryTokens = getSearchFieldQueryTokens(q);
  var facets = [];
  async.map(Object.keys(q.facetRanges), function(facetName, callback) { 
    var item = q.facetRanges[facetName];
    var facetRangeKeys = []
    var ranges = item.ranges || [['','~']];
    var limit = item.limit || queryDefaults.maxFacetLimit;
    var sortType = item.sort || 'valueDesc';
    for (var i = 0; i < ranges.length; i++) {
      for (var sfqt in searchFieldQueryTokens) {
        var range = {};
        var prefix = 'TF~' + searchFieldQueryTokens[sfqt] + '~' + facetName + '~';
        range.start = prefix + ranges[i][0];
        range.end = prefix + ranges[i][1] + '~';
        facetRangeKeys.push(range);
      }
    } 
    if (item.ranges) {
      getBucketedFacet(index, facetRangeKeys, function(facet){
        callback(null, {'key': facetName, 'value': sortFacet(facet, sortType).slice(0, limit)});
      })
    }
    else {
      getNonRangedFacet(searchFieldQueryTokens.length, index, facetRangeKeys, function(facet){
        callback(null, {'key': facetName, 'value': sortFacet(facet, sortType).slice(0, limit)});
      })
    }
  },
  function(err, result){
    callbacky(result);
  });
}


//supposedly fastest way to get unique values in an array
//http://stackoverflow.com/questions/9229645/remove-duplicates-from-javascript-array
var uniqFast = function(a) {
  var seen = {};
  var out = [];
  var len = a.length;
  var j = 0;
  for(var i = 0; i < len; i++) {
    var item = a[i];
    if(seen[item] !== 1) {
      seen[item] = 1;
      out[j++] = item;
    }
  }
  return out;
}



var getKeySet = function(q) {
  //generate keyset
  var keySet = [];
  for (var queryField in q.query) {
    for (var j = 0; j < q.query[queryField].length; j++) {
      if (q.filter) {
        for (k in q.filter) {
          for (var i = 0; i < q.filter[k].length; i++) {
            keySet.push('TF~' + queryField + '~' + q.query[queryField][j] + '~'
                        + k + '~' + q.filter[k][i]);
          }
        }
      }
      else {
        keySet.push('TF~' + queryField + '~' + q.query[queryField][j] + '~~');
      }
    }
  }
  return keySet;
}

var getEmptyResultSet = function() {
  var resultSet = {};
  resultSet['hits'] = [];
  return resultSet;
}

var getDocumentFreqencies = function(indexesMultiply, q, keySet, callback) {
  //Check document frequencies
  var docFreqs = {};
  var tfsets = [];
  indexesMultiply.get(keySet, function(err, data) {
    for (k in data) {
      if (data[k] == null) {
        //return a zero result
        docFreqs[keySet[i]] = 0;
        searchIndexLogger.debug('KEY ' + k + ' NOT FOUND IN DICTIONARY');
      }
      else {
        docFreqs[k] = data[k].length;
        tfsets.push(data[k]);
      }
    }
    var intersection = tfsets[0];
    for (var i = 1; i < tfsets.length; i++) {
      intersection = intersectionDestructive(intersection, tfsets[i]);
    }    
    if(intersection === undefined)
      return callback(true, {docFreqs:docFreqs, allDocsIDsInResultSet:intersection});
    else
      return callback(undefined, {docFreqs:docFreqs, allDocsIDsInResultSet:intersection});
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


var getFacets = function(q, indexesMultiply, callback) {
  //if no facet request callback and return
  if (!q.facets) return callback({});
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
    var thisFacetCat = facetks[counter].split('~')[3];
    indexesMultiply.createReadStream({
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
            finalFacets[k] = finalFacets[k].slice(0, q.facetLength);
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
}


var getResults = function(q, frequencies, indexesMultiply, callback) {
  var hits = [];
  //Key an RIKeyset in descending order of document frequency
  var RIKeySet = [];
  for (var k in frequencies.docFreqs)
    RIKeySet.push(k.replace(/^TF~/,'RI~'));
  var unsortedHits = {};
  indexesMultiply.get(RIKeySet, function(err, data) {
    //preserve the order of frequency (least frequent first)
    for (var j = 0; j < RIKeySet.length; j++) {
      var thisKey = RIKeySet[j];
      if (!data[thisKey]) return callback([]);
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
    sortedHits = sortedHits.slice(q.offset, (q.offset + q.pageSize));

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
        hits.push(hit);
      }
      hits = hits.sort(function(a,b){if (a.score < b.score)return 1;
                                     if (a.score > b.score) return -1;return 0;});
      callback(hits);
    });
  });
}
