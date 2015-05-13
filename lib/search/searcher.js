var async = require('async');
var lo = require('lodash');
var log = require('bunyan').createLogger({name: "norch"});
var scontext = require('search-context');
var totalDocs = 0;
var queryDefaults = {
  'facetLength':10,
//  'filter':{}
  'maxFacetLimit':100,
  'offset':0,
  'pageSize':100,
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

exports.setLogLevel = function (level) {
  log.level(level);
}


exports.search = function (indexes, q, callback) {
  lo.defaults(q, queryDefaults);
  var keySet = getKeySet(q);
  log.info(JSON.stringify(q));
  getDocumentFreqencies(indexes, q, keySet, function(err, frequencies) {
    if (err) return callback(getEmptyResultSet());
    async.parallel([
      function(callback) {
        getResults(q, frequencies, indexes, function(hits) {
          callback(null, hits);
        });
      },
      function(callback) {
        getFacets(q, frequencies, indexes, function(facets){
          callback(null, facets);
        });
      }],
      function (err, results) {
        var response = {};
        response['totalHits'] = frequencies.allDocsIDsInResultSet.length;
        response['query'] = q;
        response['facets'] = results[1];
        response['facetRanges'] = results[2];
        response['hits'] = results[0];
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

var getBucketedFacet = function(index, filter, facetRangeKeys, callbacky) {
  async.reduce(facetRangeKeys, [], function(memo, item, callback){
    var gte = item.start.split('~')[4];
    var lte = item.end.split('~')[4];
    var key = gte + '-' + lte;
    var thisSet = [];
    index.createReadStream({gte:item.start, lte:item.end})
      .on('data', function (data) {
        thisSet = thisSet.concat(data.value)
      })
      .on('end', function () {
        memo.push({"key": key,
                   "gte": gte,
                   "lte": lte,
                   "value": uniqFast(thisSet).sort()});
        return callback(null, memo);
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
    for (var i in result) {
      var filterClone = JSON.parse(JSON.stringify(filter));
      result[i].value = intersectionDestructive(result[i].value, filterClone);
      result[i].value = result[i].value.length;
//      result[i].value = result[i].value;
    }
    callbacky(result);
  });
}


var getNonRangedFacet = function(totalQueryTokens, index, facetRangeKeys,
                                 filterSet, filter, callbacky) {
  async.reduce(facetRangeKeys, [], function(memo, item, callback){
    index.createReadStream({gte:item.start, lte:item.end})
      .on('data', function (data) {
        var thisKey = data.key.split('~')[4];
        memo.push({key: thisKey,
                   gte: thisKey,
                   lte: thisKey,
                   value: data.value})
      })
      .on('end', function () {
        callback(null, memo);
      })
  }, function(err, result){
    //intersect IDs for every query token to enable multi-word faceting
    if (result.length == 0) return callbacky([]);
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

//filter
    if (filterSet) {
      for (var i in result) {
        var f = filterSet.slice()
        result[i].value = intersectionDestructive(result[i].value, f);
        if (result[i].value.length == 0) delete result[i];
      }
      result = lo.compact(result);
    }

    for (var i in result) {
      result[i].value = result[i].value.length;
      for (var j = 0; j < filter.length; j++) {
        if ((filter[j][0] <= result[i].key) &&  (result[i].key <= filter[j][1]))
          result[i]['active'] = true;
      }
    }
    
    callbacky(result);
  });
}


var getFacets = function(q, frequencies, index, callbacky) {
  if (!q.facets) return callbacky({});
  var searchFieldQueryTokens = getSearchFieldQueryTokens(q);
  var facets = [];
  async.map(Object.keys(q.facets), function(facetName, callback) { 
    var item = q.facets[facetName];
    var facetRangeKeys = []
    var ranges = item.ranges || [['','']];
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
      //set the filterSetKeys to the facet function, do an intersection to derive filters
      getBucketedFacet
      (index, frequencies.allDocsIDsInResultSet, facetRangeKeys, function(facet){
        callback(null, {'key': facetName,
                        'value': sortFacet(facet, sortType).slice(0, limit)});
      })
    }
    else {
      var filterValues = [];
      if (q.filter) filterValues = q.filter[facetName] || []; 
      getNonRangedFacet(searchFieldQueryTokens.length, index,
         facetRangeKeys, frequencies.allDocsIDsInResultSet, filterValues, function(facet){
         callback(null, {'key': facetName,
                         'value': sortFacet(facet, sortType).slice(0, limit)});
       });
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
            keySet.push(['TF~' + queryField + '~' + q.query[queryField][j] + '~'
                         + k + '~' + q.filter[k][i][0],
                         'TF~' + queryField + '~' + q.query[queryField][j] + '~'
                         + k + '~' + q.filter[k][i][1]]);
          }
        }
      }
      else {
        keySet.push(['TF~' + queryField + '~' + q.query[queryField][j] + '~~',
                     'TF~' + queryField + '~' + q.query[queryField][j] + '~~~']);
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


var getDocumentFreqencies = function(indexes, q, keySet, callback) {
  //Check document frequencies
  var docFreqs = {};
  async.map(keySet, function(item, callbacky){
    var uniq = [];
    indexes.createReadStream({'gte':item[0], 'lte':item[1] + '~'})
      .on('data', function (data) {
        uniq = uniqFast(uniq.concat(data.value));
      })
      .on('error', function (err) {
        log.error('Oh my!', err)
      })
      .on('end', function () {
        callbacky(null, uniq.sort());
      })
  }, function (err, results) {
    var docFreqs = [];
    var intersection = results[0];
    docFreqs.push([results[0].length, keySet[0]]);
    //why does this start at 1?
    for (var i = 1; i < results.length; i++) {
      docFreqs.push([results[i].length, keySet[i]]);
      intersection = intersectionDestructive(intersection, results[i]);
    }
    var err = (intersection.length == 0) || undefined;
    return callback(err, {docFreqs:docFreqs.sort(), allDocsIDsInResultSet:intersection});
  })
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


var getResults = function(q, frequencies, indexes, callbackX) {
  var hits = [];
  //Key an RIKeyset in descending order of document frequency
  var RIKeySet = [];
  for (var k in frequencies.docFreqs)
    RIKeySet.push(k.replace(/^TF~/,'RI~'));
  async.mapSeries(frequencies.docFreqs, function(item, callbacker) {
    var gte = item[1][0].replace(/^TF~/,'RI~');
    var lte = item[1][1].replace(/^TF~/,'RI~');
    var hits = {};
    hits['field'] = gte.split('~')[1];
    hits['token'] = gte.split('~')[2];
    hits['filterField'] = gte.split('~')[3];
    hits['rangeStart'] = gte.split('~')[4];
    hits['rangeEnd'] = lte.split('~')[4];
    hits['tf'] = [];
    var seekLimit = (+q.pageSize + +q.offset) * 1.5;
    var seekCounter = 0;
//    console.log(JSON.stringify(frequencies, null, 2));
    indexes.createReadStream({'gte':gte, 'lte':lte + '~'})
      .on('data', function (data) {
//        console.log(data.key);
        for (var i = 0; i < data.value.length; i++)
          if (frequencies.allDocsIDsInResultSet.indexOf(data.value[i][1]) != -1) {
//            console.log(data.value[i]);
            hits.tf.push(data.value[i])
          }
        if (seekCounter > seekLimit) {
          this.destroy();
          return callbacker(null, hits);
        }
      })
      .on('error', function (err) {
        log.error('Oh my!', err)
      })
      .on('end', function () {
        return callbacker(null, hits);
      });
  }, function(err, result) {
    var alreadyProcessed = [];
    for (var i = 0; i < result[0].tf.length; i++) {
      var thisID = result[0].tf[i][1];
      //result[0] contains dupes- check for already processed
      if (alreadyProcessed.indexOf(thisID) != -1) continue;
      else alreadyProcessed.push(thisID);
      var hit = {};
      hit['id'] = thisID;
      hit.relevance = [];
      //TODO: add field weighting
      for (var j = 0; j < result.length; j++) {
        for (var k = 0; k < result[j].tf.length; k++) { 
          if (result[j].tf[k][1] == hit.id) {
            var weight = 1
            if (q.weight) if (q.weight[result[j].field])
              weight = q.weight[result[j].field];
            hit.relevance.push([result[j].field, result[j].token, result[j].tf[k][0], weight]);
          }
        }
      }
      //strip duplicates from relevance object
      hit.relevance = _.uniq(hit.relevance, function(n){return n[0] + n[1]});
      hit['score'] = 0;
      for (var j = 0; j < hit.relevance.length; j++) 
        hit.score = +hit.score + (+hit.relevance[j][2] * +hit.relevance[j][3]);
      hits.push(hit);
    }
    getBoosts(hits, q, function(bostedHits) {
      hits = hits.sort(function(a,b){if (a.score < b.score)return 1;
                                     if (a.score > b.score)return -1;return 0;});
      hits = hits.slice(+q.offset, +q.offset + +q.pageSize);
      glueDocs(hits, indexes, q, function(result) {
        return callbackX(result);
      }); 
    });
  });
}

var getBoosts = function(hits, q, callback) {
  return callback(hits);
}

var glueDocs = function(hits, indexes, q, callbackX) {
  async.mapSeries(hits, function(item, callback) {
    indexes.get('DOCUMENT~' + item.id + '~', function(err, value) {
      item['document'] = JSON.parse(value);
      var terms = q['query']['*'];
      if (q['query'][q.teaser])
        terms = q['query'][q.teaser];
      if (q.teaser && item.document[q.teaser]) {
        try {
          item.document['teaser'] = 
            scontext(item.document[q.teaser], terms, 400, function hi (string) {
              return '<span class="sc-em">' + string + '</span>'});
        } catch (e) {log.error('error with teaser generation: ' + e)}
      }
      callback(null, item);
    })
  }, function(err, result) {
    return callbackX(result);
  });  
}
