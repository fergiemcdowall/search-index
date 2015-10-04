var async = require('async');
var _ = require('lodash');
var scontext = require('search-context');
var skeleton = require('log-skeleton');
var sw = require('stopword');
var queryDefaults = {
  maxFacetLimit: 100,
  offset: 0,
  pageSize: 100,
};

module.exports = function (options) {
  var log = skeleton((options) ? options.log : undefined);
  var searcher = {};
  searcher.search = function (indexes, q, callback) {
    _.defaults(q, queryDefaults);
    q.query = removeStopwordsFromQuery(q.query);
    //NASTY HACK- LOOK INTO THIS PROPERLY
    if (q.query['*'] == '') return callback(getEmptyResultSet(q));
    var keySet = getKeySet(q);
    log.info(JSON.stringify(q));
    getDocumentFreqencies(indexes, q, keySet, function (err, frequencies) {
      //      console.log(frequencies)
      //improve returned resultset here:
      if (err) return callback(getEmptyResultSet(q));
      async.parallel([
        function (callback) {
          getResults(q, frequencies, indexes, function (hits) {
            callback(null, hits);
          });
        },
        function (callback) {
          getFacets(q, frequencies, indexes, function (facets) {
            callback(null, facets);
          });
        }],
        function (err, results) {
          var response = {};
          response.totalHits = frequencies.allDocsIDsInResultSet.length;
          response.totalDocsInIndex = frequencies.totalDocsInIndex;
          response.documentFrequencies = frequencies.df;
          response.fieldWeight = frequencies.fieldWeight;
          response.query = q;
          response.facets = results[1];
          response.facetRanges = results[2];
          response.hits = results[0];
          callback(response);
        });
    });
  };

  var removeStopwordsFromQuery = function (qquery) {
    for (var i in qquery) {
      if (qquery.hasOwnProperty(i)) {
        for (var k = 0; k < qquery[i].length; k++) {
          var swops = {inputSeparator: '￮',
                       outputSeparator: '￮',
                       stopwords: options.stopwords};
          qquery[i] = sw.removeStopwords(qquery[i].join('￮'), swops).split('￮');
        }
      }
    }
    return qquery;
  };

  var getSearchFieldQueryTokens = function (q) {
    var searchFieldQueryTokens = [];
    for (var queryField in q.query) { if (q.query.hasOwnProperty(queryField))
      for (var i = 0; i < q.query[queryField].length; i++) {
        searchFieldQueryTokens.push(queryField + '￮' + q.query[queryField][i]);
      }
    }
    return searchFieldQueryTokens;
  };

  var sortFacet = function (facet, sortType) {
    if (sortType == 'keyAsc') return _.sortBy(facet, 'key');
    else if (sortType == 'keyDesc') return _.sortBy(facet, 'key').reverse();
    else if (sortType == 'valueAsc') return _.sortBy(facet, 'value');
    else return _.sortBy(facet, 'value').reverse();
  };

  var getBucketedFacet = function (index, filter, facetRangeKeys, activeFilters, callbacky) {
    async.reduce(facetRangeKeys, [], function (memo, item, callback) {
      var gte = item.start.split('￮')[4];
      var lte = item.end.split('￮')[4];
      var key = gte + '-' + lte;
      var thisSet = [];
      index.createReadStream({gte:item.start, lte:item.end})
        .on('data', function (data) {
          thisSet = thisSet.concat(data.value);
        })
        .on('end', function () {
          var facetEntry = {key: key,
                            gte: gte,
                            lte: lte,
                            value: uniqFast(thisSet).sort()};
          for (var i = 0; i < activeFilters.length; i++)
            if ((activeFilters[i][0] == gte) && (activeFilters[i][1] == lte))
              facetEntry.active = true;
          memo.push(facetEntry);
          return callback(null, memo);
        });
    }, function (err, result) {
      //intersect IDs for every query token to enable multi-word faceting
      result.reduce(function (a, b, i, arr) {
        if (a.key == b.key) {
          b.value = intersectionDestructive(a.value, b.value);
          delete arr[i - 1];
        }
        return b;
      });
      result = _.compact(result);
      //TODO: to return sets instead of totals- do something here.
      for (var i in result) {
        if (result.hasOwnProperty(i)) {
          var filterClone = JSON.parse(JSON.stringify(filter));
          result[i].value = intersectionDestructive(result[i].value, filterClone);
          result[i].value = result[i].value.length;
        }
      }
      callbacky(result);
    });
  };

  var getNonRangedFacet = function (totalQueryTokens, index, facetRangeKeys,
                                   filterSet, filter, callbacky) {
    async.reduce(facetRangeKeys, [], function (memo, item, callback) {
      index.createReadStream({gte:item.start, lte:item.end})
        .on('data', function (data) {
          var thisKey = data.key.split('￮')[4];
          memo.push({key: thisKey,
                     gte: thisKey,
                     lte: thisKey,
                     value: data.value});
        })
        .on('end', function () {
          callback(null, memo);
        });
    }, function (err, result) {
      //intersect IDs for every query token to enable multi-word faceting
      if (result.length === 0) return callbacky([]);
      _.sortBy(result, 'key').reduce(function (a, b, i, arr) {
        if (a.key == b.key) {
          b.counter = (a.counter + 1);
          b.value = intersectionDestructive(a.value, b.value);
          delete arr[i - 1];
        }
        else {
          if (a.counter < totalQueryTokens) delete arr[i - 1];
          if (a.value.length === 0) delete arr[i - 1];
          delete a.counter;
          b.counter = 1;
        }
        return b;
      });
      if (result[result.length - 1].counter < totalQueryTokens)
        delete result[result.length - 1];
      else if (result[result.length - 1].counter == totalQueryTokens)
        delete result[result.length - 1].counter;
      result = _.compact(result);

      //filter
      if (filterSet) {
        for (var k in result) {
          if (result.hasOwnProperty(k)) {
            var f = filterSet.slice();
            result[k].value = intersectionDestructive(result[k].value, f);
            if (result[k].value.length === 0) delete result[k];
          }
        }
        result = _.compact(result);
      }

      for (var i in result) {
        if (result.hasOwnProperty(i)) {
          result[i].value = result[i].value.length;
          for (var j = 0; j < filter.length; j++) {
            if ((filter[j][0] <= result[i].key) &&  (result[i].key <= filter[j][1]))
              result[i].active = true;
          }
        }
      }
      callbacky(result);
    });
  };

  var getFacets = function (q, frequencies, index, callbacky) {
    if (!q.facets) return callbacky({});
    var searchFieldQueryTokens = getSearchFieldQueryTokens(q);
    async.map(Object.keys(q.facets), function (facetName, callback) {
      var item = q.facets[facetName];
      var facetRangeKeys = [];
      var ranges = item.ranges || [['', '']];
      var limit = item.limit || queryDefaults.maxFacetLimit;
      var sortType = item.sort || 'valueDesc';
      for (var i = 0; i < ranges.length; i++) {
        for (var sfqt in searchFieldQueryTokens) {
          if (searchFieldQueryTokens.hasOwnProperty(sfqt)) {
            var range = {};
            var prefix = 'TF￮' + searchFieldQueryTokens[sfqt] + '￮' + facetName + '￮';
            range.start = prefix + ranges[i][0];
            range.end = prefix + ranges[i][1] + '￮';
            facetRangeKeys.push(range);
          }
        }
      }
      if (item.ranges) {
        //set the filterSetKeys to the facet function, do an intersection to derive filters
        var activeFilters = [];
        if (q.filter) if (q.filter[facetName]) activeFilters = q.filter[facetName];
        getBucketedFacet
        (index, frequencies.allDocsIDsInResultSet, facetRangeKeys, activeFilters, function (facet) {
          callback(null, {key: facetName,
                          value: sortFacet(facet, sortType).slice(0, limit)});
        });
      }
      else {
        var filterValues = [];
        if (q.filter) filterValues = q.filter[facetName] || [];
        getNonRangedFacet(searchFieldQueryTokens.length, index,
           facetRangeKeys, frequencies.allDocsIDsInResultSet, filterValues, function (facet) {
           callback(null, {key: facetName,
                           value: sortFacet(facet, sortType).slice(0, limit)});
         });
      }
    },
    function (err, result) {
      callbacky(result);
    });
  };

  //supposedly fastest way to get unique values in an array
  //http://stackoverflow.com/questions/9229645/remove-duplicates-from-javascript-array
  var uniqFast = function (a) {
    var seen = {};
    var out = [];
    var len = a.length;
    var j = 0;
    for (var i = 0; i < len; i++) {
      var item = a[i];
      if (seen[item] !== 1) {
        seen[item] = 1;
        out[j++] = item;
      }
    }
    return out;
  };

  var getKeySet = function (q) {
    //generate keyset
    var keySet = [];
    for (var queryField in q.query) {
      if (q.query.hasOwnProperty(queryField)) {
        for (var j = 0; j < q.query[queryField].length; j++) {
          if (q.filter) {
            for (var k in q.filter) {
              if (q.filter.hasOwnProperty(k)) {
                for (var i = 0; i < q.filter[k].length; i++) {
                  keySet.push(['TF￮' + queryField + '￮' + q.query[queryField][j] +
                               '￮' + k + '￮' + q.filter[k][i][0],
                               'TF￮' + queryField + '￮' + q.query[queryField][j] +
                               '￮' + k + '￮' + q.filter[k][i][1]]);
                }
              }
            }
          }
          else {
            keySet.push(['TF￮' + queryField + '￮' + q.query[queryField][j] + '￮￮',
                         'TF￮' + queryField + '￮' + q.query[queryField][j] + '￮￮￮']);
          }
        }
      }
    }
    return keySet;
  };

  var getEmptyResultSet = function (q) {
    var resultSet = {};
    resultSet.query = q.query;
    resultSet.hits = [];
    resultSet.totalHits = 0;
    resultSet.facets = q.facets;
    return resultSet;
  };

  var getDocumentFreqencies = function (indexes, q, keySet, callback) {
    //Check document frequencies
    async.map(keySet, function (item, callbacky) {
      var uniq = [];
      indexes.createReadStream({gte: item[0], lte: item[1] + '￮'})
        .on('data', function (data) {
          uniq = uniqFast(uniq.concat(data.value));
        })
        .on('error', function (err) {
          log.warn('Oh my!', err);
        })
        .on('end', function () {
          callbacky(null, {key: item, value: uniq.sort()});
        });
    }, function (asyncerr, results) {
      var obj = {};
      obj[results[0].key[1].split('￮')[1] + '￮' +
          results[0].key[1].split('￮')[2]] = results[0].value;
      var df = results.reduce(function (prev, cur) {
        var curToken = cur.key[1].split('￮')[1] + '￮' + cur.key[1].split('￮')[2];
        if ((_.isEmpty(prev)) || (!prev[curToken])) prev[curToken] = cur.value;
        else prev[curToken] = _.intersection(prev[curToken], cur.value);
        return prev;
      }, obj);
      var intersection = _.values(df).reduce(function (prev, cur) {
        return _.intersection(prev, cur);
      });
      Object.keys(df).forEach(function (i) {
        df[i] = df[i].length;
      });

      var fieldWeight = {};
      Object.keys(df).forEach(function (i) {
        fieldWeight[i] = 1;
        if (q.weight) if (q.weight[i.split('￮')[0]])
          fieldWeight[i] = q.weight[i.split('￮')[0]]
      });
      
      var docFreqs = [];
      for (var i = 0; i < results.length; i++)
        docFreqs.push([results[i].length, keySet[i]]);
      //      var err = (intersection.length === 0) || undefined;
      indexes.get('DOCUMENT-COUNT', function (err, value) {
        return callback(err,
                        {docFreqs:docFreqs.sort(),
                         allDocsIDsInResultSet:intersection,
                         df: df,
                         fieldWeight: fieldWeight,
                         totalDocsInIndex: value});
      });
    });
  };

  function intersectionDestructive (a, b) {
    var result = [];
    while (a.length > 0 && b.length > 0) {
      if (a[0] < b[0]) {a.shift();}
      else if (a[0] > b[0]) {b.shift();}
      else /* they're equal */ {
        result.push(a.shift());
        b.shift();
      }
    }
    return result;
  }

  var getResults = function (q, frequencies, indexes, callbackX) {
    var hits = [];
    //Key an RIKeyset in descending order of document frequency
    var RIKeySet = [];
    for (var k in frequencies.docFreqs) {
      if (frequencies.docFreqs.hasOwnProperty(k)) {
        RIKeySet.push(k.replace(/^TF￮/, 'RI￮'));
      }
    }
    async.mapSeries(frequencies.docFreqs, function (item, callbacker) {
      var gte = item[1][0].replace(/^TF￮/, 'RI￮');
      var lte = item[1][1].replace(/^TF￮/, 'RI￮');
      var hits = {
        field: gte.split('￮')[1],
        token: gte.split('￮')[2],
        rangeStart: gte.split('￮')[4],
        rangeEnd: lte.split('￮')[4],
        tf: []
      };
      indexes.createReadStream({gte: gte, lte: lte + '￮'})
        .on('data', function (data) {
          for (var i = 0; i < data.value.length; i++)
            if (frequencies.allDocsIDsInResultSet.indexOf(data.value[i][1]) != -1)
              hits.tf.push(data.value[i]);
        })
        .on('error', function (err) {
          log.warn('Oh my!', err);
        })
        .on('end', function () {
          return callbacker(null, hits);
        });
    }, function (err, result) {
      var alreadyProcessed = [];
      for (var i = 0; i < result[0].tf.length; i++) {
        var thisID = result[0].tf[i][1];
        //result[0] contains dupes- check for already processed
        if (alreadyProcessed.indexOf(thisID) != -1) continue;
        else alreadyProcessed.push(thisID);
        var hit = {
          id: thisID,
          tf: [],
          score: 0
        };
        result.forEach(function (tokenSets) {
          tokenSets.tf.forEach(function (tokenSet) {
            if (tokenSet[1] != hit.id) return;
            hit.tf.push(
              [tokenSets.field + '￮' + tokenSets.token,
               tokenSet[0]]
            );
          });
        });
        //strip duplicates from tf object
        hit.tf = _.uniq(hit.tf, function (n) {
          return n[0] + n[1];
        });
        hit.tf.forEach(function (tfEntry) {
          //tf-idf here
          var tf = +tfEntry[1];
          var weight = +frequencies.fieldWeight[tfEntry[0]];
          var totalDocsInIndex = +frequencies.totalDocsInIndex;
          var df = +frequencies.df[tfEntry[0]];
          var idf = weight * Math.log10(1 + (totalDocsInIndex / df));
          hit.score = +hit.score + (tf * idf);
        });

        hits.push(hit);
      }
      hits = hits.sort(function (a, b) {
        if (a.score < b.score) return 1;
        if (a.score > b.score) return -1;
        return 0;
      });
      hits = hits.slice((+q.offset), (+q.offset) + (+q.pageSize));
      glueDocs(hits, indexes, q, function (result) {
        return callbackX(result);
      });
    });
  };

  var glueDocs = function (hits, indexes, q, callbackX) {
    async.mapSeries(hits, function (item, callback) {
      indexes.get('DOCUMENT￮' + item.id + '￮', function (err, value) {
        item.document = value;
        var terms = q.query['*'];
        if (q.query[q.teaser])
          terms = q.query[q.teaser];
        if (q.teaser && item.document[q.teaser]) {
          try {
            item.document.teaser = scontext(item.document[q.teaser], terms, 400, function hi (string) {
              return '<span class="sc-em">' + string + '</span>';
            });
          } catch (e) {
            log.warn('error with teaser generation: ' + e);
          }
        }
        callback(null, item);
      });
    }, function (err, result) {
      return callbackX(result);
    });
  };
  return searcher;
};
