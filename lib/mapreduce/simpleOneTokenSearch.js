/*
This is a Mapreduce function to search the index for query strings of
only one token with no faceting or filtering employed. The simplest,
fastest search you can do
*/

//TODO: pagination
//      facets
//      field boost


var async = require('async');
var scontext = require('search-context');

exports.search = function (reverseIndex, docFreqs, docFreqIndex, q, cleanQuery, callback) {

  var queryTerm = cleanQuery['query'][0];
  var resultSet = {};
  resultSet['query'] = q;
  resultSet['totalHits'] = 0;
  resultSet['transformedQuery'] = cleanQuery;
  resultSet['hits'] = [];

  //get document frequency for queryTerm
  docFreqIndex.get(queryTerm + '~~', function (err, value) {
    //err usually means empty set
    if (!err) resultSet['totalHits'] = value;
    reverseIndex.createReadStream({
      valueEncoding: 'json',
      reverse: true,
      limit: 20,
      start: 'REVERSEINDEX~' + queryTerm + '~~~forage.composite~~',
      end: 'REVERSEINDEX~' + queryTerm + '~~~forage.composite~'})
      .on('data', function (data) {
        var keyFields = data.key.split('~');
        resultSet['hits'].push({'id': keyFields[6],
                                'relevance':{'tf': {queryTerm:keyFields[5]}},
                                'score': keyFields[5]});
      })
      .on('end', function () {
        //glue doc to result
        fetchAndGlueDocument = function (item, callback) {
          reverseIndex.get('DOCUMENT~' + item.id + '~', function (err, value) {
            item['document'] = JSON.parse(value);
            //teaser generation
            if (q['teaser']) {
              try {
                item['document']['teaser'] = 
                  scontext(item['document'][q['teaser']],
                           [queryTerm], 400,
                           function hi (string) {
                             return '<span class="sc-em">' + string + '</span>'
                           });
              } catch (e) {}
            }
            delete item['document']['forage.composite'];
            callback(null, item);
          });
        }
        //asynchronously glue documents to results
        async.map(resultSet.hits, fetchAndGlueDocument, function(err){
          //facets
          var facets = {};
          docFreqIndex.createReadStream({
            valueEncoding: 'json',
            limit: 20,
            start: queryTerm + '~',
            end: queryTerm + '~~'})
            .on('data', function (data) {
              if (data.key.indexOf('~~') != -1) return;
              var splitKey = data.key.split('~');
              var facetCategory = splitKey[1];
              if (q.facets.indexOf(facetCategory) == -1) return;
              var facetName = splitKey[2];
              var facetValue = data.value;
              if (!facets[facetCategory]) facets[facetCategory] = {};
              facets[facetCategory][facetName] = facetValue;
            })
            .on('error', function (err) {
              console.log('Oh my!', err)
            })
            .on('end', function () {
              resultSet['facets'] = facets;
              callback(resultSet);
            });
        });
      });
  });
}
