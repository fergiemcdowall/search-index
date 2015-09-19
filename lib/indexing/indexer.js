/*jshint -W083 */ //makes jslint overlook functions in lodash for-loops

var _ = require('lodash');
var async = require('async');
var calibrater = require('./calibrater.js')();
var deleter = require('./deleter.js')();
var hash = require('object-hash');
var tv = require('term-vector');
var tf = require('term-frequency');
var skeleton = require('log-skeleton');

module.exports = function (options) {
  var log = skeleton((options) ? options.log : undefined);
  var indexer = {};


  var removeInvalidFields = function (doc) {
    for (var fieldKey in doc) {
      if (_.isArray(doc[fieldKey])) continue;
      else if (doc[fieldKey] === null) {
        delete doc[fieldKey];
        log.warn(doc.id + ': ' + fieldKey + ' field is null, SKIPPING');
      }
      //only index fields that are strings or numbers
      else if (!(_.isString(doc[fieldKey]) || _.isNumber(doc[fieldKey]))) {
        delete doc[fieldKey];
        log.warn(doc.id + ': ' + fieldKey +
                 ' field not string or array, SKIPPING');
      }
    }
    return doc;
  };

  var arrayifyFilters = function (doc, filters) {
    var facets = {};
    _.forEach(filters, function (facet) {
      if (doc[facet]) {
        if (!_.isArray(doc[facet]))
          facets[facet] = [doc[facet]];
        facets[facet] = _.uniq(doc[facet]);
      }
    });
    return facets;
  };


  var generateKeys = function (docVector, searchableField, facet, docFacet) {
    var returnObject = {};
    returnObject.deleteKeys = [];
    returnObject.tfValues = [];
    _.forEach(docVector, function (vecElem) {
      vecElem.field.push(['*', 1]);
      var highestFrequencyCount = _.max(vecElem.field, 1)[1];
      var weight = vecElem.weight;
      _.forEach(vecElem.field, function (vecField) {
        var entrykey = searchableField + '￮' + vecField[0] + '￮' + facet + '￮' + docFacet;
        var tfValue = +weight + (vecField[1] / highestFrequencyCount / docVector.length).toFixed(10);
        var existingEntry = _.findIndex(returnObject.tfValues, function (chr) { return chr[0] == entrykey;});
        if (existingEntry != -1)
          returnObject.tfValues[existingEntry] = [entrykey, ((+returnObject.tfValues[existingEntry][1]) + (+tfValue))];
        else
          returnObject.tfValues.push([entrykey, tfValue]);
        returnObject.deleteKeys.push(entrykey);
      });
    });
    return returnObject;
  };



  indexer.addBatchToIndex = function (indexes, batch, batchOptions, callbacky) {
    var salt = 0;
    if (!_.isArray(batch) && _.isPlainObject(batch)) {
      batch = [batch];
    }

    //generate IDs if none are present and stringify numeric IDs
    batch.map(function(doc) {
      if (!doc.id)
        doc.id = (++salt) + '-' + hash(doc);
      doc.id = doc.id + ''; // stringify ID
    })

    deleter.newDeleteBatch(_.pluck(batch, 'id'), indexes, function (err) {
      var dbInstructions = [];
      batch.forEach(function(doc) {
        var docIndexEntries = [];
        if (err) log.warn(err);
        if (!_.isPlainObject(doc))
          return callbacky(new Error('Malformed document'), {});
        doc = removeInvalidFields(doc);
        batchOptions = _.defaults(batchOptions, options);
        if (batchOptions.fieldsToStore == 'all')
          batchOptions.fieldsToStore = Object.keys(doc);


        log.info('indexing ' + doc.id);
        docIndexEntries.push({
          type: 'put',
          key: 'DOCUMENT￮' + doc.id + '￮',
          value:  _.pick(doc, batchOptions.fieldsToStore)
        });
        var freqs = [] //put document frequencies in here
        _.forEach(doc, function(field, fieldName) {
          var fieldOptions = _.defaults(_.find(batchOptions.fieldOptions, 'fieldName', fieldName) || {}, batchOptions.defaultFieldOptions);
          if (fieldName == 'id') fieldOptions.stopwords = '';   // because you cant run stopwords on id field
          else fieldOptions.stopwords = batchOptions.stopwords 
          if (_.isArray(field)) field = field.join(' '); // make filter fields searchable
          

          var v = tv.getVector(field + '', {
            separator: batchOptions.separator,
            stopwords: fieldOptions.stopwords,
            nGramLength: fieldOptions.nGramLength
          })
          v.push(['*', 1]) //can do wildcard searh on this field

          var freq = tf.getTermFrequency(v, {
            scheme: 'doubleLogNormalization0.5', 
            weight: fieldOptions.weight
          });
          freqs.push(freq)
          var deleteKeys = [];
          freq.forEach(function(item) {
            batchOptions.filters.forEach(function (filter) {
              _.forEach(doc[filter], function(filterKey){
                docIndexEntries.push({
                  type: 'put',
                  key: 'TF￮' + fieldName + '￮' + item[0] + '￮' + filter + '￮' + filterKey,
                  value: [doc.id]
                });
                docIndexEntries.push({
                  type: 'put',
                  key: 'RI￮' + fieldName + '￮' + item[0] + '￮' + filter + '￮' + filterKey,
                  value: [[item[1], doc.id]]
                });
              })
            });
            docIndexEntries.push({
              type: 'put',
              key: 'TF￮' + fieldName + '￮' + item[0] + '￮￮',
              value: [doc.id]
            });
            docIndexEntries.push({
              type: 'put',
              key: 'RI￮' + fieldName + '￮' + item[0] + '￮￮',
              value: [[item[1], doc.id]]
            });
          });
        });
        //generate * field



        _(freqs)
          .flatten()
          .sort()
          .reduce(function(prev, item) {
            if (!prev[0]) prev.push(item);
            else if (item[0] == _.last(prev)[0]) {
              _.last(prev)[1] = _.last(prev)[1] + item[1]
            }
            else 
              prev.push(item);
            return prev;
          }, [])
          .forEach(function(item) {
            batchOptions.filters.forEach(function (filter) {
              _.forEach(doc[filter], function(filterKey){
                docIndexEntries.push({
                  type: 'put',
                  key: 'TF￮*￮' + item[0] + '￮' + filter + '￮' + filterKey,
                  value: [doc.id]
                });
                docIndexEntries.push({
                  type: 'put',
                  key: 'RI￮*￮' + item[0] + '￮' + filter + '￮' + filterKey,
                  value: [[item[1], doc.id]]
                });
              })
            });
            docIndexEntries.push({
              type: 'put',
              key: 'TF￮*￮' + item[0] + '￮￮',
              value: [doc.id]
            });
            docIndexEntries.push({
              type: 'put',
              key: 'RI￮*￮' + item[0] + '￮￮',
              value: [[item[1], doc.id]]
            });
          });
        
        
        docIndexEntries.push({
          type: 'put',
          key: 'DELETE-DOCUMENT￮' + doc.id,
          value: _.pluck(docIndexEntries, 'key')
        });
        dbInstructions.push(docIndexEntries);
      });
      dbInstructions = _(dbInstructions)
        .flatten()
        .sortBy('key')
        .reduce(function(prev, item) {
          if (item.key.substring(0, 6) == "DELETE")
            prev.push(item)
          else if (item.key.substring(0, 8) == "DOCUMENT")
            prev.push(item)
          else if (item.key.substring(0, 2) == "RI") {
            if (item.key == _.last(prev).key)
              _.last(prev).value.push(item.value[0])
            else
              prev.push(item)
          }
          else if (item.key.substring(0, 2) == "TF") {
            if (item.key == _.last(prev).key)
              _.last(prev).value = _.last(prev).value.concat(item.value)
            else
              prev.push(item)
          }
          return prev;
        }, []);
      
//      console.log(dbInstructions)

      async.map(
        dbInstructions,
        function(item, callback) {
          indexes.get(item.key, function(err, val) {
            if ((val) && (item.key.substring(0, 2) == 'TF'))
              item.value = item.value.concat(val).sort();
            else if ((val) && (item.key.substring(0, 2) == 'RI'))
              item.value = item.value.concat(val).sort();
            return callback(null, item)
          })
        },
        function(err, mergeDbInstructions) {
//          console.log(JSON.stringify(mergeDbInstructions, null, 2))
          indexes.batch(mergeDbInstructions, function(err) {
            if (err) log.warn('Ooops!', err);
            else log.info('batch indexed!');
            return callbacky(null);
          });

        });


    })
  }
  


  return indexer;
};

