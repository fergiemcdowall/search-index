/*jshint -W083 */ //makes jslint overlook functions in lodash for-loops

var _ = require('lodash');
var async = require('async');
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

  indexer.addBatchToIndex = function (indexes, batch, batchOptions, callbacky) {
    var salt = 0;
    if (!_.isArray(batch) && _.isPlainObject(batch)) {
      batch = [batch];
    }
    //generate IDs if none are present and stringify numeric IDs
    batch.map(function (doc) {
      if (!doc.id)
        doc.id = (++salt) + '-' + hash(doc);
      doc.id = doc.id + ''; // stringify ID
    });
    deleter.deleteBatch(_.pluck(batch, 'id'), indexes, function (err) {
      var dbInstructions = [];
      batch.forEach(function (doc) {
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
        var freqsForComposite = []; //put document frequencies in here
        _.forEach(doc, function (field, fieldName) {
          var fieldOptions = _.defaults(_.find(batchOptions.fieldOptions, 'fieldName', fieldName) || {}, batchOptions.defaultFieldOptions);
          if (fieldName == 'id') fieldOptions.stopwords = '';   // because you cant run stopwords on id field
          else fieldOptions.stopwords = batchOptions.stopwords;
          if (_.isArray(field)) field = field.join(' '); // make filter fields searchable
          var v = tv.getVector(field + '', {
            separator: batchOptions.separator,
            stopwords: fieldOptions.stopwords,
            nGramLength: fieldOptions.nGramLength
          });
          v.push(['*', 1]); //can do wildcard searh on this field

          var freq = tf.getTermFrequency(v, {
            scheme: 'doubleLogNormalization0.5',
            weight: fieldOptions.weight
          });
          if (fieldOptions.searchable)
            freqsForComposite.push(freq);
          var deleteKeys = [];
          if (fieldOptions.fieldedSearch) {
            freq.forEach(function (item) {
              batchOptions.filters.forEach(function (filter) {
                _.forEach(doc[filter], function (filterKey) {
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
                });
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
          };
        });
        //generate * field
        _(freqsForComposite)
          .flatten()
          .sort()
          .reduce(function (prev, item) {
            if (!prev[0]) prev.push(item);
            else if (item[0] == _.last(prev)[0]) {
              _.last(prev)[1] = _.last(prev)[1] + item[1];
            }
            else
              prev.push(item);
            return prev;
          }, [])
          .forEach(function (item) {
            batchOptions.filters.forEach(function (filter) {
              _.forEach(doc[filter], function (filterKey) {
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
              });
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
      dbInstructions.push({
        type: 'put',
        key: 'DOCUMENT-COUNT',
        value: batch.length
      });
      dbInstructions = _(dbInstructions)
        .flatten()
        .sortBy('key')
        .reduce(function (prev, item) {
          if (item.key.substring(0, 6) == 'DELETE')
            prev.push(item);
          else if (item.key.substring(0, 8) == 'DOCUMENT')
            prev.push(item);
          else if (item.key.substring(0, 2) == 'RI') {
            if (item.key == _.last(prev).key)
              _.last(prev).value.push(item.value[0]);
            else
              prev.push(item);
          }
          else if (item.key.substring(0, 2) == 'TF') {
            if (item.key == _.last(prev).key)
              _.last(prev).value = _.last(prev).value.concat(item.value);
            else
              prev.push(item);
          }
          return prev;
        }, []);
      async.eachLimit(
        dbInstructions, 5,
        function (item, callback) {
          indexes.get(item.key, function (err, val) {
            if (item.key.substring(0, 2) == 'TF') {
              if (val)
                item.value = item.value.concat(val);
              item.value = item.value.sort();
            }
            else if (item.key.substring(0, 2) == 'RI') {
              if (val)
                item.value = item.value.concat(val);
              item.value = item.value.sort(function (a, b) {
                return b[0] - a[0];
              });
            }
            else if (item.key == 'DOCUMENT-COUNT') {
              if (val)
                item.value = +val + +(item.value);
            }
            return callback(null);
          });
        },
        function (err) {
          //          console.log(JSON.stringify(mergeDbInstructions, null, 2))
          indexes.batch(dbInstructions, function (err) {
            if (err) log.warn('Ooops!', err);
            else log.info('batch indexed!');
            return callbacky(null);
          });
        });
    });
  };
  return indexer;
};

