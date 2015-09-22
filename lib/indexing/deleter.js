//deletes all references to a document from the search index

var skeleton = require('log-skeleton');
var a = require('async');
var _ = require('lodash');

module.exports = function (options) {
  var log = skeleton((options) ? options.log : undefined);
  var deleter = {};

  deleter.deleteBatch = function (deleteBatch, indexes, callbacky) {
    var numberDocsToDelete = 0;
    a.map(deleteBatch, function (docID, callback) {
      indexes.get('DELETE-DOCUMENT￮' + docID, function (err, keys) {
        if (err) {
          if (err.name == 'NotFoundError') {
            return callback(null, []);
          }
        }
        else
          numberDocsToDelete++;
        return callback(err, keys);
      });
    }, function (err, results) {
      if (!err) {
        a.map(_(results).flatten().sort().uniq(true).value(),
              function (key, callback) {
                indexes.get(key, function (err, value) {
                  var dbInstruction = {type: 'put',
                                       key: key};
                  var recalibratedValue = [];
                  if (key.substring(0, 2) == 'TF') {
                    recalibratedValue = _.remove(value, function (n) {
                      return (deleteBatch.indexOf(n + '') == -1);
                    }).sort();
                  }
                  else if (key.substring(0, 2) == 'RI') {
                    recalibratedValue = value.filter(function (item) {
                      return (deleteBatch.indexOf(item[1]) == -1);
                    }).sort(function (a, b) {
                      return b[0] - a[0];
                    });
                  }
                  if (recalibratedValue.length == 0)
                    dbInstruction.type = 'del';
                  else
                    dbInstruction.value = recalibratedValue;
                  callback(err, dbInstruction);
                });
              }, function (err, dbInstructions) {
                deleteBatch.forEach(function (docID) {
                  dbInstructions.push({
                    type: 'del',
                    key: 'DELETE-DOCUMENT￮' + docID
                  });
                });
                indexes.batch(dbInstructions, function (err) {
                  if (err) log.warn('Ooops!', err);
                  else log.info('batch indexed!');
                  // make undefined error null
                  if (_.isUndefined(err)) err = null;
                  indexes.get('DOCUMENT-COUNT', function (err, value) {
                    indexes.put('DOCUMENT-COUNT',
                                (+value - (numberDocsToDelete)),
                                function (errr) {
                                  return callbacky(err);
                                });
                  });
                });
              });
      }
      else return callbacky(err);
    });
  };

  return deleter;
};
