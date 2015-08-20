var skeleton = require('log-skeleton');
var H = require('highland');
var _ = require('lodash');

module.exports = function (options) {
  var log = skeleton((options) ? options.log : undefined);
  var matcher = {};

  matcher.matcher = function (reverseIndex, ops, callback) {

    if (!_.isPlainObject(ops))
      return callback(new Error('Options should be an object'), []);

    var opsDefaults = {
      beginsWith: "",
      field: "*",
      threshold: 3
    }

    ops = _.defaults(ops || {}, opsDefaults);

    if (ops.beginsWith.length === 0)
      return callback(new Error('match string can not be empty'), []);

    if (ops.beginsWith.length < ops.threshold)
      return callback(new Error('match string must be longer than threshold ('
                                + ops.threshold + ')'), []);

    H(reverseIndex.createReadStream({
      start: 'TF￮' + ops.field + '￮' + ops.beginsWith,
      end: 'TF￮' + ops.field + '￮' + ops.beginsWith + '￮￮￮'
    }))
    .on('error', function (err) {
      log.error('Oh my!', err);
    })
    .filter(function(data){
      return data.key.substring(data.key.length, data.key.length - 2) == '￮￮';
    })
    .map(function(data){
      return [data.key.split('￮')[2], data.value.length]; //suggestions
    })
    .sortBy(function(a, b){
      return b[1] - a[1]; //sortedSuggestions
    })
    .map(function(data){
      return data[0]; //simpleSortedSuggestions
    })
    .take(10)
    .collect()
    .pull(callback);
  };

  return matcher;
};
