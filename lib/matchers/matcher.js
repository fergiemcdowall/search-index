var skeleton = require('log-skeleton');
var H = require('highland');
var _ = require('lodash');

module.exports = function (options) {
  var log = skeleton((options) ? options.log : undefined);
  var matcher = {};

  matcher.matcher = function (reverseIndex, ops, callback) {

    if (!_.isPlainObject(ops)) {
      try {
        ops = JSON.parse(ops);
      }
      catch (e) {
        return callback(new Error('Options should be an object'), []);
      }
    }

    var opsDefaults = {
      beginsWith: '',
      field: '*',
      threshold: 3,
      limit: 10,
      type: 'simple'
    };

    ops = _.defaults(ops || {}, opsDefaults);

    if (ops.beginsWith.length === 0)
      return callback(new Error('match string can not be empty'), []);

    if (ops.beginsWith.length < ops.threshold)
      return callback(new Error('match string must be longer than threshold (' +
                                ops.threshold + ')'), []);

    H(reverseIndex.createReadStream({
      start: 'TF￮' + ops.field + '￮' + ops.beginsWith,
      end: 'TF￮' + ops.field + '￮' + ops.beginsWith + '￮￮￮'
    }))
    .on('error', function (err) {
      log.error('Oh my!', err);
    })
    .filter(function (data) {
      return data.key.substring(data.key.length, data.key.length - 2) == '￮￮';
    })
    .map(function (data) {
      return [data.key.split('￮')[2], data.value]; //suggestions
    })
    .sortBy(function (a, b) {
      return b[1].length - a[1].length; //sortedSuggestions
    })
    .map(function (data) {
      if (ops.type == 'ID')
        return data;
      if (ops.type == 'count')
        return [data[0], data[1].length];
      return data[0]; //fall back to a simple format
    })
    .take(ops.limit)
    .collect()
    .pull(callback);
  };

  return matcher;
};
