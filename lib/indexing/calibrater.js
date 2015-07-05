var _ = require('lodash');
var async = require('async');
var indexOptions = null;

exports.setIndexOptions = function (ops) {
  indexOptions = ops;
};

exports.getTotalDocs = function (indexes, callback) {
  indexes.get('search-index.totalDocs', function (err, value) {
    if (err) {
      indexOptions.log.info('Index is empty or not calibrated');
      callback(null, 0);
    }
    else {
      indexOptions.log.info(value + ' documents searchable');
      callback(null, value);
    }
  });
};

exports.incrementallyCalibrate = function (indexes, tf, callback) {
  indexOptions.log.info('calibrating...');
  async.parallel({
    countDocuments: function (callback) {
      countDocuments(indexes, function (err) {
        callback(err);
      });
    },
    reducedTF: function (callback) {
      var keys = _.keys(tf.reducedTF);
      async.map(keys, function (key, callback) {
        var rTF = tf.reducedTF[key];
        indexes.get(key, function (err, value) {
          if (!err)
            rTF = rTF.concat(value);
          rTF = rTF.sort();
          callback(null, {key: key, rTF: rTF});
        });
      }, function (err, result) {
        var batch = _.reduce(result, function (batch, item) {
          return batch.put(item.key, item.rTF);
        }, indexes.batch());
        batch.write(function () {
          callback(err);
        });
      });
    },
    reducedTFSortOnTF: function (callback) {
      var keys = _.keys(tf.reducedTFSortOnTF);
      async.map(keys, function (key, callback) {
        var rTF = tf.reducedTFSortOnTF[key];
        indexes.get(key, function (err, value) {
          if (!err)
            rTF = rTF.concat(value);

          rTF = rTF.sort(
            function (a, b) {
              if (a[0] < b[0])
                return 1;
              if (a[0] > b[0])
                return -1;
              return 0;
            }
          );
          callback(null, {key: key, rTF: rTF});
        });
      }, function (err, result) {
        var batch = _.reduce(result, function (batch, item) {
          return batch.put(item.key, item.rTF);
        }, indexes.batch());
        batch.write(function () {
          callback(null);
        });
      });
    }
  }, function (err, result) {
    if (err) {
      indexOptions.log.error(err.toString());
      return callback(err, null);
    }
    callback(null, result);
  });
};

var countDocuments = function (indexes, callback) {
  var tally = 0;
  indexes.createReadStream({
    start: 'DOCUMENT~',
    end: 'DOCUMENT~~'
  })
  .on('data', function () {
    tally++;
  })
  .on('end', function () {
    indexes.put('search-index.totalDocs', tally, function () {
      callback(null, 'calibrated ' + tally + ' docs');
    });
  });
};
