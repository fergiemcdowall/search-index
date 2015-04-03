var _ = require('lodash');
var async = require('async');
var searchIndexLogger = require('../logger/searchIndexLogger');

exports.calibrate = function (indexes, callback) {
  documentFrequencies(indexes, function (err, msg) {
    countDocuments(indexes, function (err, msg) {
      callback(err, msg);
    });
  });
};


exports.getTotalDocs = function (indexes, callback) {
  indexes.get('search-index.totalDocs', function (err, value) {
    if (err) {
      searchIndexLogger.info('Index is empty or not calibrated');
      callback(null, 0);
    }
    else {
      searchIndexLogger.info(value + ' documents searchable');
      callback(null, value);
    }
  });
}

exports.incrementallyCalibrate = function (indexes, tf, callback) {
  async.parallel({
    "countDocuments": function (callback) {
//      searchIndexLogger.info('counting documents');
      countDocuments(indexes, function (err, msg) {
        callback(null);
      });
    },
    "reducedTF": function (callback) {

      var keys = _.keys(tf['reducedTF']);

      async.map(keys, function (key, callback) {
        var rTF = tf['reducedTF'][key];
        indexes.get(key, function (err, value) {
          if (!err)
            rTF = rTF.concat(value);

          rTF = rTF.sort();
          callback(null, {key: key, rTF: rTF});
        });
      }, function (err, result) {

        var batch = _.reduce(result, function (batch, item) {
          return batch.put(item.key, item.rTF);
        }, indexes.batch())

        batch.write(function () {
          callback(null);
        })
      });
    },
    "reducedTFSortOnTF": function (callback) {
      var keys = _.keys(tf['reducedTFSortOnTF']);
      async.map(keys, function (key, callback) {
        var rTF = tf['reducedTFSortOnTF'][key];
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
        }, indexes.batch())

        batch.write(function () {
          callback(null);
        })
      });
    }
  },
  function (err, result) {
    if (err) {
      searchIndexLogger.error(err.toString());
      return callback(err)
    }
//    searchIndexLogger.info('[success] incremental calibration complete')
    callback(null)
  });
}


countDocuments = function (indexes, callback) {
  var tally = 0;
  indexes.createReadStream({
    start: 'DOCUMENT~',
    end: 'DOCUMENT~~'
  })
  .on('data', function (data) {
    tally++;
  })
  .on('end', function () {
    indexes.put('search-index.totalDocs', tally, function () {
      callback(null, 'calibrated ' + tally + ' docs');
    });
  });
};


documentFrequencies = function (indexes, callback) {
  var lastToken = '~';
  var tally = 1;
  var progressCounter = 0;
  indexes.createReadStream({
    start: 'REVERSEINDEX~',
    end: 'REVERSEINDEX~~'
  })
    .on('data', function (data) {
      var splitKey = data.key.split('~');
      //calibrate only the nonfaceted * fields
      if ((splitKey[4] == '*') && (splitKey[2] == '')) {
        var token = 'TF~' + splitKey.slice(1, 5).join('~');
        if (token != lastToken) {
          searchIndexLogger.info(lastToken + ' : ' + tally);
          progressCounter++;
          if (progressCounter % 1000 == 0)
            searchIndexLogger.info('calibrated ' + progressCounter + ' tokens');
          indexes.put(lastToken, tally);
          lastToken = token;
          tally = 1;
        }
        else {
          tally++;
        }
      }
    })
    .on('close', function () {
      callback(null);
    });
}

