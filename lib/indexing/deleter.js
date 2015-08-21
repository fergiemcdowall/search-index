//deletes all references to a document from the search index

var skeleton = require('log-skeleton');
var H = require('highland');
var transaction = require('level-transactions');

module.exports = function (options) {
  var log = skeleton((options) ? options.log : undefined);
  var deleter = {};

  deleter.deleteBatch = function (batch, index, callback) {
    var tx = transaction(index);

    var recalibrateBatchRI = {};
    var recalibrateBatchTF = {};
    log.info('deleting ' + batch);

    H(batch)
    .sort()
    .map(function (item) {
      tx.del('DOCUMENT￮' + item + '￮');
      var deleteKeys = [];

      return H(index.createReadStream({
        gte: 'DELETE-DOCUMENT￮' + item + '￮',
        lte: 'DELETE-DOCUMENT￮' + item + '￮￮'
      }))
      .map(function (data) {
        tx.del(data.key);
        deleteKeys.push({key: data.key, value: data.value});
        for (var i = 0; i < data.value.length; i++) {
          var tfKey = 'TF￮' + data.value[i];
          if (!recalibrateBatchTF[tfKey]) recalibrateBatchTF[tfKey] = [];
          recalibrateBatchTF[tfKey].push(item);

          var riKey = 'RI￮' + data.value[i];
          if (!recalibrateBatchRI[riKey]) recalibrateBatchRI[riKey] = [];
          recalibrateBatchRI[riKey].push(item);
        }
        return data;
      });
    })
    .series()
    .on('error', function (err) {
      log.err('Ooops!', err);
      callback(err);
    })
    .done(function () {
      Object.keys(recalibrateBatchRI).forEach(function (item) {
        tx.get(item, function (err, value) {
          var newVal = value.filter(function (arrValue) {
            return (recalibrateBatchRI[item].toString()
                    .indexOf(arrValue[1].toString()) == -1);
          });
          if (newVal.length === 0) tx.del(item);
          else tx.put(item, newVal);
        });
      });
      Object.keys(recalibrateBatchTF).forEach(function (item) {
        tx.get(item, function (err, value) {
          var newVal = value.filter(function (arrValue) {
            return (recalibrateBatchTF[item].indexOf(arrValue.toString()) == -1);
          });
          if (newVal.length === 0) tx.del(item);
          else tx.put(item, newVal);
        });
      });
      tx.get('search-index.totalDocs', function (err, value) {
        tx.put('search-index.totalDocs', value - batch.length);
      });
      tx.commit(callback);
    });
  };
  return deleter;
};
