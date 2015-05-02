//deletes all references to a document from the search index

var async = require('async');

exports.deleteBatch = function (batch, index, mainCallback) {
  batch = batch.sort();
  var delBatchDeleteDocument = [];
  var delBatchDocument = [];
  var recalibrateBatchRI = {};
  var recalibrateBatchTF = {};
  for (var i = 0; i < batch.length; i++)
    delBatchDocument.push({type:'del', key: 'DOCUMENT~' + batch[i] + '~'});
  console.log('deleting ' + batch);
  async.map(batch, function(item, callback) {
    var deleteKeys = [];
    index.createReadStream({gte: 'DELETE-DOCUMENT~' + item + '~',
                            lte: 'DELETE-DOCUMENT~' + item + '~~'})
      .on('data', function (data) {
        delBatchDeleteDocument.push({type:'del', key: data.key});
        deleteKeys.push({'key': data.key, 'value': data.value});
        for (var i = 0; i < data.value.length; i++) {
          var tfKey = 'TF~' + data.value[i];
          if (!recalibrateBatchTF[tfKey]) recalibrateBatchTF[tfKey] = [];
          recalibrateBatchTF[tfKey].push(item);

          var riKey = 'RI~' + data.value[i];
          if (!recalibrateBatchRI[riKey]) recalibrateBatchRI[riKey] = [];
          recalibrateBatchRI[riKey].push(item);
        }
      })
      .on('error', function (err) {
        console.log('Oh my!', err)
      })
      .on('end', function () {
        callback(deleteKeys);
      });
  }, function(err, results) {
    async.series([
      function(callbackx) {
        index.batch(delBatchDocument, function (err) {
          if (err) return console.log('Ooops!', err);
          callbackx();
        });
      },
      function(callbackx) {
        index.batch(delBatchDeleteDocument, function (err) {
          if (err) return console.log('Ooops!', err);
          callbackx();
        });
      },
      function(callbackx) {
        async.map(
          Object.keys(recalibrateBatchRI),
          function(item, callbacky) {
            index.get(item, function(err, value) {
              var newVal = value.filter(function(arrValue) {
                return (recalibrateBatchRI[item].toString()
                        .indexOf(arrValue[1].toString()) == -1);
              });
              if (newVal.length == 0)
                index.del(item, function(err) {callbacky()});
              else
                index.put(item, newVal, function(err) {callbacky()});
            })
          },
          function(err, results) {
            callbackx();
          }
        )
      },
      function(callbackx) {
        async.map(
          Object.keys(recalibrateBatchTF),
          function(item, callbacky) {
            index.get(item, function(err, value) {
              var newVal = value.filter(function(arrValue) {
                return (recalibrateBatchTF[item].indexOf(arrValue.toString()) == -1);
              });
              if (newVal.length == 0)
                index.del(item, function(err) {callbacky()});
              else
                index.put(item, newVal, function(err) {callbacky()});
            })
          },
          function(err, results) {
            callbackx();
          }
        )
      },
      function(callbackx) {
        index.get('search-index.totalDocs', function(err, value) {
          index.put('search-index.totalDocs',
                    value - delBatchDocument.length, function(err){
                      callbackx();
                    });
        });
      },
      function(callbackx) {
        mainCallback(null);
      }
    ])
  })
}
