
exports.calibrate = function(indexes, callback) {
  documentFrequencies(indexes, function(err, msg) {
    countDocuments(indexes, function(err, msg) {
      callback(err, msg);
    });
  });
};

exports.getTotalDocs = function(indexes, callback) {
  indexes.get('search-index.totalDocs', function(err, value){
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

exports.incrementallyCalibrate = function(indexesMultiply, tf, callback) {

  indexesMultiply.get(Object.keys(tf['reducedTF']), function(err, data) {
    searchIndexLogger.info('growing tf sets');
    for (var k in data) {
      if (data[k] != null) {
        tf['reducedTF'][k] = tf['reducedTF'][k].concat(data[k]);
      }
    }
    searchIndexLogger.info('counting documents');
    countDocuments(indexesMultiply, function(err, msg) {
      searchIndexLogger.info('sorting tf sets');
      //resort all keys
      for (var k in tf['reducedTF']) {
        tf['reducedTF'][k] = tf['reducedTF'][k].sort();
      }
      searchIndexLogger.info('reinserting tf sets');
      indexesMultiply.put(tf['reducedTF'], function(err){
        searchIndexLogger.info('TF reduced');


        indexesMultiply.get(Object.keys(tf['reducedTFSortOnTF']), function(err, data) {
          searchIndexLogger.info('growing tf sets for reverseindex');
          for (var k in data) {
            if (data[k] != null) {
              tf['reducedTFSortOnTF'][k] = tf['reducedTFSortOnTF'][k].concat(data[k]);
            }
          }
          searchIndexLogger.info('counting documents');
          countDocuments(indexesMultiply, function(err, msg) {
            searchIndexLogger.info('sorting tf sets');
            //resort all keys
            for (var k in tf['reducedTFSortOnTF']) {
              tf['reducedTFSortOnTF'][k] = tf['reducedTFSortOnTF'][k].sort(
                function(a,b) {
                  if (a[0] < b[0])
                    return 1;
                  if (a[0] > b[0])
                    return -1;
                  return 0;
                }
              );
            }
            searchIndexLogger.info('reinserting tf sets');
            indexesMultiply.put(tf['reducedTFSortOnTF'], function(err){
              callback(null, '[success] incremental calibration complete')
            });
          });
        });
        

      });
    });    
  });


}


countDocuments = function(indexes, callback) {
  var tally = 0;
  indexes.createReadStream({
    start: 'DOCUMENT~',
    end: 'DOCUMENT~~'})
    .on('data', function(data) {
      tally++;
    })
    .on('end', function() {
      indexes.put('search-index.totalDocs', tally, function(){
        callback(null, 'calibrated ' + tally + ' docs');
      });
    });  
};



documentFrequencies = function(indexes, callback) {
  var lastToken = '~';
  var tally = 1;
  var progressCounter = 0;
  indexes.createReadStream({
    start: 'REVERSEINDEX~',
    end: 'REVERSEINDEX~~'})
    .on('data', function(data) {
      var splitKey = data.key.split('~');
      //calibrate only the nonfaceted * fields
      if ((splitKey[4] == '*') && (splitKey[2] == '')) {
        var token = 'TF~' + splitKey.slice(1,5).join('~');
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
    .on('close', function() {
      callback(null, 'done');
    });  
}

