var logger = require('../logger.js');

exports.calibrate = function(indexes, callback) {
  documentFrequencies(indexes, function(msg) {
    countDocuments(indexes, function(msg) {
      callback(msg);
    });
  });
};

exports.getTotalDocs = function(indexes, callback) {
  indexes.get('forage.totalDocs', function(err, value){
    if (err) {
      logger.info('Index is empty or not calibrated, see https://github.com/fergiemcdowall/forage#indexing-api');
      callback(0);
    }
    else {
      logger.info(value + ' documents searchable');
      callback(value);
    }
  });
}

exports.incrementallyCalibrate = function(indexesMultiply, tf, callback) {
  indexesMultiply.get(Object.keys(tf), function(err, data) {
    recalculatedTF = [];
    for (var k in data) {
      if (data[k] != null)
        tf[k] = parseInt(data[k]) + 1);
    }
    countDocuments(indexesMultiply, function(msg) {
      indexesMultiply.put(tf, function(err){
        logger.info(tf);
        callback('[success] incremental calibration complete')
      });
    });
  })
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
      indexes.put('forage.totalDocs', tally, function(){
//        logger.debug('totalDocs: ' + tally);
        callback('calibrated ' + tally + ' docs');
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
          logger.info(lastToken + ' : ' + tally);
          progressCounter++;
          if (progressCounter % 1000 == 0)
            logger.info('calibrated ' + progressCounter + ' tokens');
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
      callback('done');
    });  
}

