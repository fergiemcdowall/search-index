exports.calibrate = function(reverseIndex, docFreqIndex, callback) {
  documentFrequencies(reverseIndex, docFreqIndex, function(msg) {
    countDocuments(reverseIndex, docFreqIndex, function(msg) {
      callback(msg);
    });
  });
};

countDocuments = function(reverseIndex, docFreqIndex, callback) {
  var tally = 0;
  reverseIndex.createReadStream({
    start: 'DOCUMENT~',
    end: 'DOCUMENT~~'})
    .on('data', function(data) {
      tally++;
    })
    .on('end', function() {
      docFreqIndex.put('forage.totalDocs', tally, function(){
        console.log('totalDocs: ' + tally);
      });
    });  
};


documentFrequencies = function(reverseIndex, docFreqIndex, callback) {
  var lastToken = '~';
  var tally = 1;
  var progressCounter = 0;
  reverseIndex.createReadStream({
    start: 'REVERSEINDEX~',
    end: 'REVERSEINDEX~~'})
    .on('data', function(data) {
      if (data.key.split('~')[4] == 'forage.composite') {
        var token = data.key.split('~')[1];
        if (token != lastToken) {
          progressCounter++;
          if (progressCounter % 1000 == 0)
            console.log('calibrated ' + progressCounter + ' tokens');
          docFreqIndex.put(lastToken, tally);
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

