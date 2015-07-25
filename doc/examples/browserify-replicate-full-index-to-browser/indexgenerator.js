var dataset = require('../../node_modules/reuters-21578-json/data/justTen/justTen.json');
var fs = require('fs');
var si = require('../../')({indexPath: 'testindex2'});

si.add({'batchName': 'reuters'}, dataset, function(err) {
  console.log('creating snapshot...');
  si.snapShotBatch(function(readStream) {
    readStream.pipe(fs.createWriteStream('backup.gz'))
      .on('close', function() {
        console.log('snapshot completed');
      });
  });
});

