exports.calibrate = function(reverseIndex, callback) {
  countDocuments(reverseIndex, {}, function(indexDocData) {
    countReverseIndex(reverseIndex, indexDocData, function(newIndexMetaData) {
      indexMetaDataGlobal = newIndexMetaData;
      callback(newIndexMetaData);
    });
  });
};

countDocuments = function(reverseIndex, indexMetaData, callback) {
  var indexedFieldNamesX = [],
    totalIndexedDocsTmp = [];
  indexMetaData['totalIndexedFields'] = 0;
  indexMetaData['indexedFieldNames'] = [];
  indexMetaData['totalDocs'] = 0;
  //throwaway arrays
  console.log('calibrating...');
  reverseIndex.createReadStream({
    start: 'DOCUMENT~',
    end: 'DOCUMENT~~'})
    .on('data', function(data) {
      console.log(data.key);
      indexMetaData['totalIndexedFields']++;
      totalIndexedDocsTmp.push(data.key.split('~')[1]);
      indexedFieldNamesX[data.key.split('~')[2]] = data.key.split('~')[2];
    })
    .on('end', function() {
      for(var o in indexedFieldNamesX) {
        if (indexMetaData['indexedFieldNames'].indexOf(indexedFieldNamesX[o]) == -1) {
          indexMetaData['indexedFieldNames'].push(indexedFieldNamesX[o]);
        }
      }
      indexMetaData['totalDocs'] = arrNoDupe(totalIndexedDocsTmp).length;
      callback(indexMetaData);
    });  
};

countReverseIndex = function(reverseIndex, indexMetaData, callback) {
  var availableFacets = [];
  indexMetaData['reverseIndexSize'] = 0;
  indexMetaData['availableFacets'] = [];
  reverseIndex.createReadStream({
    start: 'REVERSEINDEX~',
    end: 'REVERSEINDEX~~'})
    .on('data', function(data) {
      console.log(data.key);
      indexMetaData['reverseIndexSize']++;
      if (data.key.split('~')[2] != 'NO') {
        availableFacets[data.key.split('~')[2]] = data.key.split('~')[2];
      }
    })
    .on('close', function() {
      for(var o in availableFacets) {
        if (indexMetaData['availableFacets'].indexOf(availableFacets[o]) == -1) {
          indexMetaData['availableFacets'].push(availableFacets[o]);
        }
      }
      callback(indexMetaData);
    }); 
};



//take an array and returns a similar array with no duplicates
function arrNoDupe(a) {
  var temp = {},
  i,
  r = [],
  k;
  for (i = 0; i < a.length; i++) {
    temp[a[i]] = true;
  }
  for (k in temp) {
    r.push(k);
  }
  return r;
}
