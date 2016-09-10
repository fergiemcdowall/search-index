const levelup = require('levelup')
const SearchIndex = require('../../')
const Readable = require('stream').Readable
const JSONStream = require('JSONStream')
const batch = require('../../node_modules/reuters-21578-json/data/justTen/justTen.json')

var s = new Readable()
batch.forEach(function (datum) {
  s.push(JSON.stringify(datum))
})
s.push(null)


levelup('test/sandbox/simpleIndexing', {
  valueEncoding: 'json',
  db: require('level-js')
}, function (err, db) {
  if (err) console.log(err)
  SearchIndex({
    indexes: db
  }, function (err, si) { // causes woe in browsers
    if (err) console.log(err)
    s.pipe(JSONStream.parse())
      .pipe(si.defaultPipeline())
      .pipe(si.add())
      .on('data', function(data) {
        
      }).on('end', function() {
        si.get('4', function (err, doc) {
          if (err) console.log(err)
          document.getElementById('result').innerHTML = doc.title
          console.log(doc)
        })
      })
  })
})
