const levelup = require('levelup')
const SearchIndex = require('../../')

var batch = require('../../node_modules/reuters-21578-json/data/justTen/justTen.json')
levelup('test/sandbox/simpleIndexing', {
  valueEncoding: 'json',
  db: require('level-js')
}, function (err, db) {
  if (err) console.log(err)
  SearchIndex({indexes: db}, function (err, si) { // causes woe in browsers
    if (err) console.log(err)
    si.add(batch, {}, function (err) {
      if (err) console.log(err)
      si.get('4', function (err, doc) {
        if (err) console.log(err)
        document.getElementById('result').innerHTML = doc.title
        console.log(doc)
      })
    })
  })
})
