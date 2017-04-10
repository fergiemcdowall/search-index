# search-index quickstart

So you want to set up a search index in Javascript? Here is one way to
do it. You can of course make many variations on this theme- consult
the rest of the documentation for options and alternatives.

## Step 1:

Find some data in the right format. You could for example index 1000
old Reuters articles, [like the ones found here](https://raw.githubusercontent.com/fergiemcdowall/reuters-21578-json/master/data/fullFileStream/000.str)

## Step 2:

Initialize a search index and add the data

```javascript
const indexData = function(err, newIndex) {
  if (!err) {
    index = newIndex
    request(url)                      // get a stream of documents
      .pipe(JSONStream.parse())       // turn into JSON
      .pipe(index.defaultPipeline())  // vectorize
      .pipe(index.add())              // index
      .on('data', function(data) {})
  }
}
require('search-index')(ops, indexData)
```

## Step 3:

Run a search query

```javascript
index.search({
  query: [{
    AND: {
      '*': ['search', 'words']   // search for "search" and "words" in all ("*") fields
    }
  }]
}).on('data', printResults)      // make pretty results
```


## Example app:

Save this file as `index.js`, do `npm install JSONStream chalk request term-cluster`, and run it as `node index.js` to get a really basic CLI search engine for some old Reuters articles.

```javascript
const JSONStream = require('JSONStream')
const chalk = require('chalk')
const request = require('request')
const tc = require('term-cluster')
const url = 'https://raw.githubusercontent.com/fergiemcdowall/reuters-21578-json/master/data/fullFileStream/justTen.str'

const ops = {
  indexPath: 'myCoolIndex',
  logLevel: 'error'
}

var index

const indexData = function(err, newIndex) {
  if (!err) {
    index = newIndex
    request(url)
      .pipe(JSONStream.parse())
      .pipe(index.defaultPipeline())
      .pipe(index.add())
      .on('data', function(data) {})
      .on('end', searchCLI)
  }
}

const printPrompt = function () {
  console.log()
  console.log()
  process.stdout.write('search > ')
}

const searchCLI = function () {
  printPrompt()
  process.stdin.resume()
  process.stdin.on('data', search)
}

const search = function(rawQuery) {
  index.search(rawQuery.toString().replace( /\r?\n|\r/g, '' ))
    .on('data', printResults)
    .on('end', printPrompt)
}

const printResults = function (data) {
  console.log('\n' + chalk.blue(data.document.id) + ' : ' + chalk.blue(data.document.title))
  const terms = Object.keys(data.scoringCriteria[0].df).map(function(item) {
    return item.substring(2)
  })  
  for (var key in data.document) {
    if (data.document[key]) {
      var teaser = tc(data.document[key], terms)
      if (teaser) console.log(teaser)
    }
  }
  console.log()
}

require('search-index')(ops, indexData)

```
