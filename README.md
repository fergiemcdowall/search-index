# search-index

### A network resilient, persistent full-text search library for the browser and Node.js

[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg?style=flat-square)](https://gitter.im/fergiemcdowall/search-index)
[![npm](https://img.shields.io/npm/v/search-index.svg?style=flat-square)](https://www.npmjs.com/package/search-index)
[![npm](https://img.shields.io/npm/dm/search-index.svg?style=flat-square)](https://npm-stat.com/charts.html?package=search-index)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg?style=flat-square)](LICENCE)
[![Travis](https://img.shields.io/travis/rust-lang/rust.svg?style=flat-square)](https://travis-ci.org/fergiemcdowall/search-index)
[![JavaScript Style Guide](https://img.shields.io/badge/code_style-standard-brightgreen.svg?style=flat-square)](https://standardjs.com)


## Getting started

### Initialise and populate an index

#### Default method

`search-index` can be invoked with ES6 `import` or commonjs `require`
using either lazy loading or a callback:

```javascript
// Make a new index, or open an existing one with this name
import si from 'search-index'

// "lazy load"- db may not be immediately initialized
db = si({ name: 'mySearchIndex' })

// db exists in a leveldb instance if run on a server, and an
// indexedDB instance if run in a browser
db.PUT([ /* my array of objects */ ]).then(doStuff)

```

#### Script tag method

In the `/dist` folder there is a file called
`search-index.<version>.js` that can be used as a standalone in a
`<script>` tag. The library is then available under a global variable
called `searchIndex`:

```html
<script type='text/javascript' src='./search-index.1.0.2.js'></script>
<script type='text/javascript'>
  searchIndex({ name: 'myDB' }, (err, db) => {
    // db is now available
  })
</script>

```


### Search the index

```javascript

// (given objects that contain: { land: <land>, colour: <colour>, population: <number> ... })

const { SEARCH, OR } = db

// get all objects where land=SCOTLAND and colour=GREEN
SEARCH('land:SCOTLAND', 'colour:GREEN').then(result)
// (^result is a result set scored and ranked with tfidf)

// you can look as deeply as you want
SEARCH('metadata.land.fullname:SCOTLAND', 'metadata.colour:GREEN').then(result)

// or search for terms without specifing any fields
SEARCH('SCOTLAND', 'GREEN').then(result)

// or combine with boolean expressions (see below)
SEARCH(
  OR('SCOTLAND', 'IRELAND'),
  'GREEN'
).then(result)
// (these queries can be as deeply nested as required)
```


### Query the index using boolean expressions (AND, OR, NOT)

```javascript

const { AND, DOCUMENTS, NOT, OR } = db

// AND returns a set of IDs with matched properties
AND('land:SCOTLAND', 'colour:GREEN').then(result)

// as above, but returning the whole document
AND('land:SCOTLAND', 'colour:GREEN').then(DOCUMENTS).then(result)

// either land:SCOTLAND OR land:IRELAND
OR('land:SCOTLAND', 'land:IRELAND').then(result)

// queries can be embedded within each other
AND(
  'land:SCOTLAND',
  OR('colour:GREEN', 'colour:BLUE')
).then(result)

// get all object IDs where land=SCOTLAND and colour is NOT GREEN
NOT(
  'land:SCOTLAND',                      // everything in this set
  AND('colour:GREEN', 'colour:RED').    // minus everything in this set
).then(result)

```





### More examples

(See the [tests](https://github.com/fergiemcdowall/search-index/tree/master/test) for more examples.)
