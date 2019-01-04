# search-index

### A network resilient, persistent full-text search library for the browser and Node.js

[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg?style=flat-square)](https://gitter.im/fergiemcdowall/search-index)
[![npm](https://img.shields.io/npm/v/search-index.svg?style=flat-square)](https://www.npmjs.com/package/search-index)
[![npm](https://img.shields.io/npm/dm/search-index.svg?style=flat-square)](https://npm-stat.com/charts.html?package=search-index)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg?style=flat-square)](LICENCE)
[![Travis](https://img.shields.io/travis/rust-lang/rust.svg?style=flat-square)](https://travis-ci.org/fergiemcdowall/search-index)
[![JavaScript Style Guide](https://img.shields.io/badge/code_style-standard-brightgreen.svg?style=flat-square)](https://standardjs.com)


## API

Command     |  Accepts    | Returns    | Writes | Description
----------- |  ---------- | ---------- | ------ | -----------
`AGGREGATE` |             |            | no     | Can create aggregations by intersecting an array of BUCKETs with a query
`AND`       |  properties | ids        | no     | Boolean AND. Return IDs of objects that have prop.A AND prop.B
`BUCKET`    |  properties | ids        | no     | Get all document ids in this namespace
`DELETE`    |  ids        | ids        | yes    | Delete a set of documents
`DICTIONARY`|  field name | words      | no     | Return every word in specified field (or entire index if no field specified)
`DISTINCT`  |  properties | properties | no     | Gets a list of available properties in a space
`DOCUMENTS` |  ids        | objects    | no     | Get an object by its ID
`GET`       |  properties | ids        | no     | Get the IDs of objects with a property in the given range
`INDEX`     |  -          | index      | both   | Get the underlying [index](https://github.com/fergiemcdowall/fergies-inverted-index/).
`NOT`       |  ids        | ids        | no     | Get all IDs of objects in set A that are not in set B
`OR`        |  properties | ids        | no     | Boolean OR. Return IDs of objects that have either prop.A OR prop.b
`PUT`       |  objects    | ids        | yes    | Add objects to index
`SEARCH`    |  properties | objects    | no     | Equivalent to AND followed by DOCUMENTS

## Getting started

### Initialise and populate an index

```javascript

// Make a new index, or open an existing one with this name
const si = require('search-index')

// EITHER:
idx = si({ name: 'idx' }) // "lazy load"- idx may not be immediately initialized
// some time later...
idx.PUT([ /* my array of objects */ ]).then(doStuff)

// OR:
si(ops, (err, idx) => {
  // idx is always open and available
  idx.PUT([ /* my array of objects */ ]).then(doStuff)
})

```

### Search the index

```javascript

// (given objects that contain: { land: <land>, colour: <colour>, population: <number> ... })

const { SEARCH, OR } = idx

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

const { AND, NOT, OR } = idx

// AND returns a set of IDs with matched properties
AND('land:SCOTLAND', 'colour:GREEN').then(result)

// as above, but returning the whole document
AND('land:SCOTLAND', 'colour:GREEN').then(idx.DOCUMENTS).then(result)

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

### Aggregation




(See the [tests](https://github.com/fergiemcdowall/search-index/tree/master/test) for more examples.)
