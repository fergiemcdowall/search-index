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

```javascript

// Make a new index, or open an existing one with this name
import si from 'search-index'

db = si({ name: 'mySearchIndex' }) // "lazy load"- db may not be immediately initialized

db.PUT([ /* my array of objects */ ]).then(doStuff)

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


## API

- <a href="#si"><code><b>si()</b></code></a>
- <a href="#AND"><code>db.<b>AND()</b></code></a>
- <a href="#BUCKET"><code>db.<b>BUCKET()</b></code></a>
- <a href="#BUCKETFILTER"><code>db.<b>BUCKETFILTER()</b></code></a>
- <a href="#DELETE"><code>db.<b>DELETE()</b></code></a>
- <a href="#DICTIONARY"><code>db.<b>DICTIONARY()</b></code></a>
- <a href="#DISTINCT"><code>db.<b>DISTINCT()</b></code></a>
- <a href="#DOCUMENTS"><code>db.<b>DOCUMENTS()</b></code></a>
- <a href="#GET"><code>db.<b>GET()</b></code></a>
- <a href="#INDEX"><code>db.<b>INDEX</b></code></a>
- <a href="#NOT"><code>db.<b>NOT()</b></code></a>
- <a href="#OR"><code>db.<b>OR()</b></code></a>
- <a href="#PUT"><code>db.<b>PUT()</b></code></a>
- <a href="#SEARCH"><code>db.<b>SEARCH()</b></code></a>


<a name="si"></a>

### `si([options[, callback]])`

```javascript
import si from 'search-index'

// creates a DB called "myDB" using levelDB (node.js), or indexedDB (browser)
const db = si({ name: 'myDB' })
```

In some cases you will want to start operating on the database
instentaneously. In these cases you can wait for the callback:

```javascript
import si from 'search-index'

// creates a DB called "myDB" using levelDB (node.js), or indexedDB (browser)
si({ name: 'myDB' }, (err, db) => {
  // db is guaranteed to be open and available
})
```


<a name="AND"></a>

### `db.AND([ ...Promise ]).then(result)`

Boolean AND. Return IDs of objects that have prop.A AND prop.B


<a name="BUCKET"></a>

### `db.BUCKET([ ...Promise ]).then(result)`

Return IDs of objects that match the query


<a name="BUCKETFILTER"></a>

### `db.BUCKETFILTER([ ...bucket ], filter query).then(result)`

The first argument is an array of buckets, the second is an expression
that filters each bucket


<a name="DELETE"></a>

### `db.DELETE([ ...Promise ]).then(result)`

Deletes all objects by ID


<a name="DICTIONARY"></a>

### `db.DICTIONARY(options).then(result)`

Options:

* gte : greater than or equal to
* lte : less than or equal to

Gets an array of tokens stored in the index.


<a name="DISTINCT"></a>

### `db.DISTINCT(options).then(result)`

`db.DISTINCT` returns every value in the db that is greater than equal
to `gte` and less than or equal to `lte` (sorted alphabetically)

For example- get all names between `h` and `l`:

```javascript
db.DISTINCT({ gte: 'h', lte: 'l' }).then(result)
```

<a name="DOCUMENTS"></a>

### `db.DOCUMENTS([ ...id ]).then(result)`

Get documents by ID


<a name="GET"></a>

### `db.GET(property).then(result)`

`db.GET` returns all object ids for objects that contain the given
property, aggregated by object id.

For example get all names between `h` and `l`:

```javascript
db.GET({ gte: 'h', lte: 'l' }).then(result)
```

Or to get all objects that have a `name` property that begins with 'h'

```javascript
db.GET('h').then(result)
```


<a name="INDEX"></a>

### `db.INDEX`

Points to the underlying [index](https://github.com/fergiemcdowall/fergies-inverted-index/).


<a name="NOT"></a>

### `db.NOT([ ...Promise ]).then(result)`

Where A and B are sets, `db.NOT` Returns the ids of objects that are
present in A, but not in B.


<a name="OR"></a>

### `db.OR([ ...Promise ]).then(result)`

Return ids of objects that are in one or more of the query clauses


<a name="PUT"></a>

### `db.PUT([ ...Promise ]).then(result)`

Add objects to database


<a name="SEARCH"></a>

### `db.SEARCH([ ...Promise ]).then(result)`

Search the database

```javascript
  idx.SEARCH(
    idx.OR('bananas', 'different'),  // search clauses can be nested promises
    'coolness'                       // or strings (defaults to GET)
  ).then(result)
```


### More examples

(See the [tests](https://github.com/fergiemcdowall/search-index/tree/master/test) for more examples.)
