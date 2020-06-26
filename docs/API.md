# API (This version is deprecated- use the JSON API if possible)

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Initialisation](#initialisation)
  - [Make an index](#make-an-index)
  - [Access the API](#access-the-api)
- [Altering the index](#altering-the-index)
  - [DELETE](#delete)
  - [PUT](#put)
- [Composable querying](#composable-querying)
  - [AND](#and)
  - [DOCUMENTS](#documents)
  - [GET](#get)
  - [NOT](#not)
  - [OR](#or)
- [Searching](#searching)
  - [SEARCH](#search)
- [Tokenisation](#tokenisation)
  - [DICTIONARY](#dictionary)
- [Aggregation](#aggregation)
  - [BUCKET](#bucket)
  - [BUCKETFILTER](#bucketfilter)
  - [DISTINCT](#distinct)
- [Accessing the underlying index](#accessing-the-underlying-index)
  - [INDEX](#index)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Initialisation

### Make an index

`si([options[, callback]])`

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

### Access the API

Using one of the methods above you should now have an index called
`db` (or another name of your choosing). If you want to call, say
`GET` or `SEARCH` you could do either

```javascript
db.SEARCH('searchterm')
// ...
db.GET('getterm')
```

or

```javascript
const { GET, SEARCH } = db

SEARCH('searchterm')
// ...
GET('getterm')
```


## Altering the index

***Add, update or delete data from the index***

### DELETE

`DELETE([ ...ids ]).then(result)`

Deletes all objects by ID

```javascript
await DELETE([ '1', '2', '5' ])
```

### PUT

`PUT([ ...objects ])`

A promise that adds an array of documents to database. Each document can indexed with or without an `_id`. If no `_id` is specified then one will be assigned automatically.


```javascript
await PUT([
  {
    _id: 1,
    bandName: 'The Beatles',
    description: 'The nice boys of pop'
  }, {
    _id: 'two',
    bandName: 'The Rolling Stones',
    description: 'The bad boys of rock'
  }, {
    _id: 3,
    bandName: 'The Who',
    description: 'Nearly as good as Led Zeppelin'
  }
])
```

## Composable querying

***These query functions can be [mixed together in any combination](#how-do-i-compose-queries) to make powerful and expressive queries that are returned in a set sorted by document id***

### AND

`AND([ ...Promise ]).then(result)`

Boolean AND. Return IDs of objects that have prop.A AND prop.B

### DOCUMENTS

`DOCUMENTS([ ...id ]).then(result)`

Get documents by ID

### GET

`GET(property).then(result)`

`GET` returns all object ids for objects that contain the given
property, aggregated by object id.

For example get all names between `h` and `l`:

```javascript
GET({ gte: 'h', lte: 'l' }).then(result)
```

Or to get all objects that have a `name` property that begins with 'h'

```javascript
GET('h').then(result)
```

### NOT

`NOT([ ...Promise ]).then(result)`

Where A and B are sets, `NOT` Returns the ids of objects that are
present in A, but not in B.

### OR

`OR([ ...Promise ]).then(result)`

Return ids of objects that are in one or more of the query clauses


## Searching

***Search in your corpus for keywords and return a set of documents that is sorted with the most relevant first***

### SEARCH

`SEARCH([ ...Promise ]).then(result)`

Search the database and get documents back. 

```javascript
  SEARCH(
    OR('bananas', 'different'),  // search clauses can be nested promises
    'coolness'                       // or strings (defaults to GET)
  ).then(result)
```

## Tokenisation

***Tokenisation allows you to create functionality based on the set of tokens that is in the index such as autosuggest or word clouds***

### DICTIONARY

`DICTIONARY(options).then(result)`

Options:

* gte : greater than or equal to
* lte : less than or equal to

Gets an array of tokens stored in the index. Usefull for i.e. auto-complete or auto-suggest scenarios.

Examples on usage:

```javascript
// get all tokens in the index
DICTIONARY().then( /* array of tokens */ )

// get all tokens in the body.text field
DICTIONARY('body.text').then( /* array of tokens */ )

// get tokens in the body.text field that starts with 'cool'
DICTIONARY('body.text.cool').then( /* array of tokens */ )

// you can also use gte/lte ("greater/less than or equal")
DICTIONARY({
  gte: 'body.text.a',
  lte: 'body.text.g'
}).then( /* array of tokens */ )
```

## Aggregation

***You can use the aggregation functions to categorise the index data, normally for the purposes of website navigation or dividing data up into segments***

### BUCKET

`BUCKET([ ...Promise ]).then(result)`

Return IDs of objects that match the query

### BUCKETFILTER

`BUCKETFILTER([ ...bucket ], filter query).then(result)`

The first argument is an array of buckets, the second is an expression
that filters each bucket

### DISTINCT

`DISTINCT(options).then(result)`

`DISTINCT` returns every value in the db that is greater than equal
to `gte` and less than or equal to `lte` (sorted alphabetically)

For example- get all names between `h` and `l`:

```javascript
DISTINCT({ gte: 'h', lte: 'l' }).then(result)
```

## Accessing the underlying index
### INDEX

`INDEX`

Points to the underlying [index](https://github.com/fergiemcdowall/fergies-inverted-index/).



