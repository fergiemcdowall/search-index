<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [API](#api)
  - [Initialisation](#initialisation)
    - [si](#si)
  - [Altering the index](#altering-the-index)
    - [DELETE](#delete)
    - [PUT](#put)
  - [Composable querying](#composable-querying)
    - [AND](#and)
    - [DOCUMENTS](#documents)
    - [NOT](#not)
    - [OR](#or)
  - [Searching](#searching)
    - [GET](#get)
    - [SEARCH](#search)
  - [Tokenisation](#tokenisation)
    - [DICTIONARY](#dictionary)
  - [Aggregation](#aggregation)
    - [BUCKET](#bucket)
    - [BUCKETFILTER](#bucketfilter)
    - [DISTINCT](#distinct)
  - [Accessing the underlying index](#accessing-the-underlying-index)
    - [INDEX](#index)
- [FAQ](#faq)
  - [How do I get my data into search-index?](#how-do-i-get-my-data-into-search-index)
    - [Replicate an index](#replicate-an-index)
    - [Create a new index](#create-a-new-index)
  - [What is the difference between AND, GET and SEARCH?](#what-is-the-difference-between-and-get-and-search)
  - [How do I get out entire documents and not just document IDs?](#how-do-i-get-out-entire-documents-and-not-just-document-ids)
  - [How do I search on specific fields?](#how-do-i-search-on-specific-fields)
  - [How do I compose queries?](#how-do-i-compose-queries)
  - [How do I perform a simple aggregation on a field?](#how-do-i-perform-a-simple-aggregation-on-a-field)
    - [Get a list of unique values for a field](#get-a-list-of-unique-values-for-a-field)
    - [Get a set of document ids per unique field value](#get-a-set-of-document-ids-per-unique-field-value)
    - [Get counts per unique field value](#get-counts-per-unique-field-value)
    - [Define custom "buckets"](#define-custom-buckets)
    - [Combine an aggregation with a search](#combine-an-aggregation-with-a-search)
  - [How do I make a simple typeahead / autosuggest / matcher](#how-do-i-make-a-simple-typeahead--autosuggest--matcher)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# API

## Initialisation
### si

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

## Altering the index
### DELETE

`db.DELETE([ ...Promise ]).then(result)`

Deletes all objects by ID

### PUT

`db.PUT([ ...Promise ]).then(result)`

Add objects to database


## Composable querying
### AND

`db.AND([ ...Promise ]).then(result)`

Boolean AND. Return IDs of objects that have prop.A AND prop.B

### DOCUMENTS

`db.DOCUMENTS([ ...id ]).then(result)`

Get documents by ID

### NOT

`db.NOT([ ...Promise ]).then(result)`

Where A and B are sets, `db.NOT` Returns the ids of objects that are
present in A, but not in B.

### OR

`db.OR([ ...Promise ]).then(result)`

Return ids of objects that are in one or more of the query clauses


## Searching
### GET

`db.GET(property).then(result)`

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

### SEARCH

`db.SEARCH([ ...Promise ]).then(result)`

Search the database and get documents back. 

```javascript
  idx.SEARCH(
    idx.OR('bananas', 'different'),  // search clauses can be nested promises
    'coolness'                       // or strings (defaults to GET)
  ).then(result)
```

## Tokenisation
### DICTIONARY

`db.DICTIONARY(options).then(result)`

Options:

* gte : greater than or equal to
* lte : less than or equal to

Gets an array of tokens stored in the index. Usefull for i.e. auto-complete or auto-suggest scenarios.

Examples on usage:

```javascript
// get all tokens in the index
idx.DICTIONARY().then( /* array of tokens */ )

// get all tokens in the body.text field
idx.DICTIONARY('body.text').then( /* array of tokens */ )

// get tokens in the body.text field that starts with 'cool'
idx.DICTIONARY('body.text.cool').then( /* array of tokens */ )

// you can also use gte/lte ("greater/less than or equal")
idx.DICTIONARY({
  gte: 'body.text.a',
  lte: 'body.text.g'
}).then( /* array of tokens */ )
```

## Aggregation
### BUCKET

`db.BUCKET([ ...Promise ]).then(result)`

Return IDs of objects that match the query

### BUCKETFILTER

`db.BUCKETFILTER([ ...bucket ], filter query).then(result)`

The first argument is an array of buckets, the second is an expression
that filters each bucket

### DISTINCT

`db.DISTINCT(options).then(result)`

`db.DISTINCT` returns every value in the db that is greater than equal
to `gte` and less than or equal to `lte` (sorted alphabetically)

For example- get all names between `h` and `l`:

```javascript
db.DISTINCT({ gte: 'h', lte: 'l' }).then(result)
```

## Accessing the underlying index
### INDEX

`db.INDEX`

Points to the underlying [index](https://github.com/fergiemcdowall/fergies-inverted-index/).



# FAQ

## How do I get my data into search-index?

You can either replicate an index from an existing index, or create a
completely new index.

### Replicate an index

Get the underlying index by using db.INDEX and then replicate into the
index by using the [levelup API](https://github.com/Level/levelup#dbcreatereadstreamoptions)

### Create a new index

First, initialise an index, and then use `PUT` to add documents to the
index.

```javascript
const db = si({ name: indexName })
// then somewhere else in the code, being aware of asynchronousity
db.PUT([ /* my array of objects */ ]).then(doStuff)
```

## What is the difference between AND, GET and SEARCH?

* **AND** `AND` returns a set of documents that contain *all* of the
terms contained in the query

* **GET** `GET` returns a set of documents that contain the terms
contained in the query. `GET` is the same as `AND` or `OR` with only
one term specified.

* **SEARCH** `SEARCH` performs an `AND` query and returns a set of
documents which is ordered in terms of relevance. Search-index uses a
TF-IDF algorithm to determine relevance.


## How do I get out entire documents and not just document IDs?

You need to join your resultset with `DOCUMENTS`

```javascript
AND('land:SCOTLAND', 'colour:GREEN').then(DOCUMENTS).then(console.log)
```

## How do I search on specific fields?

To return hits for all documents containing 'apples' or 'oranges' in
the `title` field you would do something like this:

```javascript
OR(
  'title:apples',
  'title:oranges',
)
```

## How do I compose queries?

Queries can be composed by combining and nesting promises. For example

```javascript
AND(
  OR(
    'title:quite',
    AND(
      'body.text:totally',
      'body.text:different'
    )
  ),
  'body.metadata:cool'
).then(console.log)  // returns a result
```

## How do I perform a simple aggregation on a field?

### Get a list of unique values for a field

Use `DISTINCT` to get a list of unique values for a field called "agency":

```javascript
db.DISTINCT('agency').then(console.log)
/*
[
  'agency.POLICE',
  'agency.DOJ',
  'agency.SUPREMECOURT'
]
*/

```

### Get a set of document ids per unique field value

```javascript
db.DISTINCT('agency')
 .then(result => Promise.all(result.map(db.BUCKET)))
 .then(console.log)
/*
[
  { gte: 'agency.POLICE', lte: 'agency.POLICE', _id: [ 2,3,4,7 ] },
  { gte: 'agency.DOJ' lte: 'agency.DOJ', _id: [ 1, 6 ]
  { gte: 'agency.SUPREMECOURT' lte: 'agency.SUPREMECOURT', _id: [ 5, 7 ]
]
*/

```

### Get counts per unique field value

```javascript
db.DISTINCT('agency')
 .then(result => Promise.all(result.map(db.BUCKET)))
 .then(result => result.map(item => { item.count = item._id.length; return item } ))
 .then(console.log)
/*
[
  { gte: 'agency.POLICE' lte: 'agency.POLICE', _id: [ 2,3,4,7 ], count: 4 },
  { gte: 'agency.DOJ' lte: 'agency.DOJ', _id: [ 1, 6 ], count: 2 },
  { gte: 'agency.SUPREMECOURT' lte: 'agency.SUPREMECOURT', _id: [ 5, 7 ], count: 2 }
]
*/

```


### Define custom "buckets"

```javascript
Promise.all([
  'totalamt.0',
  'totalamt.10000000',
  'totalamt.200000000'
].map(db.BUCKET))
 .then(console.log)
/*
[
  { gte: 'totalamt.0',
    lte: 'totalamt.0',
    _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c787' ] },
  { gte: 'totalamt.10000000',
    lte: 'totalamt.10000000',
    _id: [ '52b213b38594d8a2be17c785' ] },
  { gte: 'totalamt.200000000',
    lte: 'totalamt.200000000',
    _id: [ '52b213b38594d8a2be17c789' ] }
]
*/
```

```javascript
Promise.all([
  { gte: 'totalamt.0', lte: 'totalamt.10000000'},
  { gte: 'totalamt.10000001', lte: 'totalamt.99999999'}
].map(db.BUCKET))
 .then(console.log)
/*
[
  { gte: 'totalamt.0',
    lte: 'totalamt.10000000',
    _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c785', '52b213b38594d8a2be17c787' ] },
  { gte: 'totalamt.10000001',
    lte: 'totalamt.99999999',
    _id: [ '52b213b38594d8a2be17c789' ] }
]
*/

```

### Combine an aggregation with a search

```javascript
const bucketStructure = db.DISTINCT('agency')
 .then(result => Promise.all(result.map(db.BUCKET)))
const search = db.SEARCH('board_approval_month:October')
// here the aggregation will only be performed on documents matching that
// satisfy the search criteria ('board_approval_month:October')
db.BUCKETFILTER(bucketStructure, search).then(/* result */)
```
## How do I make a simple typeahead / autosuggest / matcher

There are of course many ways to do this, but if you just want a
simple "begins with" autosuggest, then you can simply use the
`DICTIONARY` function:

```javascript
// get all tokens in the index
db.DICTIONARY().then( /* array of tokens */ )

// get all tokens in the body.text field
db.DICTIONARY('body.text').then( /* array of tokens */ )

// get tokens in the body.text field that starts with 'cool'
db.DICTIONARY('body.text.cool').then( /* array of tokens */ )

// you can also use gte/lte ("greater/less than or equal")
db.DICTIONARY({
  gte: 'body.text.a',
  lte: 'body.text.g'
}).then( /* array of tokens */ )
```

Alternatively you can use `DICTIONARY` to extract all terms from the
index and then feed them into some third-party matcher logic.
