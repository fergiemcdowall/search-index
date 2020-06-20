# FAQ

## How do I get my data into search-index?

You can either replicate an index from an existing index, or create a
completely new index.

### Replicate an index

Get the underlying index by using INDEX and then replicate into the
index by using the [levelup API](https://github.com/Level/levelup#dbcreatereadstreamoptions)

### Create a new index

First, initialise an index, and then use `PUT` to add documents to the
index.

```javascript
const db = si({ name: indexName })
// then somewhere else in the code, being aware of asynchronousity
PUT([ /* my array of objects */ ]).then(doStuff)
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
DISTINCT('agency').then(console.log)
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
DISTINCT('agency')
 .then(result => Promise.all(result.map(BUCKET)))
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
DISTINCT('agency')
 .then(result => Promise.all(result.map(BUCKET)))
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
].map(BUCKET))
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
].map(BUCKET))
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
const bucketStructure = DISTINCT('agency')
 .then(result => Promise.all(result.map(BUCKET)))
const search = SEARCH('board_approval_month:October')
// here the aggregation will only be performed on documents matching that
// satisfy the search criteria ('board_approval_month:October')
BUCKETFILTER(bucketStructure, search).then(/* result */)
```
## How do I make a simple typeahead / autosuggest / matcher

There are of course many ways to do this, but if you just want a
simple "begins with" autosuggest, then you can simply use the
`DICTIONARY` function:

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

Alternatively you can use `DICTIONARY` to extract all terms from the
index and then feed them into some third-party matcher logic.
