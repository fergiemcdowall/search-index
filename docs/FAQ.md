<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
# FAQ

- [How do I create an index?](#how-do-i-create-an-index)
- [How do I get my data into search-index?](#how-do-i-get-my-data-into-search-index)
  - [Export/Import an index](#exportimport-an-index)
  - [Add documents to an index](#add-documents-to-an-index)
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


# How do I create an index?

```javascript
const si = import si from 'si'

// ...

const index = await si() // or si().then(index => ...)
```


# How do I get my data into search-index?

You can either import an index, or add documents.

## Export/Import an index

```javascript
// EXPORT an index
const exportFile = await index1.EXPORT()

// IMPORT an index
index2.IMPORT(exportFile)
```

NOTE: `IMPORT`ing an index completely overwrites any existing index

## Add documents to an index

```javascript
// then somewhere else in the code, being aware of asynchronousity
PUT([ /* my array of objects */ ]).then(doStuff)
```


# How do I get out entire documents and not just document IDs?

Use `{ DOCUMENTS: true }`.

Query that returns document IDs:
```javascript
QUERY({
  SEARCH: [ 'search', 'terms' ]
})
```

Query that returns documents:
```javascript
QUERY({
  SEARCH: [ 'search', 'terms' ]
}, {
  DOCUMENTS: true
})
```


# How do I search on specific fields?

To return hits for all documents containing 'orange' in
the `title` field you would do something like this:

```javascript
QUERY({
  SEARCH: [ 'title:orange' ]
})

// can also be expressed as:
QUERY({
  SEARCH: [{
    FIELD: [ 'title' ],
    VALUE: 'orange'
  }]
})
```

# How do I compose queries?

Queries can be composed by nesting `SEARCH`, `AND`, `NOT` and `OR`
clauses as deeply as required. For example:

```javascript
QUERY({
  OR: [{
    AND: [ 'brand:volvo', 'manufacturer:tesla' ]
  }, 'make:bmw']
})
```

# How do I perform a simple aggregation on a field?

## Get a list of unique values for a field

```javascript
const uniqueValues = await QUERY({
  DISTINCT: token
})
```

## Get a set of document ids per unique field value

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

## Get counts per unique field value

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


## Define custom "buckets"

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

## Combine an aggregation with a search

```javascript
const bucketStructure = DISTINCT('agency')
 .then(result => Promise.all(result.map(BUCKET)))
const search = SEARCH('board_approval_month:October')
// here the aggregation will only be performed on documents matching that
// satisfy the search criteria ('board_approval_month:October')
BUCKETFILTER(bucketStructure, search).then(/* result */)
```
# How do I make a simple typeahead / autosuggest / matcher

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
