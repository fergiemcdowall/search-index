<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
# API Documentation for search-index

- [Module API](#module-api)
  - [Importing and requiring](#importing-and-requiring)
  - [Instantiating an index](#instantiating-an-index)
    - [`si(options)`](#sioptions)
- [Index API](#index-api)
  - [DELETE](#delete)
  - [DOCUMENT_COUNT](#document_count)
  - [FIELDS](#fields)
  - [INDEX](#index)
  - [QUERY](#query)
    - [Running queries](#running-queries)
      - [Returning references or documents](#returning-references-or-documents)
      - [Nesting query verbs](#nesting-query-verbs)
      - [Manipulating result sets](#manipulating-result-sets)
    - [Query tokens](#query-tokens)
      - [Find anywhere](#find-anywhere)
      - [Find in named field or fields](#find-in-named-field-or-fields)
      - [Find within a range](#find-within-a-range)
      - [Find where a field exists](#find-where-a-field-exists)
    - [Query verbs](#query-verbs)
      - [AND](#and)
      - [BUCKET](#bucket)
      - [BUCKETFILTER](#bucketfilter)
      - [DICTIONARY](#dictionary)
      - [DISTINCT](#distinct)
      - [DOCUMENTS](#documents)
      - [MAX](#max)
      - [MIN](#min)
      - [NOT](#not)
      - [OR](#or)
      - [PAGE](#page)
      - [SCORE](#score)
      - [SEARCH](#search)
      - [SORT](#sort)
  - [PUT](#put)
  - [PUT_RAW](#put_raw)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

***(Convention: it is assumed here that the search-index module is always assigned to the variable `si`, but you can of course assign it to whatever you want)***

# Module API

## Importing and requiring

This module can either be invoked using `import` or `require`
depending on your environment:

```javascript
import si from search-index
```

or

```javascript
const si = require('search-index')
```

## Instantiating an index

Once the `search-index` module is assigned to a variable you can
then instantiate an index by invoking the module variable as a
Promise:
    

```javascript
si().then(idx => { /* idx is a new search index */ })
```

### `si(options)`

`si(options)` returns a Promise which creates a search index when invoked

`options` is an optional object that can contain the following values:

| Name | Type | Default | Description |
|---|---|---|---|
| `caseSensitive` | `boolean` | `false` | If true, `case` is preserved (so 'BaNaNa' != 'banana'), if `false`, text matching will not be case sensitive |
| `fii` | `fergies-inverted-index` | `fergies-inverted-index()` | The underlying index. If you want to run `search-index` on a different backend (say for example Redis or Postgres), then you can instantiate `fergies-inverted-index` with the `leveldown` of your choice and then use it to make a new `search-index` |
| `name` | `String` | `'fii'` | Name of the index- will correspond to a physical folder on a filesystem (default for node) or a namespace in a database (default for web is indexedDB) depending on which backend you use  |
| `tokenAppend` | `String` | `'#'` | The string used to separate language tokens from scores in the underlying index. Should have a higher sort value than all text characters that are stored in the index- however, lower values are more platform independent (a consideration when replicating indices into web browsers for instance) |
| `stopwords` | `Array` | `[]` | A list of words to be ignored when indexing and querying |


# Index API

For the purposes of brevity, this document assumes that a search index
has been initialized in such a way that `INDEX`, `QUERY` and `UPDATE` are
available as variables:

```javascript
const { INDEX, QUERY, UPDATE } = await si()
```

It may be helpful to check out the
[tests](https://github.com/fergiemcdowall/search-index/tree/master/test)
for more examples.


## DELETE

```javascript
// Delete documents from the index
const result = await DELETE(documentIds)
// "documentIds" is an Array of IDs
// "result" is the status of the deletion
```

## DOCUMENT_COUNT

```javascript
// returns the total amount of documents in the index
const totalDocs = await DOCUMENT_COUNT()
```


## FIELDS

```javascript
// get every document field name that has been indexed:
const fields = await FIELDS()
```


## INDEX

`INDEX` points to the underlying instance of [`fergies-inverted-index`](https://github.com/fergiemcdowall/fergies-inverted-index).


## QUERY

### Running queries

`QUERY` is a function that allows you to run queries on the search
index. It is called with a query object and returns a Promise:

```javascript
const results = await QUERY(query)`
```

#### Returning references or documents

`QUERY` can return both refences to documents and the documents
themselves.

References are returned by default. To return documents, append the
[`DOCUMENTS`](#DOCUMENTS) command to the query:

```javascript
const results = await QUERY(query, { DOCUMENTS: true })`
```

(**Performance tip:** When dealing with large result sets, use
[`SCORE`](#SCORE), [`SORT`](#SORT) and [`PAGE`](#PAGE) before [`DOCUMENTS`](#DOCUMENTS) in order to minimize the
amount of documents that need to be fetched from the index)

#### Nesting query verbs

Query verbs can be nested to create powerful expressions:

```javascript
// Example: AND with a nested OR with a nested AND
{
  AND: [ token1, token2, {
    OR: [ token3, {
      AND: [ token4, token5 ]
    }]
  }]
}
```

#### Manipulating result sets

[SCORE](#score), [SORT](#sort) and [PAGE](#page) can be invoked after
results have been returned:

```javascript
// Example: get the second page of documents ordered by price
QUERY({
  FIELD: 'price'  // Select all documents that have a 'price'
}, {
  SCORE: 'SUM'    // Score on the sum of the price field
}, {
  SORT: {
    TYPE: 'NUMERIC',       // sort numerically, not alphabetically
    DIRECTION: 'ASCENDING' // cheapest first
  }
}, {
  PAGE: {
    NUMBER: 1,    // '1' is the second page (pages counted from '0')
    SIZE: 20      // 20 results per page
  }
})
```

### Query tokens

`search-index` is a text orientated reverse index. This means that
documents are retrievable by passing text tokens that they contain
into queries. There are various ways to express tokens:

#### Find anywhere

```javascript
'<token value>'
```

Example:

```javascript
'banana'
```

#### Find in named field or fields

```javascript
'<field name>:<token value>'
```
Example:

```javascript
'fruit:banana'

// (can also be expressed as ->)
{
  FIELD: 'fruit',
  VALUE: 'banana'
}
```

```javascript
// Find in two or more specified fields:
{
  FIELD: [ 'fruit', 'description' ], // array of field names
  VALUE: 'banana'
}
```

#### Find within a range
```javascript
{
  FIELD: fieldName,
  VALUE: {
    GTE: gte,        // greater than or equal to
    LTE: lte         // less than or equal to
  }
}
```
Example (get all fruits beginning with 'a', 'b' or 'c'):
```javascript
// this token range would capture 'banana'
{
  FIELD: 'fruit',
  VALUE: {
    GTE: 'a',
    LTE: 'c'
  }
}
```

#### Find where a field exists
```javascript
// Find all documents that contain a 'price' field
{
  FIELD: 'price'
}
```


### Query verbs

#### AND

```javascript
// Boolean AND: Return results that contain all tokens
{
  AND: [ token1, token2, ... ]
}
```

#### BUCKET

```javascript
// Return the IDs of documents that exist with the given token
{
  BUCKET: token
}
```

#### BUCKETFILTER

```javascript
// Fetch a collection of BUCKETs and then subtract any documents that
// are returned by FILTER
{
  BUCKETFILTER: {
    BUCKETS: [ token1, token2, ... ],
    FILTER: token3
  }
}
```

#### DICTIONARY

```javascript
// Return each available field value for the given token. (to see
// fields use DISTINCT)
{
  DICTIONARY: token
}
```

#### DISTINCT

```javascript
// Return each available field and value for the given token.
{
  DISTINCT: token
}
```

#### DOCUMENTS

```javascript
// Returns full documents instead of just metadata. If preceded by a
// query that returns document metadata such as AND, OR, NOT, or
// SEARCH, { DOCUMENTS: true } will return documents associated with
// that result set. { DOCUMENTS: true } without a preceding query will
// return all documents in index 
PrecendingQuery, {
  DOCUMENTS: true
}

// For example:
{
  SEARCH: [ token1, token2 ]
}, {
  DOCUMENTS: true  // returns only documents associated with preceding result set
}

// returns every single document in index:
{
  DOCUMENTS: true
}
```


#### MAX

```javascript
// get the _alphabetically_ maxiumum/last value of the given token space
{
  MAX: token
}
```


#### MIN

```javascript
// get the _alphabetically_ minimum/first value of the given token space
{
  MIN: token
}
```


#### NOT

```javascript
{
  INCLUDE: queryExpression1,
  EXCLUDE: queryExpression2
}
```


#### OR

```javascript
// Boolean OR: Return results that contain one or more tokens
{
  OR: [ token1, token2, ... ]
}
```


#### PAGE

```javascript
// show a single page of the result set
{
  PAGE: {
    NUMBER: pageNumber, // to count from the end of the result set use negative numbers
    SIZE: pageSize
  }
}

// PAGEing can only be invoked once a result set has been generated
{
  SEARCH: [ token1, token2 ]
}, {
  PAGE: {
    NUMBER: 1,
    SIZE: 20
  }
}

```

#### SCORE

```javascript
// show a single page of the result set
{
  SCORE: scoreType // can be 'TFIDF', 'SUM, 'PRODUCT' or 'CONCAT'
}

// SCOREing can only be invoked once a result set has been generated
{
  AND: [ token1, token2 ]
}, {
  SCORE: 'SUM'
}

```

#### SEARCH

```javascript
// Return search results sorted by relevance to query tokens
{
  SEARCH: [ token1, token2, ... ]
}
```


#### SORT

```javascript
// Return search results sorted by relevance to query tokens
{
  SORT: {
    TYPE: type,              // can be 'NUMERIC' or 'ALPHABETIC'
    DIRECTION: direction,    // can be 'ASCENDING' or 'DESCENDING'
    FIELD: field             // field to sort on
  }
}

// SORTing can only be invoked once a result set has been generated
{
  DOCUMENTS: true
}, {
  SORT: {
    TYPE: 'NUMERIC',
    DIRECTION: 'ASCENDING',
    FIELD: '_doc.price'
  }
}

```


## PUT

```javascript
// Put documents into the index
const result = await PUT(documents, options)
// "result" shows the success or otherwise of the insertion
// "documents" is an Array of javascript Objects.
// "options" is an Object that contains indexing options
```

If any document does not contain an `_id` field, then one will be
generated and assigned


`options` is an optional object that can contain the following values:

| Name | Type | Default | Description |
|---|---|---|---|
|`doNotIndexField`|`Array`|`[]`|These fields will not be searchable, but they will still be stored|
|`doNotStoreRawDocs`|`Boolean`|`false`|Whether to store the raw document or not. In many cases it may be desirable to store it externally, or to skip storing when indexing if it is going to be updated directly later on|


## PUT_RAW

```javascript
// Put raw documents into the index
const result = await PUT_RAW(rawDocuments)
// "result" shows the success or otherwise of the insertion
// "rawDocuments" is an Array of javascript Objects that must
// contain an _id field
```

`PUT_RAW` writes raw documents to the index. Raw documents are the
documents that the index returns. Use raw documents when the documents
that are indexed are not the same as the ones that you want the index
to return. This can be useful if you want documents to be retrievable
for terms that dont appear in the actual document. It can also be
useful if you want to store stripped down versions of the document in
the index in order to save space.

NOTE: if the documents that the index returns are very different to
the corresponding documents that are indexed, it may make sense to use
the `doNotStoreRawDocs` flag when indexing.
