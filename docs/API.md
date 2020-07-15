<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**API Documentation for search-index**

- [Initialisation](#initialisation)
  - [Importing and requiring](#importing-and-requiring)
  - [Instantiating an index](#instantiating-an-index)
    - [`si(options)`](#sioptions)
- [Index API](#index-api)
  - [INDEX](#index)
  - [QUERY](#query)
    - [Running queries](#running-queries)
      - [Combining query verbs](#combining-query-verbs)
      - [Sorting, Scoring and paging](#sorting-scoring-and-paging)
    - [Query tokens](#query-tokens)
      - [Find anywhere](#find-anywhere)
      - [Find in named field](#find-in-named-field)
      - [Find token a range](#find-token-a-range)
    - [Query verbs](#query-verbs)
      - [AND](#and)
      - [BUCKET](#bucket)
      - [BUCKETFILTER](#bucketfilter)
      - [DICTIONARY](#dictionary)
      - [DISTINCT](#distinct)
      - [DOCUMENTS](#documents)
      - [GET](#get)
      - [NOT](#not)
      - [OR](#or)
      - [PAGE](#page)
      - [SCORE](#score)
      - [SEARCH](#search)
      - [SORT](#sort)
  - [UPDATE](#update)
    - [DELETE](#delete)
    - [PUT](#put)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

***(Convention: it is assumed here that the search-index module is always assigned to the variable `si`, but you can of course assign it to whatever you want)***

# Initialisation

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

Once `search-index` is assigned to a variable you can then instantiate an
index by calling the variable as a Promise:

```javascript
si().then(idx => { /* idx is a new search index */ })
```

### `si(options)`

`si(options)` returns a Promise which creates a search index when invoked

`options` is an optional object that can contain the following values:

| Name | Type | Default | Description |
|---|---|---|---|
| name | String | `'fii'` | Name of the index- will correspond to a physical folder on a filesystem (default for node) or a namespace in a database (default for web is indexedDB) depending on which backend you use  |
| tokenAppend | String | `'#'` | The string used to separate language tokens from scores in the underlying index. Should have a higher sort value than all text characters that are stored in the index- however, lower values are more platform independent (a consideration when replicating indices into web browsers for instance) |
| fii | `fergies-inverted-index` | `fergies-inverted-index()` | The underlying index. If you want to run `search-index` on a different backend (say for example Redis or Postgres), then you can instantiate `fergies-inverted-index` with the `leveldown` of your choice and then use it to make a new `search-index` |

# Index API

For the purposes of brevity, this document assumes that a search index
has been initialized in such a way that `INDEX`, `QUERY` and `UPDATE` are
available as variables:

```javascript
const { INDEX, QUERY, UPDATE } = await si()
```


## INDEX

`INDEX` is a variable that points to the underlying instance of
`fergies-inverted-index`.


## QUERY

### Running queries

`QUERY` is a function that allows you to run queries on the search
index. It is called with a query object and returns a Promise:

```javascript
const results = await QUERY(queryObject)`
```

#### Combining query verbs

TODO

#### Sorting, Scoring and paging

TODO

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

#### Find in named field

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

#### Find token a range
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

// TODO: rewrite DICTIONARY so that it takes a DISTINCT as input

#### DISTINCT

// TODO: DISTINCT on many or all fields

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

#### GET

```javascript
// Returns a list of documents that contain the provided token
{
  GET: token
}
```

// TODO: does this need to be public? Is it different from AND/OR with one term or BUCKET?

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

## UPDATE

`UPDATE` is a function that allows you to make changes to the search
index. It returns a Promise.

```javascript
const result = await UPDATE(updateInstruction)`
```


### DELETE

```javascript
// Delete documents from index
{
  DELETE: documentIds // an array of document IDs
}
```

### PUT

```javascript
// Add documents to index
{
  PUT: documents // an array of documents (plain old javascript objects)
}

// if any document does not contain an _id field, then one will be
// generated and assigned
```
