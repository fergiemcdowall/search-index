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
  - [Get a list of unique values for a given field](#get-a-list-of-unique-values-for-a-given-field)
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
  OR: [
    {
      AND: [ 'brand:volvo', 'manufacturer:tesla' ]
    },
    'make:bmw'
  ]
})
```

# How do I perform a simple aggregation on a field?

## Get a set of document ids per unique field value

```javascript
const buckets = await QUERY({
  BUCKETS: { FIELD: fieldName }
})
```

## Get counts per unique field value

```javascript
const buckets = await QUERY({
  BUCKETS: { FIELD: fieldName }
}).then(bkts => bkts.map(
  bkt => ({
    FIELD: bkt.FIELD,
    VALUE: bkt.VALUE,
    count: bkt._id.length
  })
))
```


## Define custom "buckets"

```javascript
const buckets = await QUERY({
  BUCKETS: [
    token1,
    token2,
    token3
  ]
})
```

## Combine an aggregation with a search

```javascript
const buckets = QUERY({
  AGGREGATE: {
    BUCKETS: buckets,
    FACETS: facets,
    QUERY: query
  }
})
```

# How do I make a simple typeahead / autosuggest / matcher

There are of course many ways to do this, but if you just want a
simple "begins with" autosuggest, then you can simply use the
`DICTIONARY` function:

```javascript
const results = DICTIONARY('b')   // ['bananas','branch','brunch']
const results = DICTIONARY('br')  // ['branch','brunch']
const results = DICTIONARY('bra') // ['branch']
```

Alternatively you can use `DICTIONARY` to extract all terms from the
index and then feed them into some third-party matcher logic.

```javascript
import FuzzySet from 'fuzzyset'
// ...
const dict = await DICTIONARY()
const fs = FuzzySet()
dict.forEach(d => fs.add(d))
// ...
// fuzzy matching
fs.get(searchTerm)
```
