<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
# API Documentation for search-index

- [Module API](#module-api)
  - [Importing and requiring](#importing-and-requiring)
  - [Instantiating an index](#instantiating-an-index)
    - [`si(options)`](#sioptions)
- [Index API](#index-api)
  - [Tokens](#tokens)
    - [Find anywhere](#find-anywhere)
    - [Find in named field or fields](#find-in-named-field-or-fields)
    - [Find within a range](#find-within-a-range)
    - [Find where a field exists](#find-where-a-field-exists)
    - [Create a tokenization pipeline when querying](#create-a-tokenization-pipeline-when-querying)
  - [ALL_DOCUMENTS](#all_documents)
  - [BUCKETS](#buckets)
  - [CREATED](#created)
  - [DELETE](#delete)
  - [DICTIONARY](#dictionary)
  - [DISTINCT](#distinct)
  - [DOCUMENTS](#documents)
  - [DOCUMENT_COUNT](#document_count)
  - [EXPORT](#export)
  - [FACETS](#facets)
  - [FIELDS](#fields)
  - [FLUSH](#flush)
  - [IMPORT](#import)
  - [INDEX](#index)
  - [MAX](#max)
  - [MIN](#min)
  - [PUT](#put)
    - [Tokenization pipeline when indexing](#tokenization-pipeline-when-indexing)
      - [Reorder pipeline](#reorder-pipeline)
      - [Create custom pipeline stages](#create-custom-pipeline-stages)
  - [PUT_RAW](#put_raw)
  - [QUERY](#query)
    - [Running queries](#running-queries)
      - [Returning references or documents](#returning-references-or-documents)
      - [Nesting query verbs](#nesting-query-verbs)
      - [Manipulating result sets](#manipulating-result-sets)
    - [Query options](#query-options)
      - [BUCKETS](#buckets-1)
      - [DOCUMENTS](#documents-1)
      - [FACETS](#facets-1)
      - [PAGE](#page)
      - [PIPELINE](#pipeline)
      - [SCORE](#score)
      - [SORT](#sort)
      - [WEIGHT](#weight)
    - [Query verbs](#query-verbs)
      - [ALL_DOCUMENTS](#all_documents-1)
      - [AND](#and)
      - [NOT](#not)
      - [OR](#or)
  - [SEARCH](#search)
  - [TOKENIZATION_PIPELINE_STAGES](#tokenization_pipeline_stages)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

***(Convention: it is assumed here that the search-index module is always assigned to the variable `si`, but you can of course assign it to whatever you want)***

# Module API

## Importing and requiring

This module can be invoked with `import` and/or `require`
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
instantiate an index by invoking the module variable as a Promise:
    

```javascript
const idx = await si(options)
```

When intantiated in a browser `search-index` will use `indexedDB`
as a keystore by default, when intantiated in node.js it will use
`levelDB`. `search-index` can also use other keystores via the `db`
parameter.


### `si(options)`

`si(options)` returns a Promise which creates a search index when invoked

`options` is an object that can contain the following properties:

| Name | Type | Default | Description |
|---|---|---|---|
| `caseSensitive` | `boolean` | `false` | If true, `case` is preserved (so 'BaNaNa' != 'banana'), if `false`, text matching will not be case sensitive |
| `db` | [`abstract-leveldown`](https://github.com/Level/awesome/#stores) store | `leveldown` | The underlying data store. If you want to run `search-index` on a different backend (say for example Redis or Postgres), then you can pass the appropriate [`abstract-leveldown`](https://github.com/Level/awesome/#stores) compatible store |
| `cacheLength` | `Number` | `1000` | Length of the LRU cache. A bigger number will give faster reads but use more memory. Cache is emptied after each write. |
| `name` | `String` | `'fii'` | Name of the index- will correspond to a physical folder on a filesystem (default for node) or a namespace in a database (default for web is indexedDB) depending on which backend you use  |
| `tokenAppend` | `String` | `'#'` | The string used to separate language tokens from scores in the underlying index. Should have a higher sort value than all text characters that are stored in the index- however, lower values are more platform independent (a consideration when replicating indices into web browsers for instance) |
| `stopwords` | `Array` | `[]` | A list of words to be ignored when indexing and querying |


# Index API

For the purposes of brevity, this document assumes that a search index
has been initialized in such a way that the functions below are
available as variables:

```javascript
const { INDEX, QUERY, UPDATE /* etc. */ } = await si()
```

It may be helpful to check out the
[tests](https://github.com/fergiemcdowall/search-index/tree/master/test/src)
for more examples.

## Tokens

`search-index` is a text orientated reverse index. This means that
documents are retrievable by passing text tokens that they contain
into queries. There are various ways to express tokens:

### Find anywhere

```javascript
'<token value>'
```

Example:

```javascript
'banana'
```

### Find in named field or fields

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

### Find within a range
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

### Find where a field exists
```javascript
// Find all documents that contain a 'price' field
{
  FIELD: 'price'
}
```

### Create a tokenization pipeline when querying

Use the [`PIPELINE`](#pipeline) option when using [`QUERY`](#query)



## ALL_DOCUMENTS

See also [DOCUMENTS](#documents)

```javascript
// Return all documents from index.
const documents = await ALL_DOCUMENTS(limit)
// "limit" is the maximum total of documents to be returned
```

## BUCKETS

```javascript
// Return the IDs of documents for each given token filtered by the
// query result
const buckets = await BUCKETS(token1, token2, ...)
```


## CREATED

```javascript
// find out when index was first created
const timestamp = await CREATED()
```


## DELETE

NOTE: for indices to be deleteable documents must be indexed whith
[`storeVectors`](#put) set to `true`

See also [FLUSH](#flush)

```javascript
// Delete documents from the index
const result = await DELETE(id1, id2, id3 /*...*/)
```


## DICTIONARY

See also [DISTINCT](#distinct)

```javascript
// Return each available field value for the given token space.
const dictionary = await DICTIONARY(token)
```

## DISTINCT

See also [DICTIONARY](#dictionary)

```javascript
// Return distinct field values from index
const distinct = await DISTINCT(token)
```


## DOCUMENTS

See also [ALL_DOCUMENTS](#all_documents)

```javascript
// Return named documents from index.
const documents = await DOCUMENTS(id1, id2, id3 /* ... */)
```


## DOCUMENT_COUNT

```javascript
// returns the total amount of documents in the index
const totalDocs = await DOCUMENT_COUNT()
```


## EXPORT

```javascript
// creates a backup/export of an index
const indexExport = await EXPORT()
```


## FACETS

```javascript
// Return document ids for each distinct field/value combination for
// the given token space.
const facets = await FACETS(token)
```


## FIELDS

```javascript
// get every document field name that has been indexed:
const fields = await FIELDS()
```


## FLUSH
```javascript
// Delete everything and start again (including creation metadata)
await FLUSH()
```


## IMPORT

```javascript
// creates an index from a backup/export
await IMPORT(index)
```


## INDEX

`INDEX` points to the underlying instance of [`fergies-inverted-index`](https://github.com/fergiemcdowall/fergies-inverted-index).


## MAX

```javascript
// get the maxiumum/last value of the given token space
const max = await MAX(token)
```


## MIN

```javascript
// get the minimum/first value of the given token space
const min = await MIN(token)
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
| `caseSensitive` | `boolean` | `false` | If true, `case` is preserved (so 'BaNaNa' != 'banana'), if `false`, text matching will not be case sensitive |
|`ngrams`|`object`|<pre lang="javascript">{<br>  lengths: [ 1 ],<br>  join: ' ',<br>  fields: undefined<br>}</pre>| An object that describes ngrams. See [ngraminator](https://www.npmjs.com/package/ngraminator) for how to specify ngrams |
|`replace`|`object`|`{ fields: [], values: {} }`|`fields` is an array that specifies the fields where replacements will happen, `values` is an array that specifies the tokens to be swapped in, for example: `{ values: { sheep: [ 'animal', 'livestock' ] } }`|
|`skipField`|`Array`|`[]`|These fields will not be searchable, but they will still be stored|
|`stopwords`| `Array` | `[]` | A list of words to be ignored when indexing |
|`storeRawDocs`|`boolean`|`true`|Whether to store the raw document or not. In many cases it may be desirable to store it externally, or to skip storing when indexing if it is going to be updated directly later on|
|`storeVectors`|`boolean`|`false`|When `true`, documents will be deletable and overwritable, but will take up more space on disk|
|`tokenizationPipeline`|`Array`|<pre lang="javascript">[<br>  SPLIT,<br>  SKIP,<br>  LOWCASE,<br>  REPLACE,<br>  NGRAMS,<br>  STOPWORDS,<br>  SCORE_TERM_FREQUENCY<br>]</pre>| Tokenisation pipeline. Stages can be added and reordered|

### Tokenization pipeline when indexing

Every field of every document that is indexed is passed through the
tokenization pipeline. The tokenization pipeline consists of a
sequence of stages that are applied to the field with the result of
the preceding stage providing the input for the result of the
following stage.

The default tokenization pipeline looks like this:

```javascript
tokenizer: (tokens, field, ops) =>
  SPLIT([tokens, field, ops])
    .then(SKIP)
    .then(LOWCASE)
    .then(REPLACE)
    .then(NGRAMS)
    .then(STOPWORDS)
    .then(SCORE_TERM_FREQUENCY)
    .then(([tokens, field, ops]) => tokens)
```

#### Reorder pipeline

Example: reorder the pipeline to remove stopwords _before_ creating
ngrams:

```javascript
const { PUT, TOKENIZATION_PIPELINE_STAGES } = await si({
  name: 'pipeline-test'
})
await PUT(docs, {
  tokenizer: (tokens, field, ops) =>
  TOKENIZATION_PIPELINE_STAGES.SPLIT([tokens, field, ops])
    .then(TOKENIZATION_PIPELINE_STAGES.SKIP)
    .then(TOKENIZATION_PIPELINE_STAGES.LOWCASE)
    .then(TOKENIZATION_PIPELINE_STAGES.REPLACE)
    .then(TOKENIZATION_PIPELINE_STAGES.STOPWORDS) // <-- order switched
    .then(TOKENIZATION_PIPELINE_STAGES.NGRAMS)    // <-- order switched
    .then(TOKENIZATION_PIPELINE_STAGES.SCORE_TERM_FREQUENCY)
    .then(([tokens, field, ops]) => tokens)
})
```

#### Create custom pipeline stages

A custom pipeline stage must be in the following form:

```javascript
// take tokens (Array of tokens), field (the field name), and options,
// and then return then return the Array of tokens
([ tokens, field, ops ]) => {
  // some processing here...
  return [ tokens, field, ops ]
}
```


Example: Normalize text characters:

```javascript
const { PUT, TOKENIZATION_PIPELINE_STAGES } = await si({
  name: 'pipeline-test'
})
await PUT(docs, {
  tokenizer: (tokens, field, ops) =>
    TOKENIZATION_PIPELINE_STAGES.SPLIT([tokens, field, ops])
      .then(TOKENIZATION_PIPELINE_STAGES.SKIP)
      .then(TOKENIZATION_PIPELINE_STAGES.LOWCASE)
      .then(TOKENIZATION_PIPELINE_STAGES.REPLACE)
      .then(TOKENIZATION_PIPELINE_STAGES.NGRAMS)
      .then(TOKENIZATION_PIPELINE_STAGES.STOPWORDS)
      // björn -> bjorn, allé -> alle, etc.
      .then(([tokens, field, ops]) => [
        tokens.map(t => t.normalize("NFD").replace(/[\u0300-\u036f]/g, ""),
        field,
        ops
      ])
      .then(TOKENIZATION_PIPELINE_STAGES.SCORE_TERM_FREQUENCY)
      .then(([tokens, field, ops]) => tokens)
})
```

Example: stemmer:

```javascript
const stemmer = require('stemmer')
const { PUT, TOKENIZATION_PIPELINE_STAGES } = await si({
  name: 'pipeline-test'
})
await PUT(docs, {
  tokenizer: (tokens, field, ops) =>
    TOKENIZATION_PIPELINE_STAGES.SPLIT([tokens, field, ops])
      .then(TOKENIZATION_PIPELINE_STAGES.SKIP)
      .then(TOKENIZATION_PIPELINE_STAGES.LOWCASE)
      .then(TOKENIZATION_PIPELINE_STAGES.REPLACE)
      .then(TOKENIZATION_PIPELINE_STAGES.NGRAMS)
      .then(TOKENIZATION_PIPELINE_STAGES.STOPWORDS)
      // björn -> bjorn, allé -> alle, etc.
      .then(([tokens, field, ops]) => [
        tokens.map(stemmer),
        field,
        ops
      ])
      .then(TOKENIZATION_PIPELINE_STAGES.SCORE_TERM_FREQUENCY)
      .then(([tokens, field, ops]) => tokens)
})
```


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
the corresponding documents that are indexed, it may make sense to set
`storeRawDocs: false` when indexing (making indexing slightly faster),
and instead add them with `PUT_RAW` afterwards.


## QUERY

### Running queries

`QUERY` is a function that allows you to run queries on the search
index. It is called with a query object and returns a Promise:

```javascript
const results = await QUERY(query, options)
```

`options` is an optional object that can contain the following properties:

| Name | Type | Default | Description |
|---|---|---|---|
| [`BUCKETS`](#buckets-1) | `Array` | `[]` | Aggregate on user defined buckets |
| [`DOCUMENTS`](#documents) | `boolean` | `false` | If `true` return entire document, if not `true` return reference to document|
| [`FACETS`](#facets-1) | `Array` | `[]` | Aggregate on fields in the index |
| [`PAGE`](#page) | `object` | `{ NUMBER: 0, SIZE: 20 }` | Pagination |
| [`PIPELINE`](#pipeline) | `object` | `token => new Promise(resolve => resolve(token))` | Query tokenization pipeline |
| [`SCORE`](#score) | `String` | `'TFIDF'` | Calculate a value per document |
| [`SORT`](#sort) | `object` | `{ TYPE: 'NUMERIC', DIRECTION: 'DESCENDING', FIELD: '_score' }` | Sort documents |
| [`WEIGHT`](#weight) | `Array` | `[]` | Weight fields and/or values |

#### Returning references or documents

`QUERY` can return both refences to documents and the documents
themselves.

References are returned by default. To return documents, pass the
[`DOCUMENTS`](#DOCUMENTS) option:

```javascript
    const results = await QUERY(query, { DOCUMENTS: true })
```

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

Results can be paginated with [SCORE](#score), [SORT](#sort) and [PAGE](#page)

```javascript
// Example: get the second page of documents ordered by price
QUERY({
  FIELD: 'price'           // Select all documents that have a 'price'
}, {
  SCORE: 'SUM',            // Score on the sum of the price field
  SORT: {
    TYPE: 'NUMERIC',       // sort numerically, not alphabetically
    DIRECTION: 'ASCENDING' // cheapest first
  },
  PAGE: {
    NUMBER: 1,             // '1' is the second page (pages counted from '0')
    SIZE: 20               // 20 results per page
  }
})
```


### Query options


#### BUCKETS

See also [BUCKETS](#buckets)

```javascript
// Return the IDs of documents for each given token filtered by the
// query result
{
  BUCKETS: [ token1, token2, /* ... */ ]
}
```

#### DOCUMENTS

```javascript
// Returns full documents instead of just metadata.
{
  DOCUMENTS: true
}
```


#### FACETS

See also [FACETS](#facets)

```javascript
// Return document ids for each distinct field/value combination for
// the given token space, filtered by the query result.
{
  FACETS: token
}
```


#### PAGE

```javascript
// show a single page of the result set
{
  PAGE: {
    NUMBER: pageNumber, // to count from the end of the result set
                        // use negative numbers
    SIZE: pageSize
  }
}
```


#### PIPELINE

```javascript
// Alter a token on the way into a query
{
  PIPELINE: token =>
    new Promise(resolve => {
      // swap out all "ø" with "o"
      token.VALUE.GTE = token.VALUE.GTE.replace(/ø/g, 'o')
      token.VALUE.LTE = token.VALUE.LTE.replace(/ø/g, 'o')
      return resolve(token)
    })
}
```


#### SCORE

```javascript
// show a single page of the result set
{
  SCORE: scoreType // can be 'TFIDF', 'SUM, 'PRODUCT' or 'CONCAT'
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
```

#### WEIGHT

```javascript
// Weights fields and/or values
{
  WEIGHT: [{
    FIELD: fieldName,     // Name of field (matches all field if not present)
    VALUE: fieldValue,    // Value of field (matches all values if not present)
    WEIGHT: weight        // A numeric factor that weights the field/value
  }, /* ... more weights here if required... */ ]
}
```

### Query verbs

#### ALL_DOCUMENTS

```javascript
// returns all documents. Use PAGE to limit how many you see
{
  ALL_DOCUMENTS: true
}
```


#### AND

```javascript
// Boolean AND: Return results that contain all tokens
{
  AND: [ token1, token2, /* ... */ ]
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
  OR: [ token1, token2, /* ... */ ]
}
```


## SEARCH

```javascript
// equivalent to
// QUERY(q, {
//   SCORE: 'TFIDF',
//   SORT: true
// })
const results = await SEARCH(q)
```


## TOKENIZATION_PIPELINE_STAGES

Tokenization pipeline stages can be added, removed or reordered when
`PUT`ing by passing them to the `tokenizationPipeline` option.

Use this functionality when processing text on the way into the
index. Typical tasks would be to add stemming, synonym replacement,
character normalisation, or phone number normalisation.

It is possible to create your own tokenization pipeline stage. See the
[PUT section](#tokenization-pipeline) for more info

| Name | Description |
|---|---|
| SKIP | Skip these fields |
| LOWCASE | Bump all tokens to lower case |
| NGRAMS | create ngrams |
| SCORE_TERM_FREQUENCY | Score frequency of terms |
| REPLACE | Replace terms with other terms (synonyms) |
| SPLIT | Splits string into tokens (note: this is always the first stage, and `tokens` is a string rather than an array)|
| SPY | print output from precending stage to `console.log` |
| STOPWORDS | remove stopwords |


test
