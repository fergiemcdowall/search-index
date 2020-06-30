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
    - [Composing queries](#composing-queries)
    - [Vocabulary](#vocabulary)
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

`QUERY` is a function that allows you to run queries on the search
index. It is called with a query object and returns a Promise:

```javascript
const results = await QUERY(queryObject)`
```

### Composing queries

### Vocabulary

#### AND
#### BUCKET
#### BUCKETFILTER
#### DICTIONARY
#### DISTINCT
#### DOCUMENTS
#### GET
#### NOT
#### OR
#### PAGE
#### SCORE
#### SEARCH
#### SORT


## UPDATE

`UPDATE` is a function that allows you to write to the search index

### DELETE
### PUT
