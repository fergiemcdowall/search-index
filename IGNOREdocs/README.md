<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
# Quickstart

- [Initializing search-index](#initializing-search-index)
  - [Default method](#default-method)
  - [Script tag method](#script-tag-method)
  - [Using something else than default db](#using-something-else-than-default-db)
- [Add content](#add-content)
- [Search the index](#search-the-index)
  - [Standard search queries](#standard-search-queries)
  - [Using boolean expressions (AND, OR, NOT)](#using-boolean-expressions-and-or-not)
  - [Combine standard search with boolean OR](#combine-standard-search-with-boolean-or)
  - [Splitting a multiple word query the right way](#splitting-a-multiple-word-query-the-right-way)
- [Autocomplete / autosuggest](#autocomplete--autosuggest)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->


<a name="initializing"></a>

# Initializing search-index

<a name="init-default"></a>
## Default method

`search-index` can be invoked with ES6 `import` or commonjs `require`
using either lazy loading or a callback:

```javascript
// call the library
import si from 'search-index'

// Make a new index, or open an existing one with this name
// (note that si() returns a promise so if calling from a non-async
// environment where you cant do "await" you may have to do
// si().then(db => { /* stuff... */ })
const db = await si({ name: 'mySearchIndex' })

// db exists in a leveldb instance if run on a server, and an
// indexedDB instance if run in a browser
db.PUT([ /* my array of objects */ ]).then(doStuff)

```

<a name="init-scripttag"></a>

## Script tag method

In the `/dist` folder there is a file called
`search-index.<version>.js` that can be used as a standalone in a
`<script>` tag. The library is then available under a global variable
called `searchIndex`:

```html
<script type='text/javascript' src='./search-index.1.0.2.js'></script>
<script type='text/javascript'>
  import si from search-index
  // ...
</script>

```

<a name="init-switchdb"></a>

## Using something else than default db

For node.js, levelDB is the default db and for the browser it is indexedDB. If you want to use something else, you can. 

```javascript
import encode from 'encoding-down'
import fii from 'fergies-inverted-index'
import levelup from 'levelup'
import memdown from 'memdown'
import si from '../../dist/search-index.esm.js'


levelup(encode(memdown('myDB'), {
  valueEncoding: 'json'
}), (err, store) => {
  if (err) return console.error(err)
  let db = si({
    fii: fii({ store: store })
  })
  // db is now available
})

// Your DB will now be stored in memory only
// Nice for i.e. mocking a DB when developing
```


<a name="adding"></a>

# Add content

wip


<a name="search"></a>

# Search the index

The `db.SEARCH()` is the same as `db.AND().then(db.DOCUMENTS)`. It's a quick and standard way to get documents back from the index. If you want to do boolean search with any of the AND, OR or NOT or a combination, you have to chain it with a `.then(DOCUMENTS)` to get the matching documents to the IDs you've retrieved. 

<a name="search-standard"></a>

## Standard search queries
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
```

<a name="query-boolean"></a>
         
## Using boolean expressions (AND, OR, NOT)

The boolean search operators AND, OR and NOT will only give you id's back, so you need to chain it with a `.then(DOCUMENTS)` to get actual documents for those id's back.

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
  AND('colour:GREEN', 'colour:RED')    // minus everything in this set
).then(result)

```

<a name="combine-standard-or"></a>

## Combine standard search with boolean OR

```javascript
// SEARCH combined with boolean OR 
SEARCH(
  OR('SCOTLAND', 'IRELAND'),
  'GREEN'
).then(result)
// (these queries can be as deeply nested as required)
```

<a name="search-querysplit"></a>

## Splitting a multiple word query the right way

search-index store words, so searching for a string containing many words won't work. You need to split your query into a row of single words. 

You can't do this:
```javascript
let queryString = 'interesting query'
idx.SEARCH(queryString.split(' '))

// Feeds an array ['interesting','query'] to the searh API and will never return any results
```

So, then you need to use the [spread syntax](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax):

```javascript
let queryString = 'interesting query'
idx.SEARCH(...queryString.split(' '))

// will input 'interesting','query' to the search API and will return results with the words 'interesting' and 'query' in them.
```


<a name="autocomplete"></a>

# Autocomplete / autosuggest

For that you would use `DICTIONARY`. It can give you all tokens in the index, or tokens that are greater than and/or less than a given value.

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

If you want to do fancy stuff with levenstein distance, or other forms of fuzzy matching, then you can use `DICTIONARY` to export tokens into a third party lib of your choice.
