# Documentation

* <a href="#initializing"><b>Initializing search-index</b></a>
* <a href="#db"><b>Using something else than default db</b></a>
* <a href="#adding"><b>Adding content</b></a>
* <a href="#search"><b>Search the index</b></a>
* <a href="#query-boolean"><b>Query the index using boolean expressions (AND, OR, NOT)</b></a>
* <a href="#autocomplete"><b>Autocomplete / autosuggest</b></a>

<a name="initializing"></a>

## Initializing search-index

### Default method

`search-index` can be invoked with ES6 `import` or commonjs `require`
using either lazy loading or a callback:

```javascript
// Make a new index, or open an existing one with this name
import si from 'search-index'

// "lazy load"- db may not be immediately initialized
db = si({ name: 'mySearchIndex' })

// db exists in a leveldb instance if run on a server, and an
// indexedDB instance if run in a browser
db.PUT([ /* my array of objects */ ]).then(doStuff)

```

### Script tag method

In the `/dist` folder there is a file called
`search-index.<version>.js` that can be used as a standalone in a
`<script>` tag. The library is then available under a global variable
called `searchIndex`:

```html
<script type='text/javascript' src='./search-index.1.0.2.js'></script>
<script type='text/javascript'>
  searchIndex({ name: 'myDB' }, (err, db) => {
    // db is now available
  })
</script>

```

<a name="db"></a>

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
  t.error(err)
  db = si({
    fii: fii({ store: store })
  })
})

// Your DB will now be stored in memory only
// Nice for i.e. mocking a DB when developing
```


<a name="adding"></a>

## Adding content

wip


<a name="search></a>

## Search the index

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

// or combine with boolean expressions (see below)
SEARCH(
  OR('SCOTLAND', 'IRELAND'),
  'GREEN'
).then(result)
// (these queries can be as deeply nested as required)
```


<a name="query-boolean></a>

## Query the index using boolean expressions (AND, OR, NOT)

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
  AND('colour:GREEN', 'colour:RED').    // minus everything in this set
).then(result)

```


wip

<a name="autocomplete"></a>

## Autocomplete / autosuggest

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