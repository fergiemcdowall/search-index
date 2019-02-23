# Documentation

* <a href="#initializing"><b>Initializing search-index</b></a>
* <a href="#adding"><b>Adding content</b></a>
* <a href="#searching"><b>Searching</b></a>
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

<a name="adding"></a>

## Adding content

wip

<a name="searching"></a>

## Searching

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