# search-index

### A network resilient, persistent full-text search library for the browser and Node.js

[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg?style=flat-square)](https://gitter.im/fergiemcdowall/search-index)
[![npm](https://img.shields.io/npm/v/search-index.svg?style=flat-square)](https://www.npmjs.com/package/search-index)
[![npm](https://img.shields.io/npm/dm/search-index.svg?style=flat-square)](https://npm-stat.com/charts.html?package=search-index)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg?style=flat-square)](LICENCE)
[![Build Status](https://travis-ci.org/fergiemcdowall/search-index.svg?branch=master)](https://travis-ci.org/fergiemcdowall/search-index)
[![JavaScript Style Guide](https://img.shields.io/badge/code_style-standard-brightgreen.svg?style=flat-square)](https://standardjs.com)


## Documentation

* [API](https://github.com/fergiemcdowall/search-index/tree/master/docs/API.md)
* [FAQ](https://github.com/fergiemcdowall/search-index/tree/master/docs/API.md#faq)
* [Example code](https://github.com/fergiemcdowall/search-index/tree/master/docs/README.md)
* [Browser demo](https://fergiemcdowall.github.io/search-index/demo/) and [short explanation](https://github.com/fergiemcdowall/search-index/tree/master/demo/)


## Quick start

### Initialise search-index

#### Default method

`search-index` can be invoked with ES6 `import` or commonjs `require`
using either lazy loading or a callback:

```javascript
// Make a new index, or open an existing one with this name
import si from 'search-index'

// "lazy load"- db may not be immediately initialized
db = si({ name: 'mySearchIndex' })

// ... or callback to be sure you have it in time
si({ name: 'myDB' }, (err, db) => {
  // db is guaranteed to be open and available
})
```

#### Script tag method

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

### Add documents to index

```javascript
// db exists in a leveldb instance if run on a server
db.PUT([ /* my array of objects */ ]).then(doStuff)
```

### Search the index

```javascript
// search for terms without specifing any fields
db.SEARCH('SCOTLAND', 'GREEN').then(result)

```


## More examples

See the [tests](https://github.com/fergiemcdowall/search-index/tree/master/test) for more examples.
