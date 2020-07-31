# search-index

### A network resilient, persistent full-text search library for the browser and Node.js

[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg?style=flat-square)](https://gitter.im/fergiemcdowall/search-index)
[![npm](https://img.shields.io/npm/v/search-index.svg?style=flat-square)](https://www.npmjs.com/package/search-index)
[![npm](https://img.shields.io/npm/dm/search-index.svg?style=flat-square)](https://npm-stat.com/charts.html?package=search-index)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg?style=flat-square)](LICENCE)
[![Build Status](https://travis-ci.org/fergiemcdowall/search-index.svg?branch=master)](https://travis-ci.org/fergiemcdowall/search-index)
[![JavaScript Style Guide](https://img.shields.io/badge/code_style-standard-brightgreen.svg?style=flat-square)](https://standardjs.com)


## Quick start

```javascript

const si = require('search-index')
// or
// import si from search-index

si().then(async index => {
  await index.PUT([ /* an array of objects */ ])
  // objects are now indexed
  const results = await index.SEARCH( /* a search string */ )
  // results contains search results
})

```

## Documentation

* [API](docs/API.md)
* [FAQ](docs/FAQ.md)
* [Node demo](docs/README.md)
* [Browser demo](demo/index.html) and [source code](demo/)

