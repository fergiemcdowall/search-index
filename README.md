# search-index
### A streaming, network resilient, persistent full-text search library for the browser and Node.js

[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg?style=flat-square)](https://gitter.im/fergiemcdowall/search-index)
[![npm](https://img.shields.io/npm/v/search-index.svg?style=flat-square)](https://www.npmjs.com/package/search-index)
[![npm](https://img.shields.io/npm/dm/search-index.svg?style=flat-square)](https://npm-stat.com/charts.html?package=search-index)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg?style=flat-square)](LICENCE)
[![Travis](https://img.shields.io/travis/rust-lang/rust.svg?style=flat-square)](https://travis-ci.org/fergiemcdowall/search-index)
[![JavaScript Style Guide](https://img.shields.io/badge/code_style-standard-brightgreen.svg?style=flat-square)](https://standardjs.com)

```javascript
const getData = function(err, myIndex) {
  readStreamOfDocuments        // <- a stream of documents to be indexed
    .pipe(myIndex.feed())      // <- an extendable document processing pipeline (do objectMode: true for a stream of objects)
}
require('search-index')(options, getData) // <- make a new index
```

`search-index` is a freetext search library for JavaScript. You can use it to drop fantabulous search functionality into your javascript applications

Find out how to use the `search-index` module here:

### For the impatient
 * [Quickstart](./docs/quickstart.md)
 * [Simple web demo](https://fergiemcdowall.github.io/search-index/demo/)

### API

 * [API reference](./docs/API.md)

### Documentation
 * [Create an index](./docs/create.md)
 * [Add documents](./docs/add.md)
 * [Search documents](./docs/search.md)
 * [Aggregate documents (buckets and categories)](./docs/aggregations.md)
 * [Set up autocomplete and query suggestions](./docs/autosuggest.md)
 * [Sync an index](./docs/replicate.md)
 * [Run search-index in the browser](./docs/browser.md)
 * [Sync from a server to a browser](./docs/browserSync.md)
 * [Work with Gulp.js](./docs/gulp.md)

<!--
### Other How-tos and Articles on the Interwebs
 * [Getting started with search-index](#)
 * [How to implement stemming in search-index](#)
 * [How to implement synonyms in search-index](#)
 * [Create a Network-Resiliant Search Application](#)
 * [Functionality vs index size (how to shrink your index)](#)
 * [Teasers and Search Term Highlighting](#)
-->
### Release notes

 * See [releases page](https://github.com/fergiemcdowall/search-index/releases) on GitHub

### Contribute

 * [Contribution guidelines](./docs/contributing.md)
