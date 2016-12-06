# search-index
### A streaming, network resilient, persistent full-text search library for the browser and Node.js

[![Join the chat at https://gitter.im/fergiemcdowall/search-index][gitter-image]][gitter-url]
[![NPM version][npm-version-image]][npm-url]
[![NPM downloads][npm-downloads-image]][npm-url]
[![MIT License][license-image]][license-url]
[![Build Status][travis-image]][travis-url]
[![js-standard-style][js-standard-image]][js-standard-url] 

```javascript
const getData = function(err, myIndex) {
  readStreamOfDocuments                   // <- a stream of documents to be indexed
    .pipe(myIndex.defaultPipeline())      // <- an extentable document processing pipeline
    .pipe(myIndex.add())                  // <- myIndex is a search index that can now be queried
}
require('search-index')(options, getData) // <- make a new index
```

`search-index` is a freetext search library for JavaScript. You can use it to drop fantabulous search functionality into your javascript applications

Find out how to use the `search-index` module here:

### For the impatient
 * [Quickstart](./doc/quickstart.md)
 * [Simple web demo](https://rawgit.com/fergiemcdowall/search-index/master/doc/demo/index.html)

### API

 * [API reference](./doc/API.md)

### Documentation
 * [Create an index](./doc/create.md)
 * [Add documents](./doc/add.md)
 * [Search documents](./doc/search.md)
 * [Aggregate documents (buckets and categories)](./doc/aggregations.md)
 * [Set up autocomplete and query suggestions](./doc/autosuggest.md)
 * [Sync an index](./doc/replicate.md)
 * [Run search-index in the browser](./doc/browser.md)
 * [Sync from a server to a browser](./doc/browserSync.md)
 * [Work with Gulp.js](./doc/gulp.md)

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

* 0.7.0 : new constructor
* 0.8.0 : new search API
* 0.9.0 : Streaming API

[license-image]: http://img.shields.io/badge/license-MIT-blue.svg?style=flat-square
[license-url]: LICENSE

[npm-url]: https://npmjs.org/package/search-index
[npm-version-image]: http://img.shields.io/npm/v/search-index.svg?style=flat-square
[npm-downloads-image]: http://img.shields.io/npm/dm/search-index.svg?style=flat-square

[travis-url]: http://travis-ci.org/fergiemcdowall/search-index
[travis-image]: http://img.shields.io/travis/fergiemcdowall/search-index.svg?style=flat-square

[gitter-url]: https://gitter.im/fergiemcdowall/search-index?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge
[gitter-image]: https://img.shields.io/badge/GITTER-join%20chat-green.svg?style=flat-square

[js-standard-url]: https://github.com/feross/standard
[js-standard-image]: https://img.shields.io/badge/code%20style-standard%20js-green.svg?style=flat-square

