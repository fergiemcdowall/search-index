# search-index
### A persistent full text search engine for the browser and Node.js
[![NPM version][npm-version-image]][npm-url] [![NPM downloads][npm-downloads-image]][npm-url] [![MIT License][license-image]][license-url] [![Build Status][travis-image]][travis-url]

```javascript
var si = require('search-index')(options);

si.add(data, opt, function (err) {
  //add stuff to index
});

si.search(q, function (err, searchResults) {
  //search in index
});
```

`search-index` is a freetext search library for JavaScript. You can use it to drop kick-ass search functionality into your node.js, HTML, OSX and Android applications.

You can also generate search indexes and easily share them- you could for example make an index available on bittorrent, or you could push an index out to a browser. It is really easy to move indexes around, and create decentralised search engines.

`search-index` uses LevelDB as a backend via the [LevelUp](https://github.com/Level/levelup) interface. These days, LevelDB-ish databases are installed pretty much everywhere, which is why `search-index` is so portable.

Find out how to use the `search-index` module here:

### For the impatient
 * [Quickstart](./doc/quickstart.md)

### User Documentation
 * [Create an index](./doc/create.md)
 * [Add documents to an index](./doc/add.md)
 * [Search in an index](./doc/search.md)
 * [Matchers and autosuggest](./doc/autosuggest.md)
 * [Replicate an index](./doc/replicate.md)

### Examples
 * [Examples](doc/examples)

### API reference

 * [API reference](https://cdn.rawgit.com/fergiemcdowall/search-index/better_docs/doc/api/module-search-index.html)

[license-image]: http://img.shields.io/badge/license-MIT-blue.svg?style=flat
[license-url]: LICENSE

[npm-url]: https://npmjs.org/package/search-index
[npm-version-image]: http://img.shields.io/npm/v/search-index.svg?style=flat
[npm-downloads-image]: http://img.shields.io/npm/dm/search-index.svg?style=flat

[travis-url]: http://travis-ci.org/fergiemcdowall/search-index
[travis-image]: http://img.shields.io/travis/fergiemcdowall/search-index.svg?style=flat

