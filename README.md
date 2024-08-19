# search-index

### A network resilient, persistent full-text search library for the browser and Node.js

[![npm](https://img.shields.io/npm/v/search-index.svg?style=flat-square)](https://www.npmjs.com/package/search-index)
[![npm](https://img.shields.io/npm/dm/search-index.svg?style=flat-square)](https://npm-stat.com/charts.html?package=search-index)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg?style=flat-square)](LICENCE)
[![Build Status](https://travis-ci.org/fergiemcdowall/search-index.svg?branch=master)](https://travis-ci.org/fergiemcdowall/search-index)
[![JavaScript Style Guide](https://img.shields.io/badge/code_style-standard-brightgreen.svg?style=flat-square)](https://standardjs.com)


## Quick start

```javascript
import { SearchIndex } from 'search-index' 

// initialize an index
const { PUT, QUERY } = new SearchIndex(options)

// add documents to the index
await PUT(documents)

// read documents from the index
const results = await QUERY(query)
```

## Documentation

* [Browser
  demo](https://codesandbox.io/p/github/fergiemcdowall/search-index-demo/)
  ([source code](https://github.com/fergiemcdowall/search-index-demo))
* [API](docs/API.md)
* [FAQ](docs/FAQ.md)
