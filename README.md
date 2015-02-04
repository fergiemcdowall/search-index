[![NPM version][npm-version-image]][npm-url] [![NPM downloads][npm-downloads-image]][npm-url] [![MIT License][license-image]][license-url] [![Build Status][travis-image]][travis-url]

**Table of Contents**  *generated with [DocToc](http://doctoc.herokuapp.com/)*

- [Search-index](#user-content-search-index)
- [The Norch Search Engine](#user-content-the-norch-search-engine)
- [Features](#user-content-features)
- [Installation](#user-content-installation)
- [Usage](#user-content-usage)
- [Initialization](#user-content-initialization)
- [API](#user-content-api)
	- [si.add](#user-content-siadd)
	- [si.del](#user-content-sidel)
	- [si.get](#user-content-siget)
	- [si.search](#user-content-sisearch)
	- [si.match](#user-content-simatch)
	- [si.tellMeAboutMySearchIndex](#user-content-sitellmeaboutmysearchindex)
	- [si.empty](#user-content-siempty)
	- [si.snapshot](#user-content-sisnapshot)
	- [si.replicate](#user-content-sireplicate)
- [Query Parameters](#user-content-query-parameters)
	- [query](#user-content-query)
	- [offset](#user-content-offset)
	- [pageSize](#user-content-pagesize)
	- [facets](#user-content-facets)
	- [facetLength](#user-content-facetlength)
	- [facetSort](#user-content-facetsort)
	- [teaser](#user-content-teaser)
	- [weight](#user-content-weight)
	- [filter](#user-content-filter)
- [License](#user-content-license)



Search-index
============

[![NPM](https://nodei.co/npm/search-index.png?stars&downloads&downloadRank)](https://nodei.co/npm/search-index/)

[![NPM](https://nodei.co/npm-dl/search-index.png)](https://nodei.co/npm/search-index/)

http://npm-stat.com/charts.html?package=search-index

Search-index is a text search module for Node.js. Think "node version
of Lucene, but much simpler".

Search-index allows you to perform free-text search over structured or
unstructured data, and return a resultset ordered by relevance.

Search-index is built with the [soooperfast levelUP
module](https://github.com/rvagg/node-levelup), and the [very useful
Natural module](https://github.com/NaturalNode/natural).

The Point of Search-Index is to simplify set up and operation of an
search engine. Search-index is essentially free from configuration-
the index is dynamic and morphs into the structure that you require
automatically, based on the documents that it is fed.


Search-index is in an alpha stage- meaning that it has been known to
work quite well, but edge cases and portability may be
challenging. Query-result is robust. See known issues and performance
tips below.


#The Norch Search Engine

Search-index is currently the index powering the [Norch search
engine](https://github.com/fergiemcdowall/norch).


#Features

* Full text search
* Stopword removal
* Faceting
* Filtering
* Fielded search
* Field weighting
* Relevance weighting (tf-idf)
* Paging (offset and resultset length)
* Teasers

#Installation

Releases are listed
[here](https://github.com/fergiemcdowall/search-index/releases). Generally
you will want the most recent one.

The easiest way to include search-index in your project is by using npm

    npm install search-index
    
The module can then be initialised by including the line

    si = require('search-index');
    
at the top of your app.

#Usage

To make a searchable index, you must first add documents with `si.add`.

Documents are then searchable with `si.search`.

#Initialization

`search-index` is called with `require` like so:

```javascript
var si = require('search-index');
```

`search-index` can be initialized with `options` like so:

```javascript
var options = { indexPath: 'si2', logLevel: 'error', logSilent: false }
var si = require('search-index')(options)
```
**Available options**

* **indexPath** The physical location of the index on the filesystem. Default is `si`
* **logLevel** A winston log level like `info`, `debug` (lots of logs) or `error` (nearly silent). Default is `info`
* **logSilent** Set to true to suppress any log the search-index might do

#API

##si.add

Inserts document into the index


```javascript
si.add({'batchName': batchName, 'filters': filters}, data, function(err) {
  if (!err) console.log('indexed!');
});
```

Where `batchName` is any name to tag the batch, and `filters` tells the index which fields can be filtered and agregated on, and `data` is an object containing one or more documents in a format similar to:


```javascript
[
  {
    'id':'1'
    'title':'A really interesting document',
    'body':'This is a really interesting document',
    'metadata':['red', 'potato']
  },
  {
    'id':'2'
    'title':'Another interesting document',
    'body':'This is another really interesting document that is a bit different',
    'metadata':['yellow', 'potato']
  }
]
```

...and `filters` is an array of field names that may be contained
in the document that the index will use for building filters. A filter
field must always be an array of single String tokens, for example
`['metadata','places']`. 'search-index' wont accept strings, so
remember to wrap home-rolled JSON with 'JSON.parse' to turn it into an
object.

Example:
```javascript
var batch = [
  {
    'id':'1'
    'title':'A really interesting document',
    'body':'This is a really interesting document',
    'metadata':['red', 'potato']
  },
  {
    'id':'2'
    'title':'Another interesting document',
    'body':'This is another really interesting document that is a bit different',
    'metadata':['yellow', 'potato']
  }
];
var batchName = 'twoDocs';
var filters = ['metadata'];

si.add({'batchName': batchName, 'filters': filters}, data, function(err) {
  if (!err) console.log('indexed!');
});
```

Note: if you dont specify an id field, ```search-index``` will specify one for you.

##si.del

Delete the document and all associated index entries.

```javascript
si.del(docID, function(err) {
  if (!err) console.log('success!');
});
```

##si.get

Get the document and all associated index entries.

```javascript
si.get(docID, function(err, doc) {
  if (!err) console.log(doc);
});
```


##si.search

Queries the search index

```javascript
si.search(query, function(err, results) {
  //check for errors and do something with search results, for example this:
  if (!err) console.log(results)
});
```

...where query is an object similar to (see Query Parameters for more
info):

```javascript
    {
    "query": {
      "*": ["usa"]
    },
    "offset": "0",
    "pageSize": "20",
    "facets": [
      "places",
      "organisations"
    ],
    "weight": {
      "title": [
        "10"
      ]
    },
    "filter": {
      "places": [
        "usa"
      ]
    }
  }
```


##si.match

A matcher is a service that generates a dictionary of words based on
the contents of the index, and then returns appropriate sets based on
substrings. For example, once the matcher is generated, a `beginsWith`
of "lon" might return ['London', 'longing', 'longitude'] depending on
the contents of the index. Terms are ordered by total occurances in
index.

```javascript
si.matcher(beginsWith, function(err, matches) {
  if (!err) console.log(matches);
});
```


##si.tellMeAboutMySearchIndex

Returns metadata about the state of the index.

```javascript
si.tellMeAboutMySearchIndex(function(msg) {
  console.log(msg);
});
```


##si.empty

Empties the search index, can be used in conjunction with replication.

```javascript
si.empty(function(err) {
  if (!err) console.log("Emptied! Search-index now contains no documents- please refeed or replicate");
});
```


##si.snapshot

Returns a `readStream` that can then be piped on, for instance to file.

```javascript
//assumes that: var fs = require('fs')
si.snapShot(function(readStream) {
  readStream.pipe(fs.createWriteStream('backup.gz'))
    .on('close', function() {
    //a snapshot of the search-index now exists in the file 'backup.gz'
  });
});
```


##si.replicate

Replicates an index from a snapshot file generated by `si.snapshot`.

```javascript
//assumes that backup is in a file called 'backup.gz'
si.replicate(fs.createReadStream('backup.gz'), function(msg){
  that.completed = true;
});
```

#Query Parameters

##query

A free text string containing one or many tokens. `*` is
equivalent to 'search all fields'

```javascript
    "query": {"*":["usa"]}
```

You can also specify named fields like so :

```javascript
    "query": {
      "title":["usa", "reagan"]
    }
```

and so:

```javascript
    "query": {
      "title": ["reagan"],
      "body": ["intelligence", "agency", "contra"]
    }
```

##offset

The starting point in the resultset of the results returned. Useful
for paging

```javascript
    "offset": 0
```

##pageSize

The maximum number of results in the resultset that can be
returned. Counted from `offset`

```javascript
    "pageSize": 20
```

##facets

Allows faceted navigation, the parameter is an array of fields. If no
`facets` is sent, all possible facets are returned.

```javascript
    "facets": [
      "topics",
      "places"
    ]
```

##facetLength
Defines the amount of entries per facet category. Defaults to 10.

```javascript
    "facets": [
      "topics",
      "places"
    ],
    "facetLength": 50
```


##facetSort
Defines the sort order of facets. Facets can be sorted on keys or values in an ascending or descening order. Possible values for `facetSort` are `keyAsc`, `keyDesc`, `valueAsc`, and `valueDesc`. The default sort is equivalent to `valueDesc`.

```javascript
    "facets": [
      "topics",
      "places"
    ],
    "facetLength": 50,
    "facetSort": "keyAsc"
```


##teaser

Creates a field that shows where the search terms exist in the given
field. For example, a teaser field could be generated from the
document field `body`

```javascript
    "teaser": "body"
```

##weight

Sets a factor by which the score of a field should be weighted. Useful
for building custom relevancy models

```javascript
    "weight": {
      "body": [
        "10"
      ],
      "title": [
        "5"
      ]
    }
```

##filter

Used to return a resultset on a facet.

```javascript
    "filter": {
      "places": [
        "ussr"
      ]
    }
```

#License

Search-index is released under the MIT license:

Copyright (c) 2013 Fergus McDowall

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

[license-image]: http://img.shields.io/badge/license-MIT-blue.svg?style=flat
[license-url]: LICENSE

[npm-url]: https://npmjs.org/package/search-index
[npm-version-image]: http://img.shields.io/npm/v/search-index.svg?style=flat
[npm-downloads-image]: http://img.shields.io/npm/dm/search-index.svg?style=flat

[travis-url]: http://travis-ci.org/fergiemcdowall/search-index
[travis-image]: http://img.shields.io/travis/fergiemcdowall/search-index.svg?style=flat

