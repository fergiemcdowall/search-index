**Table of Contents**  *generated with [DocToc](http://doctoc.herokuapp.com/)*

- [Search-index](#user-content-search-index)
- [The Forage Search Engine](#user-content-the-forage-search-engine)
- [Features](#user-content-features)
- [Installation](#user-content-installation)
- [Usage](#user-content-usage)
- [API](#user-content-api)
	- [si.add(batch, batchName, filters, [,callback])](#user-content-siaddbatch-batchname-filters-callback)
	- [si.del(docID [,callback])](#user-content-sideldocid-callback)
	- [si.get(docID [,callback])](#user-content-sigetdocid-callback)
	- [si.search(query, [,callback])](#user-content-sisearchquery-callback)
	- [si.generateMatcher([callback])](#user-content-sigeneratematchercallback)
	- [si.matcher(beginsWith, [callback])](#user-content-simatcherbeginswith-callback)
	- [si.getIndexMetadata([,callback])](#user-content-sigetindexmetadatacallback)
- [Query Parameters](#user-content-query-parameters)
	- [query](#user-content-query)
	- [offset](#user-content-offset)
	- [pageSize](#user-content-pagesize)
	- [facets](#user-content-facets)
	- [teaser](#user-content-teaser)
	- [weight](#user-content-weight)
	- [filter](#user-content-filter)
- [License](#user-content-license)

Search-index
============

[![NPM](https://nodei.co/npm/search-index.png?stars&downloads)](https://nodei.co/npm/search-index/)

[![NPM](https://nodei.co/npm-dl/search-index.png)](https://nodei.co/npm/search-index/)

http://npm-stat.vorba.ch/charts.html?package=search-index

[![Build Status](https://secure.travis-ci.org/fergiemcdowall/search-index.png)](http://travis-ci.org/fergiemcdowall/search-index)

Search-index is a search index module for Node.js. Think "node version
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


#The Forage Search Engine

Search-index is currently the index powering the [Forage search
engine](https://github.com/fergiemcdowall/forage).


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


#API


##si.add(batch, batchName, filters, [,callback])

Insets document into the index


```javascript
si.add(batch, batchName, filters, function(msg) {
  res.send(msg);
});
```

Where `batch` is a JSON sequence named `batchName` containing
one or more documents in a format similar to:

```javascript
{
  'doc1':{
    'title':'A really interesting document',
    'body':'This is a really interesting document',
    'metadata':['red', 'potato']
  },
  'doc2':{
    'title':'Another interesting document',
    'body':'This is another really interesting document that is a bit different',
    'metadata':['yellow', 'potato']
  }
}
```

...and `filters` is an array of field names that may be contained
in the document that the index will use for building filters. A filter
field must always be an array of single String tokens, for example
`['metadata','places']`.


##si.del(docID [,callback])

Delete the document and all associated index entries.

```javascript
si.del(docID, function(msg) {
  console.log(msg);
});
```

##si.get(docID [,callback])

Get the document and all associated index entries.

```javascript
si.get(docID, function(msg) {
  console.log(msg);
});
```


##si.search(query, [,callback])

Queries the search index

```javascript
si.search(query, function(msg) {
  res.send(msg);
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


##si.generateMatcher([callback])

Creates a matcher suitable for typeahead or autosuggest boxes. A
matcher is a service that generates a dictionary of words based on the
contents of the index, and then returns appropriate sets based on
substrings. The matcher is used by calling `si.matcher`. For example,
"lon" might return ['London', 'longing', 'longitude'].

```javascript
si.generateMatcher(function(msg) {
  console.log(msg);
});
```

##si.matcher(beginsWith, [callback])

A matcher is a service that generates a dictionary of words based on
the contents of the index, and then returns appropriate sets based on
substrings. For example, once the matcher is generated, a `beginsWith`
of "lon" might return ['London', 'longing', 'longitude'] depending on
the contents of the index.

```javascript
si.matcher(beginsWith, function(suggestion) {
  console.log(suggestions);
});
```


##si.getIndexMetadata([,callback])

Returns metadata about the state of the index.

```javascript
si.indexData(function(msg) {
  console.log(msg);
});
```


#Query Parameters

##query

A free text string containing one or many tokens. `*` is
equivalent to 'search all fields'

```javascript
    "query": {'*':["usa"]}
```

You can also specify named fields like so :

```javascript
    "query": {
      "title":["usa, reagan"]
    }
```

note: currently you can only limit to one field

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



