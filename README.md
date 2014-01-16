Search-index
============

[![NPM](https://nodei.co/npm/search-index.png?stars&downloads)](https://nodei.co/npm/search-index/)

[![NPM](https://nodei.co/npm-dl/search-index.png)](https://nodei.co/npm/search-index/)

http://npm-stat.vorba.ch/charts.html?package=search-index

[![Build Status](https://secure.travis-ci.org/fergiemcdowall/search-index.png)](http://travis-ci.org/fergiemcdowall/search-index)

Search-index is a search index module for Node.js. Think "node version
of Lucene, but much simpler".

Search-index allows you to perform free-text search over structured or unstructured data, and return
a resultset ordered by relevance.

Search-index is built with the [soooperfast levelUP module](https://github.com/rvagg/node-levelup), and the
[very useful Natural module](https://github.com/NaturalNode/natural).

The Point of Search-Index is to simplify set up and operation of an
search engine. Search-index is essentially free from configuration-
the index is dynamic and morphs into the structure that you require
automatically, based on the documents that it is fed.


Search-index is in an alpha stage- meaning that it has been known to
work quite well, but edge cases and portability may be
challenging. Query-result is robust. See known issues and performance
tips below.


#The Forage Search Engine

Search-index is currently the index powering the [Forage search engine](https://github.com/fergiemcdowall/forage).


#Features

* Full text search
* Stopword removal
* Faceting
* Filtering
* Fielded search
* Field weighting
* Relevance weighting (tf-idf)
* Paging (offset and resultset length)

#Installation

Releases are listed [here](https://github.com/fergiemcdowall/search-index/releases). Generally you will want the most recent one.

The easiest way to include search-index in your project is by using npm

    npm install search-index
    
The module can then be initialised by including the line

    si = require('search-index');
    
at the top of your app.

#API

##si.indexData([,callback])

Returns metadata about the state of the index. Metadata is accrued
incrementally, so it is vulnerable to corruption. At any time metadata
can be corrected by running the computationally demanding calibrate
function.

```javascript
si.indexData(function(msg) {
  console.log(msg);
});
```

##si.calibrate([,callback])

Iterate through entire index and count everything up. Tf-idf
calculations are most precise when indexData is up to date. Only needs
to be called if index has been altered externally, or if key
collisions occur (mostly theoretical). A persistant running tally is
kept by search-index which can be seen in the file search-index.json.


```javascript
si.calibrate(function(msg) {
  console.log(msg);
});
```

##si.indexPeek(start, stop [,callback])

Take a look at the raw index. Start is the start point and stop is the
stop point. All keys in between will be returned. For debug purposes.

```javascript
si.indexPeek(req.query['start'], req.query['stop'], function(msg) {
  console.log(msg);
});
```

##si.deleteDoc(req.body.docID [,callback])

Delete the document and all associated index entries.

```javascript
si.deleteDoc(documentID, function(msg) {
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
    "query": [
      "usa"
    ],
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


##si.index(batch, batchName, filters, [,callback])

Insets document into the index


```javascript
si.index(batch, batchName, filters, function(msg) {
  res.send(msg);
});
```

Where ```batch``` is a JSON sequence named ```batchName``` containing one or more documents in a
format similar to:

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

...and ```filters``` is an array of field names that may be contained
in the document that the index will use for building filters. A filter
field must always be an array of single String tokens, for example ```['metadata','places']```



#Query Parameters

##query

A free text string containing one or many tokens. Search everything with ´´´´*´´´

```javascript
    "query": [
      "usa"
    ]
```

##searchFields

Fields to search in. Query will only search in the fields specified here.

```javascript
    "searchFields": [
      "title"
    ]
```

##offset

The starting point in the resultset of the results returned. Useful
for paging

```javascript
    "offset": 0
```

##pageSize

The maximum number of results in the resultset that can be
returned. Counted from ```offset```

```javascript
    "pageSize": 20
```

##facets

Allows faceted navigation, the parameter is an array of fields. If no
```facets``` is sent, all possible facets are returned.

```javascript
    "facets": [
      "topics",
      "places"
    ]
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



