Search-index
============

Search-index is a search index module for Node.js. Think "node version of Lucene, but much simpler".

Search-index is built with the [soooperfast levelUP module](https://github.com/rvagg/node-levelup), and the
[very useful Natural module](https://github.com/NaturalNode/natural).

The Point of Search-Index is to simplify set up and operation of an search engine. Search-index is essentially
free from configuration- the index is dynamic and morphs into the structure that you require automatically, based on the
documents that it is fed.

Search-index is in an alpha stage- meaning that it has been known to work quite well, but edge cases and portability
may be challenging. Query-result is robust and sometimes indexing requires hand-holding. See known issues and performance
tips below.

#The Norch Search Engine

Search-index is currently the index powering the [Norch search engine](https://github.com/rvagg/node-levelup).

#Features

* Full text search
* Stopword removal
* Faceting
* Filtering
* Relevance weighting (tf-idf)
* Field weighting
* Paging (offset and resultset length)

#Installation

The easiest way to include search-index in your project is to to it via npm

    npm install search-index
    
The module can then be initialised by including the line

    si = require('search-index');
    
at the top of your app.

#API

##si.indexData([,callback])

Returns metadata about the state of the index. Metadata is accrued incrementally, so it is vulnerable to corruption.
At any time metadata can be corrected by running the computationally demanding calibrate function.

```javascript
si.indexData(function(msg) {
  console.log(msg);
});
```

##si.calibrate([,callback])

Iterate through entire index and count everything up. Tf-idf calculations are most precise when indexData is up to date.
Only needs to be called if index has been altered externally, or if key collisions occur (mostly theoretical). A 
persistant running tally is kept by search-index which can be seen in the file norchindex.json.

```javascript
si.calibrate(function(msg) {
  console.log(msg);
});
```

##si.indexPeek(start, stop [,callback])

Take a look at the raw index. Start is the start point and stop is the stop point. All keys in between will be returned.
For debug purposes.

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

...where query is an object similar to:

```javascript
    {
    "query": [
      "usa"
    ],
    "offset": "0",
    "pagesize": "20",
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


##si.index(batch, filters, [,callback])

Insets document into the index

```javascript
si.index(batch, filters, function(msg) {
  res.send(msg);
});
```

Where ```batch``` is a JSON file containing one or more documents and formatted similar to:

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

...and ```filters``` is an array of field names that may be contained in the document that the index will use for
building filters. A filter field must always be an array of single String tokens, for example ```['metadata','places']```


    
    
