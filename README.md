Search-index
============

search-index is a search index module for Node.js. Think "node version of Lucene, but much simpler".

search-index is built with the [soooperfast levelUP module](https://github.com/rvagg/node-levelup), and the
[very useful Natural module](https://github.com/NaturalNode/natural).

The Point of Search-Index is to simplify set up and operation of an search engine. Search-index is essentially
free from configuration- the index is dynamic and morphs into the structure that you require automatically, based on the
documents that it is fed.

Search-index is in an alpha stage- meaning that it has been known to work quite well, but edge cases and portability
may be challenging. Query-result is robust, sometimes indexing requires hand-holding. See known issues and performance
tips below.

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

    si.indexData(function(msg) {
      console.log(msg);
    });
    
##si.calibrate([,callback])

    si.calibrate(function(msg) {
      console.log(msg);
    });

##si.indexPeek(start, stop [,callback])

    si.indexPeek(req.query['start'], req.query['stop'], function(msg) {
      console.log(msg);
    });

##si.deleteDoc(req.body.docID [,callback])

    si.deleteDoc(documentID, function(msg) {
      console.log(msg);
    });

##si.search(query, [,callback])

    si.search(query, function(msg) {
      res.send(msg);
    });

##si.index(batch, filters, [,callback])

    si.index(batch, filters, function(msg) {
      res.send(msg);
    });
    
    
    
