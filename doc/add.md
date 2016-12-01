# Add documents to a search index

This is the typical way to add documents to a search index that have
never been indexed before. If the documents already exist in another
search index, it may be more appropriate to [sync your
indices](replicate.md).

```javascript
request(url)                      // <- could also be something like fs.getReadStream
  .pipe(JSONStream.parse())
  .pipe(index.defaultPipeline())
  .pipe(index.add())
  .on('data', function(d) {
    // this function needs to be called if you want to listen for the end event
  })
  .on('end', function() {
    // complete
  })
```

Indexing options can be set at a batch, document, or field level.

## Prerequisites

First [instantiate a search-index](create.md)

## Data format

An example file can be found [here](https://raw.githubusercontent.com/fergiemcdowall/reuters-21578-json/master/data/fullFileStream/justTen.str)

Documents can be added in arbitrarily sized batches as a standard node
[readable
stream](https://nodejs.org/api/stream.html#stream_readable_streams). `search-index`'s
default processing pipeline expects a stream of contiguous javascript
objects, and will attempt to process them as key:value pairs, where
the key is a field name and the value is a field value

```javascript
{
  id: '1',
  body: 'this is a cool document'
}
{
  id: '2',
  body: 'No- wait! this document is even cooler!!'
}
```

## Get a Readable Stream

### From a URL

Use request and JSONStream

```javascript
const request = require('request')
const JSONStream = require('JSONStream')
const url = 'http://link.to.my/data'

request(url)
  .pipe(JSONStream.parse())
```

### From a File

Use fs and JSONStream

```javascript
const JSONStream = require('JSONStream')
const fs = require('fs')

fs.createReadStream('myDataFile.str')
  .pipe(JSONStream.parse())
```

### From Native JavaScript

Create a `Readable` stream and push documents to it.

```javascript
const Readable = require('stream').Readable
const s = new Readable( {objectMode: true} )

s.push({
  id: '3',
  body: 'this doc has a great body'
})
// add as many docs as you want

s.push(null)
// s is now ready to be piped
```
## Document Processing Pipeline

`.defaultPipeline()` uses [docProc](https://github.com/fergiemcdowall/docproc).

`.defaultPipeline()` converts these documents into a [special vector
format](https://github.com/fergiemcdowall/docproc). As you become more advanced, you may want to manipulate these
vectors in order to implement advanced features such as stemming or
synonyms. However `search-index` ships with a default processing
pipeline that allows you to send Plain Old JavaScript Objects to the
index, and they will be treated as "field name": "field value"

```javascript
dataStream                       // <- stream of docs to be indexed
  .pipe(index.defaultPipeline())    
                                 // <- for mad science- put further
                                 // transforms in here
```

The `.defaultPipeline()` takes several options relating to how the
documents can be searched. [Check out the API](API.md#defaultpipeline) for more
information.

## Adder

The `.add` is a writable stream that accepts the processed documents

```javascript
dataStream                       // <- stream of docs to be indexed
  .pipe(index.defaultPipeline())
  .pipe(index.add())
                                 // <- you must drain and end the stream here (see top of page)
```

