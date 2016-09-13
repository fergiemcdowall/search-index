# API reference

### Opening and Closing

 * [require('search-index')](#initialization)
 * [close(...)](#close)

### Reading

 * [buckets(...)](#buckets)
 * [categorize(...)](#categorize)
 * [get(...)](#get)
 * [match(...)](#match)
 * [search(...)](#search)
 * [tellMeAboutMySearchIndex(...)](#tellmeaboutmysearchindex)

### Writing

 * [add(...)](#add)
 * [defaultPipeline(...)](#defaultpipeline)
 * [del(...)](#del)
 * [flush(...)](#flush)

### Replication

 * [dbReadStream(...)](#dbreadstream)
 * [dbWriteStream(...)](#dbwritestream)



## Opening and Closing

### Initialization

Make sure that `search-index` is npm installed and then do either

```javascript
const searchIndex = require('search-index')
searchIndex(options, function(err, si) {
  // si is now a new search index
})
```

or

```javascript
require('search-index')(options, function(err, si) {
  // si is now a new search index
})
```

`options` is an object that can take the following values:

 * **db** _datastore, default:leveldown_ : a leveldown compatible datastore

 * **fieldedSearch** _boolean, default:true_ : Can you search on
     individual document fields? Setting this to false saves space and
     makes the index faster.

 * **indexPath** _string, default:'si'_ : what should the index be
     called on disk.

 * **logLevel** _string, default:'error'_ : a bunyan log level.

 * **nGramLength** _number, default:1_ : length of word sequences to
     be indexed. Use this to capture phrases of more than one word.

 * **separator** _RegExp, default:/[\|' \.,\-|(\n)]+/_ : A RegExp in
     the "string split" format

 * **stopwords** _Array, default:
     require('stopword').en_ : A list of
     words that are not considered for indexing. Typically they will
     be numbers, common words (english examples: 'is', 'a', 'and'), or
     otherwise "forbidden" words.



### close(...)

Closes the index and the underlying data store. Generally this should
happen automatically, and therefore this function need never be
called. Only needed if you are doing very fast restarts.

```javascript
index.close(function(err) {
  if (!err) console.log('success!')
})
```

      

## Reading

### buckets(...)

Return a readable stream of user defined aggregations, can be used to
generate categories by price, age, etc.

```javascript
  si.buckets({
    query: [
      {
        AND: {'*': ['*']}
      }
    ],
    buckets: [
      {
        field: 'price',
        gte: '2',
        lte: '3',
        set: false
      }
      {
        field: 'price',
        gte: '4',
        lte: '7',
        set: false
      }
    ]
  }).on('data', function (data) {
    // do something with the data
  }).on('end', function () {
    // finshed
  })
```


### categorize(...)

Collate documents under all possible values of the given field name,
and return a readable stream

```javascript
  si.categorize({
    query: [
      {
        AND: {'*': ['swiss', 'watch']}
      }
    ],
    category: {
      field: 'manufacturer'
    }
  }).on('data', function (data) {
    // do something with the data
  }).on('end', function () {
    // finshed
  })
```

### get(...)

Gets a document from the corpus

```javascript
index.get('docID', function(err) {
  if (!err) console.log('success!')
})
```

* **docID** is the ID of the document that should be retrieved


### match(...)

Use match to create autosuggest and autocomplete functionality. [See
also here](./autosuggest.md)

```javascript
index.match({
  beginsWith: 'epub'
}).on('data', function (data) {
  data.should.be.exactly(matches.shift())
}).on('end', function () {
  done()
})
```

**options** is an object that can contain:

* **beginsWith** _string default:''_ return all words that begin with
  this string
* **field** _string default:'*'_ perform matches on data found in this field
* **threshold** _number default:3_ only preform matches once
  `beginsWith` is longer than this number
* **limit** _number default:10_ maximum amount of matches to return
* **type** _string default:'simple'_ the type of matcher to use, can
  only be 'simple' for the time being.


### search(...)

Searches in the index. [See also here](./search.md)

```javascript
si.search({
  query: [{
    AND: {'*': ['gigantic', 'teddy', 'bears']}
  }]
}).on('data', function (data) {
  // do something cool with search results
})
```

**q** is an object that describes a search query and can contain the
  following properties:

 * **query** _Object_ An object that specifies query terms and
   fields. For example `{'title': ['ronald', 'reagan']}`. An asterisk
   can be used as a wildcard for either fieldname or terms
   `{'*': ['ronald', 'reagan']}` or `{'title': ['*']}`. If documents
   have been indexed with an nGram length of 2 or more, it is possible
   to search for the phrase 'ronald reagan': `{'*': ['ronald
   reagan']}`. AND or NOT conditions can be specified, and queries can
   be chained together to create OR statements:

   ```javascript
   [
     {
       AND: {'*': ['watch', 'gold'] },
       NOT: {'name': ['apple'] }
     },
     {
       AND: {'*': ['apple', 'watch'] }
     }
   ]
   ```
   Find "watch" AND "gold" in all ("*") fields OR "apple" and "watch"
   in all fields

 * **categories** _Array_ Allows you to collate counts for each
     catgory that the documents are tagged with

   ```javascript
   [
     {
       field: 'manufacturer'
     }
   ]
   ```

 * **buckets** _Array_ buckets are like categories, except ranges can
     be defined with `gte` (greater than or equal to) and `lte` (less
     than or equal to)

   ```javascript
   [{
     field: 'price',
     gte: '2',
     lte: '3'
   }]
   ```

 * **filters** _Array_ An object that can filter search
   results. Filters can only be applied to fields that have been
   flagged with the `filter` option during indexing. Filters are
   commonly used in conjunction with the selection of catgories and
   buckets.

   ```javascript
   [{
     field: 'price',
     gte: '2',
     lte: '3'
   }]
   ```

 * **offset** _number_ Sets the start index of the results. In a
   scenario where you want to go to "page 2" in a resultset of 30
   results per page, you would set `offset` to `30`. Page 3 would
   require an `offset` of `60`, and so on.

 * **pageSize** _number_ Sets the size of the resultset.

 * **teaser** _string_ Specifies which field should be used to
   generate a teaser



### tellMeAboutMySearchIndex(...)

Returns info about the state of the index

```javascript
si.tellMeAboutMySearchIndex(function (err, info) {
  console.log(info)
})
```


## Writing

### add(...)

Returns a [writeable
stream](https://nodejs.org/api/stream.html#stream_class_stream_writable)
that can be used to index documents into the search index.

```javascript
s.pipe(JSONStream.parse())
  .pipe(si.defaultPipeline(options))
  .pipe(si.add())
```


### defaultPipeline(...)

Prepares a "standard document" (an object where keys become field names,
and values become corresponding field values) for indexing. Customised
pipeline stages can be inserted before and after processing if required.

**options** is an object that describes how the documents will be
indexed and can contain the following fields:

 * **defaultFieldOptions** _Object_ default options to use for this
   batch
 * **fieldOptions** _Object_ overrides `defaultFieldOptions`, can have
   the following object values:
   * **filter** _boolean default:false_ : can you filter on this field
   * **nGramLength** _number, default:1_ : length of word sequences to
     be indexed. Use this to capture phrases of more than one word.
   * **searchable** _boolean default:true_ : is this field searchable
   * **weight** _number: default:0_ this number will be added to the
     score for the field allowing some fields to count more or less
     than others.
   * **store** _Array_ specifies which fields to store in index. You
     may want to index fields that are not shown in results, for
     example when dealing with synonyms


### del(...)

Deletes one or more documents from the corpus

```javascript
index.del('docID', function(err) {
  if (!err) console.log('success!')
})
```

* **docID** an array of document IDs that are to be deleted. A single
    ID can also be used.

### flush(...)

Empties the index. Can be used during replication.

```javascript
index.flush(function(err) {
  if (!err) console.log('success!')
})
```

## Replication


### dbReadStream(...)

Use `dbReadStream()` to create a stream of the underlying key-value
store. This can be used to pipe indexes around. You can for example
replicate indexes to file, or to other (empty) indexes

```javascript
// replicate an index to file
si.dbReadStream(options) 
  .pipe(fs.createWriteStream(sandboxPath + '/backup.gz'))
  .on('close', function() {
    // done
  })
```

```javascript
// replicate an index to another search-index
replicator.dbReadStream({gzip: true})
  .pipe(zlib.createGunzip())
  .pipe(JSONStream.parse())
  .pipe(replicatorTarget2.DBWriteStream())
  .on('close', function () {
    // done
  })
```

**options** is an optional object that describes how the stream is formatted

* **gzip** If set to `true`, the readstream will be compressed into the gzip format

### dbWriteStream(...)

Use `dbWriteStream()` to read in an index created by
`DBReadStream()`.

```javascript
si.DBReadStream(options)
  .pipe(fs.createWriteStream(sandboxPath + '/backup.gz'))
  .on('close', function() {
    done()
  })
```

**options** is an optional object that describes how the stream will be written

* **merge** If set to `true`, the writestream will merge this index with the existing one, if set to false the existing index must be empty


