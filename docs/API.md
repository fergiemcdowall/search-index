# API reference

### Opening and Closing

 * [require('search-index')](#initialization)
 * [close(...)](#close)

### Reading

 * [availableFields(...)](#availablefields)
 * [buckets(...)](#buckets)
 * [categorize(...)](#categorize)
 * [countDocs(...)](#countdocs)
 * [get(...)](#get)
 * [match(...)](#match)
 * [search(...)](#search)
 * [totalHits(...)](#totalhits)

### Writing

 * [add(...)](#add)
 * [concurrentAdd(...)](#concurrentadd)
 * [defaultPipeline(...)](#defaultpipeline)
 * [del(...)](#del)
 * [flush(...)](#flush)

### Syncing

 * [dbReadStream(...)](#dbreadstream)
 * [dbWriteStream(...)](#dbwritestream)

### Options and Settings
 
(viewable under `index.options`)

 *  [batchSize](#batchsize)
 *  [db](#db)
 *  [fieldedSearch](#fieldedsearch)
 *  [fieldOptions](#fieldoptions)
 *  [preserveCase](#preservecase)
 *  [storeable](#storeable)
 *  [searchable](#searchable)
 *  [indexPath](#indexpath)
 *  [logLevel](#loglevel)
 *  [nGramLength](#ngramlength)
 *  [separator](#separator)
 *  [stopwords](#stopwords)


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

or

```javascript
const getData = function(err, si) {
  // si is now a new search index
}
require('search-index')(options, getData)
```

`options` can be any [option](#options and settings) which will then form the defaults of the
index until it is closed, or the options are changed.


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

### availableFields(...)

Returns a readable stream of all fields that can be searched in.

```javascript
si.availableFields().on('data', function (field) {
  // "field" is the name of a field that is searchable
}).on('end', function () {
  // done
})
```

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
        gte:   '2',
        lte:   '3',
        set:   false
      }
      {
        field: 'price',
        gte:   '4',
        lte:   '7',
        set:   false
      }
    ]
  }).on('data', function (data) {
    // do something with the data
  }).on('end', function () {
    // finshed
  })
```

**query** is a standard `search-index` query that will specify a set
  of documents

**buckets** is an array of buckets that must be an object with the
  following 4 properties:

  * **field** (mandatory) The field name
  * **gte** "greater that or equal to"
  * **limit** Limit the entries that will be returned
  * **lte** "less than or equal to"
  * **set** if true- return a set of IDs. If false or not set, return
            a count


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

**query** is a standard `search-index` query that will specify a set
  of documents

**catogory** is an array of objects specifying fields to categorize
  on:

  * **field** Name of the field to categorize on
  * **set** if true- return a set of IDs. If false or not set, return
            a count

In addition the q object can have *offset* and *pageSize*, that
will work the same way as for the /search endpoint.


### countDocs(...)

Returns the total amount of docs in the index

```javascript
si.countDocs(function (err, count) {
  console.log('this index contains ' + count + ' documents')
})
```


### get(...)

Gets a document from the corpus

```javascript
index.get(docIDs).on('data', function (doc) {
  // doc is a document for each ID in docIDs
})

```

* **docIDs** an array of document IDs


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
       AND: {'*':    ['watch', 'gold'] },
       NOT: {'name': ['apple'] }
     },
     {
       AND: {'*':    ['apple', 'watch'] }
     }
   ]
   ```
   Find "watch" AND "gold" in all ("*") fields OR "apple" and "watch"
   in all fields


 * **filters** _Object_ One or more objects added to the AND and/or
   NOT objects that filters the search results. Filters can only be
   applied to any searchable fields in your index. Filters are
   commonly used in conjunction with the selection of catgories and
   buckets.

   ```javascript
   [
     {
       AND: {
         '*':    ['watch', 'gold'],
         'price': [{
           gte: '1000',
           lte: '8'
         }]
       }
     }
   ]
   ```

 * **offset** _number_ Sets the start index of the results. In a
   scenario where you want to go to "page 2" in a resultset of 30
   results per page, you would set `offset` to `30`. Page 3 would
   require an `offset` of `60`, and so on.

 * **pageSize** _number_ Sets the size of the resultset.


### totalHits(...)

Returns a count of the documents for the given query including
those hidden by pagination

```javascript
si.totalHits(q, function (err, count) {
  console.log('the query ' + q + ' gives ' + count + ' results')
})
```

## Writing

### add(...)

Returns a [writeable
stream](https://nodejs.org/api/stream.html#stream_class_stream_writable)
that can be used to index documents into the search index.

Note that this stream cannot be used concurrently. If documents are
being sent on top of one another then it is safer to use
`concurrentAdd`, however `add` is faster and uses less resources.

 * **batchOptions** is an object describing [indexing options](#defaultpipeline)

```javascript
// s is a Readable stream in object mode
s.pipe(si.defaultPipeline(batchOptions))
  .pipe(si.add())
  .on('data', function(d) {
    // this function needs to be called if you want to listen for the end event
  })
  .on('end', function() {
    // complete
  })
```

### concurrentAdd(...)

An alternative to `.add(...)` that allows adding by passing an array
of documents and waiting for a callback. Useful for environments where
node streams cannot be constructed (such as browsers).

Note that `concurrentAdd` queues documents internally, so in a
scenario where unordered documents are being added rapidly from many
sources `concurrentAdd` should be used.

 * **data** is an array of documents
 * **batchOptions** is an object describing [indexing options](#defaultpipeline)

```javascript
mySearchIndex.concurrentAdd(batchOptions, data, function(err) {
  // docs added
})
```

### defaultPipeline(...)

Prepares a "standard document" (an object where keys become field names,
and values become corresponding field values) for indexing. Customised
pipeline stages can be inserted before and after processing if required.

**options** is an object where each key corresponds to a field name in
  the documents to be indexed and can contain the following settings:

 * **defaultFieldOptions** _Object_ default options to use for this
   batch
 * **fieldOptions** _Object_ overrides `defaultFieldOptions`, can have
   the following object values:
   * **fieldedSearch** _boolean, default:true_ : can searches be
     carried out on this specific field
   * **nGramLength** _number, default:1_ : length of word sequences to
     be indexed. Use this to capture phrases of more than one word.
   * **preserveCase** _boolean, default:true_ : preserve the case of
     the text
   * **searchable** _boolean default:true_ : is this field searchable? 
   * **separator**  _regex_ : A regex in the [String.split()](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/String/split) format
     that will be used to tokenize this field
   * **sortable** _boolean default:false_ : can this field be sorted
     on? If true field is not searchable
   * **stopwords** _Array, default: require('stopword').en_ An array
     of [stop words](https://en.wikipedia.org/wiki/Stop_words).
   * **storeable** _Array_ specifies which fields to store in index. You
     may want to index fields that are not shown in results, for
     example when dealing with synonyms
   * **weight** _number: default:0_ this number will be added to the
     score for the field allowing some fields to count more or less
     than others.


### del(...)

Deletes one or more documents from the corpus

```javascript
si.del(docIDs, function(err) {
  return done()
})
```

* **docIDs** an array of document IDs referencing documents that are
    to be deleted.

### flush(...)

Empties the index. Deletes everything.

```javascript
index.flush(function(err) {
  if (!err) console.log('success!')
})
```

## Syncing

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

## Options and Settings

### batchsize
_number_

Specifies how many documents to process, before merging them into the
index. When the end of the stream is reached all remaning documents
will be merged, even if batchsize is not reached.

### db
_a levelup instance_

The datastore.

### fieldedSearch
_boolean_

If true, then the field is searchable.

### fieldOptions
_boolean_

Contains field specific overrides to global settings

Example on setting options on several fields:
```javascript
fieldOptions: {
  id: {
    searchable: false
  },
  url: {
    searchable: false
  }
}
```

### preserveCase
_true_

If true, case is preserved. For example: queries for "Potato" will not
match "potato"

### storeable
_boolean_

If true, a cache of the field is stored in the index

### searchable
_boolean_

If true, this field will be searchable by wildcard ('*'). See also
[fieldedSearch](#fieldedsearch)

### indexPath
_string_

The location of the datastore. If `db` is specified, then indexPath is ignored

### logLevel
_string_

A [bunyan](https://github.com/trentm/node-bunyan) log level.

### nGramLength
_number_ or _array_ or _object_

All valid definitions of nGramLength:
```javascript
nGramLength = 1                // 1
nGramLength = [1,3]            // 1 & 3
nGramLength = {gte: 1, lte: 3} // 1, 2 & 3
```

Specifies how to split strings into phrases. See
https://www.npmjs.com/package/term-vector for examples

### separator
_string_

Specifies how strings are to be split, using a regex in the
[String.split()](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/String/split) format

### stopwords
_array_
An array of [stopwords](https://en.wikipedia.org/wiki/Stop_words)
