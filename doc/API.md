# API reference

 * [add(...)](#add)
 * [close(...)](#close)
 * [get(...)](#get)
 * [del(...)](#del)
 * [flush(...)](#flush)
 * [match(...)](#match)
 * [replicate(...)](#replicate)
 * [search(...)](#search)
 * [snapShot(...)](#snapshot)
 * [tellMeAboutMySearchIndex(...)](#tellmeaboutmysearchindex)

## initialization

Make sure that `search-index` is npm installed and then do either

```javascript
const searchIndex = require('search-index')
const index = searchIndex(options)
```

or

```javascript
const index = require('search-index')(options)
```

`options` is an object that can take the following values:

 * **deletable** _boolean, default:true_ : is this index read-only? If
     so it will be smaller but immuteable.

 * **fieldedSearch** _boolean, default:true_ : Can you search on
     individual document fields? Setting this to false saves space and
     makes the index faster.

 * **fieldsToStore** _array or 'all', default 'all'_ : which fields in
     the incoming documents should be considered by the index.

 * **indexPath** _string, default:'si'_ : what should the index be
     called on disk.

 * **logLevel** _string, default:'error'_ : a bunyan log level.

 * **nGramLength** _number, default:1_ : length of word sequences to
     be indexed. Use this to capture phrases of more than one word.

 * **separator** _RegExp, default:/[\|' \.,\-|(\n)]+/_ : A RegExp in
     the "string split" format

 * **stopwords** _Array, default:
     require('term-vector').getStopwords('en').sort()_ : A list of
     words that are not considered for indexing. Typically they will
     be numbers, common words (english examples: 'is', 'a', 'and'), or
     otherwise "forbidden" words.
      

## Functions

### add(...)

Insert one or more documents into the index, and specify how they will
be retrievable.

```javascript
index.add(docs, options, function(err) {
  if (!err) console.log('success!')
})
```

**options** is an object that describes how the documents will be
indexed and can contain the following fields:

 * **batchName** _String_ make this batch identifiable in logs
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
 * **fieldsToStore** _Array_ specifies which fields to store in index
   that can then be retrieved with `get()`




### close(...)

Closes the index and the underlying data store. Generally this should
happen automatically, and therefore this function need never be
called.

```javascript
index.close(function(err) {
  if (!err) console.log('success!')
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

### del(...)

Deletes one or more documents from the corpus

```javascript
index.get('docID', function(err) {
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

### match(...)

Use match to create autosuggest and autocomplete functionality. [See
also here](./autosuggest.md)

```javascript
index.match(options, function(err, results) {
  if (!err) console.log('success!')
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

### replicate(...)

Use `replicate()` to read in indexes that have been serialized with
`snapShot()`. [See also here](replicate.md)

```javascript
si.replicate(fs.createReadStream('/backup.gz'), function (err) {
  if (!err) console.log('success!')
});
```

### search(...)

Searches in the index. [See also here](./search.md)

```javascript
si.search(q, function (err, searchResults) {
  // do something cool with searchResults
})
```

**q** is an object that describes a search query and can contain the
  following properties:

 * **query** _Object_ An object that specifies query terms and
   fields. For example `{'title': ['ronald', 'reagan']}`. An asterisk
   can be used as a wildcard for either fieldname or terms
   `{'*': ['ronald', 'reagan']}` or `{'title': ['*']}`. If documents
   have been indexed with an nGram length of 2 or more, it is possible
   to search for the phrase 'ronald reagan': `{'*': ['ronald reagan']}`

 * **facets** _Object_ An object that allows you to specify faceted
   navigation. You must specify fields that were flagged with the
   `filter` option during indexing. To simply collate values you can
   do something like this `{totalamt: {}}`. You can also specify
   buckets or ranges like so:

   ```javascript
   {
     totalamt: {
       sort: 'keyAsc',
       ranges: [
         [
           '000000000000000',
           '000000006000000'
         ],
         [
           '000000006000001',
           '010000000000000'
         ]
       ]
     }
   }
   ```
   **facets** can take the following parameters:
   * **ranges** _Array_ An array of arrays that describe value
   ranges. If `ranges` is not specified, then the facet will collate
   on every distinct value it finds.
   * **sort** How the list is to be sorted- can be `keyAsc`,
   `keyDesc`, `valueAsc`, `valueDesc`


 * **filters** _Object_ An object that can filter search
   results. Filters can only be applied to fields that have been
   flagged with the `filter` option during indexing. Filters are
   commonly used in conjunction with the selection of facets.

   ```javascript
   {
     totalamt: [
       ['000000000000000', '000000006000000']
     ]
   }
   ```
 * **offset** _number_ Sets the start index of the results. In a
   scenario where you want to go to "page 2" in a resultset of 30
   results per page, you would set `offset` to `30`. Page 3 would
   require an `offset` of `60`, and so on.

 * **pageSize** _number_ Sets the size of the resultset.

 * **teaser** _string_ Specifies which field should be used to
   generate a teaser
   
### snapShot(...)

Use `snapShot()` to serialize search indexes to disk.

```javascript
si.snapShot(function (rs) {
  rs.pipe(fs.createWriteStream('/backup.gz'))
    .on('close', function () {
      console.log('success')
    })
    .on('error', function (err) {
      console.log('failure')
    });
});
```

### tellMeAboutMySearchIndex(...)

Returns info about the state of the index

```javascript
si.tellMeAboutMySearchIndex(function (err, info) {
  console.log(info)
})
```