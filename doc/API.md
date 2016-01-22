# API reference

## Initialization

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

### empty(...)

Empties the index. Can be used during replication

```javascript
index.empty(function(err) {
  if (!err) console.log('success!')
})
```

### match(...)

Use match to create autosuggest and autocomplete functionality

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
`snapShot()`

```javascript
si.replicate(fs.createReadStream('/backup.gz'), function (err) {
  if (!err) console.log('success!')
});
```

### search(...)

Todo...

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

### tellMeAboutMySearchIndex

todo(...)