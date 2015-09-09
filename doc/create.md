# Creating search indexes

Search indexes are created by `require`-ing `search-index` and passing it startup options.

```javascript
var options = {} //put the startup options you want here
var si = require('search-index')(options);
```

## Startup options

### Defaults

The defaults options are equivalent to this:

```javascript
  {
    deletable: true,
    fieldedSearch: true,
    indexPath: 'si',
    logLevel: 'error',
    nGramLength: 1,
    stopwords: SearchIndex.getStopwords('en'),
    fieldsToStore: 'all'
  }
```

### deletable

Non-deletable indexes are smaller, so if file size is a consideration
(for example: if the index is to be replicated), and deletion
unnecessary set `deletable` to false.

### fieldedSearch

It may be desirable to search on individual fields, but this takes up
lots of space in the index. Set `fieldedSearch` to false if fielded
search is not needed, and file size is a consideration.

### indexPath

Specifies the name of the keystore on the system.

### logLevel

Specifies a [Bunyan](https://github.com/trentm/node-bunyan) log level

### nGramLength

If you want to search for phrases, then the nGramLength has to be
adjusted. The number represents to maximum length (in words) of
searchable phrases.

### stopwords

Words that are not indexed. The default is english (en), but you can choose
any common and meaningless words for your language or domain.

### fieldsToStore

You may only want to store some of the fields in the document. For
  example, and ID or a link to the actual document location. 'all'
  means everything. An array of field names specifies the fields to
  store.
