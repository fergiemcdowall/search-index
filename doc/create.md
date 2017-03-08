# Creating search indexes

Search indexes are created by `require`-ing `search-index` and
instantiating with startup options.


```javascript
var options = {} //put the startup options you want here
var SearchIndex = require('search-index')
SearchIndex(options, function(err, index) {
  //do stuff with index
});
```

## Startup options

### Defaults

The default options are equivalent to this:

```javascript
{
  batchSize: 1000,
  db: (a levelup db),
  fieldedSearch: true,
  fieldOptions: {},
  preserveCase: false,
  storeable: true,
  searchable: true,
  indexPath: 'si',
  logLevel: 'error',
  nGramLength: 1,
  nGramSeparator: ' ',
  separator: /[\|' \.,\-|(\n)]+/,
  stopwords: require('stopword').en,
}
```

See the [API docs](API.md#options-and-settings-1) for a description of what each setting
does. Options can be set for specific fields, and can be overridden
everytime a document is added.
