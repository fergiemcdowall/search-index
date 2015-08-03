# Adding documents to search indexes

Documents can be added and deleted in batches of size 1-n. If the
index is initialized with `deletable` set to `false` then the index is
read-only.

Indexing options can be set at a batch and at document level.


### Adding

The syntax for adding a batch of documents is:

```javascript
//options (can be left empty)
var batchOptions = {};
batchOptions.fieldOptions = [
  {fieldName: 'field1', ...},
  {fieldName: 'field2', ...},
  ...
];

//add
si.add(batchOptions, data, function (err) {
  if (!err) console.log('success!');
});
```

### Default batch options

Default batch options are equivalent to:

```javascript
{
  batchName: 'my batch',
  fieldOptions: [],                                 //none
  fieldsToStore: SearchIndex.options.fieldsToStore, //inherited from initialization options
  defaultFieldOptions: defaultFieldOptions          //can be overrided per field
}
```

### Default field options

Default field options are equivalent to:

```javascript
{
  filter: false,
  nGramLength: SearchIndex.options.nGramLength,     //inherited from initialization options
  searchable: true,
  weight: 1,
  fieldedSearch: SearchIndex.options.fieldedSearch  //inherited from initialization options
}
```

#### filter

Set this to `true` in order to be able to do faceting and filtering on
this field

#### nGramLength

Sets the length of the phrase search on this field

#### searchable

Is field searchable? You might want to store a field in a document
that will not be searchable.

#### weight

Specifies the relevancy weighting that this field gets at indexing
time. Pre-weighting fields is the most effiecient way of creating
relevancy models.

#### fieldedSearch

Is it possible to search on only this field?


### Batch format

Batches are arrays of objects that may or may not have an id field. If
no id field is present, then one will be generated. [example batch here](https://raw.githubusercontent.com/fergiemcdowall/reuters-21578-json/master/data/full/reuters-000.json)

```javascript
[
  {},
  {},
  {}
]
```

