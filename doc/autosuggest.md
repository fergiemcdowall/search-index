# Autosuggest and matching

Search-index offers two main strategies for autosuggest

1. Just do a normal search
2. Use the matcher functionality

## Just do a normal search

Simply wire your autosuggest up to the [search](search.md) endpoint

## Use the matcher functionality

The matcher is a fast dictionary lookup of all the terms that are in
the corpus. It returns a list that is ordered by frequency in the
index (most frequent first).

It accepts an options object that can contain:

```javascript
var options = {
  beginsWith: "", // The beginning of the text to match
  field: "*",     // The field to use (defaults to all fields)
  threshold: 3,   // The amount of characters to ignore before returning matches (default: 3)
  limit: 10,      // The maximum amount of suggestions
  type: 'simple'  // The type of autosuggest returned- can be `simple`, `ID` or `count`
}
```

Basic use:

```javascript
si.match(options, function (err, matches) {
  //matches is an array of all the terms that match the input.
})
```


Example:

```javascript
var options = {
  beginsWith: "lon",
  field: "city",
  threshold: 2
}
si.match(options, function (err, matches) {
  //matches is an array of all the terms that match the input.
})
```

...might return:

```javascript
['london', 'long beach', 'long island', 'lonsdale']
```

You can also set the `type` option to `ID` to return sets of IDs:

```javascript
var options = {
  beginsWith: "lon",
  field: "city",
  threshold: 2,
  type: 'ID'
}
si.match(options, function (err, matches) {
  //matches is an array of all the terms that match the input.
})
```

...which would return a list of suggestions, together with the sets of IDs for
the documents that they appear in

```javascript
[['london', ['1', '3', '4', '10', '12']],
 ['long beach', ['1', '5', '10']],
 ['long island', ['3']],
 ['lonsdale', ['11']]]
```

Setting the `type` option to `count` will return a count for every
suggestion where `count` is a the total number of documents that the
term appears in:

```javascript
var options = {
  beginsWith: "lon",
  field: "city",
  threshold: 2,
  type: 'count'
}
si.match(options, function (err, matches) {
  //matches is an array of all the terms that match the input.
})
```

...returns a list of suggestions, together with the sets of IDs for
the documents that they appear in

```javascript
[['london', 5],
 ['long beach', 3],
 ['long island', 1],
 ['lonsdale', 1]]
```

