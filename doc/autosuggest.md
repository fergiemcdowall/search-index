# Autosuggest and matching

Search-index offers two main strategies for autosuggest

### 1: Just do a normal search

Simply wire your autosuggest up to the [search](search.md) endpoint

### 2: Use the matcher functionality

The matcher is a fast dictionary lookup of all the terms that are in
the corpus. It returns a list that is ordered by frequency in the
index (most frequent first).

It accepts an options object that can contain

```javascript
var options = {
  beginsWith: "", // The beginning of the text to match
  field: "*",     // The field to use (defaults to all fields)
  threshold: 3    // The amount of characters to ignore before returning matches (default: 3)
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

might return

```javascript
['london', 'long beach', 'long island', lonsdale]
```
