# Autosuggest and matching

Search-index offers two main strategies for autosuggest

### 1: Just do a normal search

Simply wire your autosuggest up to the [search](search.md) endpoint

### 2: Use the matcher functionality

The matcher is a fast dictionary lookup of all the terms that are in
the corpus. It returns a list that is ordered by frequency in the
index (most frequent first).

```javascript
//str is the input in the search box
si.match(str, function (err, matches) {
  //matches is an array of all the terms that match the input.
})
```
