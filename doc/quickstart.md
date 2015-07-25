# search-index quickstart

So you want to set up a search index in Javascript? Here is one way to
do it. You can of course make many variations on this theme- consult
the rest of the documentation for options and alternatives.

## Step 1:

Find some data in the right format. You could for example index 1000
old Reuters articles, [like the ones found here](https://raw.githubusercontent.com/fergiemcdowall/reuters-21578-json/master/data/full/reuters-000.json)

## Step 2:

Initialize a search index and add the data

```javascript
var si = require('search-index');
var data = require('path/to/data/file');
si.add(opt, data, function (err) {
  if (err) console.log('oops! ' + err);
  else console.log('success!');
});

```

## Step 3:

Run a search query

```javascript
var q = {};
q.query = {'*': ['usa']};
si.search(q, function (err, searchResults) {
  //do something with the searchResults here
});
```