# Run search-index in the Browser

The latest browserified version of `search-index` is available in the
<a href="../dist">dist directory</a>. There is a live demo of
search-index running in the browser
[here](https://cdn.rawgit.com/fergiemcdowall/search-index/master/doc/demo/index.html)


There are many ways of using `search-index` in a browser. You could
for example do something like this:

```html
<div id="result">waiting for results...</div>

<script src="search-index.min.js"></script>
<script>

// display search results in a div
const paintResultDiv = function(result) {
  console.log(result)
  document.getElementById('result').innerHTML = result.document.body
}

// index some data
const indexData = function(err, myCoolSearchIndex) {
  mySearchIndex.concurrentAdd({}, data, function(err) {
    // now you can perform search queries
    myCoolSearchIndex.search('pickled onions').on('data', paintResultDiv)
  })
}

// initialize search-index
SearchIndex({
  indexPath: 'fergie'
}, indexData) 

</script>
```
