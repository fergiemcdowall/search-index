// the variable that will become the search index
var mySearchIndex

// display search results in a div
const paintResultDiv = function(result) {
  var node = document.createElement('div')
  node.innerHTML = '<b>' + result.document.title + '</b><br>'
    + result.document.body + '<br>---'
  document.getElementById('result').appendChild(node)
}

const search = function (q) {
  document.getElementById('result').innerHTML = ''
  mySearchIndex.search(q).on('data', paintResultDiv)
}

const initIndex = function (err, index) {
  if (err) console.error(err)
  mySearchIndex = index
  search()
}

// index some data
const indexData = function (data) {
  mySearchIndex.concurrentAdd({}, data, function(err) {
    // and then show search results (defaults to everything)
    search()
  })
}

// initialize search-index
SearchIndex({
  indexPath: 'si-demo',
  keySeparator: '~' // this is a websafe separator for most languages- change if weirdness occurs
}, initIndex)

// PAGE CONTROLLERS

// Add a doc to the index
document.getElementById("a").onmousedown = function() {
  const t = document.getElementById("title").value
  const b = document.getElementById("body").value
  if ((t + b).length > 0) indexData([{ title: t, body: b }])
}

// show the search panel
document.getElementById("showSearch").onmousedown = function() {
  document.getElementById("add").style.display = "none"
  document.getElementById("search").style.display = "block"
  document.getElementById("s").focus()
}

// show the add panel
document.getElementById("showAdd").onmousedown = function() {
  document.getElementById("add").style.display = "block"
  document.getElementById("search").style.display = "none"
  document.getElementById("title").focus()
}

// do a search
document.getElementById("s").onkeyup = function() {
  search(document.getElementById("s").value)
}

// do a search
document.getElementById("sa").onmousedown = function() {
  search()
}

// init with title field in focus
document.getElementById("title").focus()
