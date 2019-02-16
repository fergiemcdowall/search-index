// "lazy load"- db may not be immediately initialized
const db = searchIndex({ name: 'searchIndexInBrowser' })

// Functions
const search = function (q) {
  emptyElements(['searchResults'])
  db.SEARCH(q)
    .then(function(results) {
      //console.log(Array.isArray(results))
      console.log(JSON.stringify(results[0]))
      results.forEach(function(result) {
        console.log(result);
        populateResultsDiv(result)
    })
    .catch(function (error) {
      console.log('Error while searching:')
      console.log(error.message)
    })
  })
}

const indexData = function (data) {
  emptyElements(['title','body'])
  db.PUT([data])
    .then(function(message) {
      console.log(message)
    })
    .catch(function (error) {
      console.log('Error while indexing:')
      console.log(error.message)
    })
}

const populateResultsDiv = function(result) {
  console.log('Boom')
  console.log(result)
  //result = JSON.parse(result)
  var node = document.createElement('div')
  node.innerHTML = '<b>' + result.obj.title + '</b><br>'
    + result.obj.body + '<br>---'
  document.getElementById('searchResults').appendChild(node)
}

const emptyElements = function (elements) {
  elements.forEach(function(element) {
    document.getElementById(element).innerHTML = ''
    document.getElementById(element).value = ''
  })
}

// index data
document.getElementById("indexContent").onmousedown = function() {
  const docTitle = document.getElementById("title").value
  const docBody = document.getElementById("body").value
  if ((docTitle + docBody).length > 0) indexData({ title: docTitle, body: docBody })
  console.log({ title: docTitle, body: docBody })
}

// do a search
document.getElementById("searchQuery").onkeyup = function() {
  search(document.getElementById("searchQuery").value)
  console.log('Search query: ')
  console.log(document.getElementById("searchQuery").value)
}