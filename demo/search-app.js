// "lazy load"- db may not be immediately initialized
const db = searchIndex({ name: 'searchIndexInBrowser' })

const indexData = function (data) {
  emptyElements(['title','body'])
  db.PUT([data])
    .then(function(message) {
      console.log(message)
    })
    .catch(function (err) {
      console.log('Error while indexing:')
      console.log(err.message)
    })
}

const search = function (q) {
  emptyElements(['searchResults'])
  db.SEARCH(...(q.split(' ')))
    .then(function(results) {
      console.log(JSON.stringify(results[0]))
      results.forEach(function(result) {
        console.log(result);
        populateResultsDiv(result)
    })
    .catch(function (err) {
      console.log('Error while searching:')
      console.log(err)
    })
  })
}

// Workaround for a possible bug when ID is generated
const generateID = function (str) {
  return str.split('').reduce((prevHash, currVal) =>
    (((prevHash << 5) - prevHash) + currVal.charCodeAt(0))|0, 0);
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

// Empty HTML elements
const emptyElements = function (elements) {
  elements.forEach(function(element) {
    document.getElementById(element).innerHTML = ''
    document.getElementById(element).value = ''
  })
}

// Listen to button click and initiate indexing of data
document.getElementById("indexContent").onmousedown = function() {
  const docTitle = document.getElementById("title").value
  const docBody = document.getElementById("body").value
  const docId = generateID(docTitle + docBody)
  if ((docTitle + docBody).length > 0) indexData({ _id: docId, title: docTitle, body: docBody })
  console.log({ _id: docId, title: docTitle, body: docBody })
}

// Listen to key up and initiate a search
document.getElementById("searchQuery").onkeyup = function() {
  search(document.getElementById("searchQuery").value)
  console.log('Search query: ')
  console.log(document.getElementById("searchQuery").value)
}