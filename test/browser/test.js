const that = this

const paintResultDiv = function(result) {
  result = JSON.parse(result)
  console.log(result)
  document.getElementById('result').innerHTML = result.document.body
}

const indexData = function(err, index) {
  index.flush(function(err) {
    highland([{
      id: '1',
      body: 'this is a document from the search index'
    }])
      .pipe(index.defaultPipeline())
      .pipe(index.add())
      .on('data', function(data) {
        console.log(data)
      })     
      .on('end', function() {
        index.search().on('data', paintResultDiv)
      })     
  })
}

SearchIndex({
  indexPath: 'fergie'
}, indexData) 
