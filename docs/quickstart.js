const si = require('search-index')

(async function() {
  const si = require('search-index')

  // initialize an index
  const { PUT, QUERY } = await si()

  // add documents to the index
  await PUT([{
    _id: '1',
    text: 'the first document'
  }, {
    _id: '2',
    text: 'the second document'
  }])

  // read documents from the index
  const results = await QUERY('document').then(console.log)

})()
