(async () => {
  const si = require('../../')
  const db = await si({
    name: 'DELETE-EXAMPLE',
    storeVectors: true
  })

  console.log('\nADDING ->')
  await db.PUT([
    {
      _id: 1,
      bandName: 'The Beatles',
      description: 'The nice boys of pop'
    }, {
      _id: 'two',
      bandName: 'The Rolling Stones',
      description: 'The bad boys of rock'
    }, {
      _id: 3,
      bandName: 'The Who',
      description: 'Nearly as good as Led Zeppelin'
    }
  ]).then(console.log)

  console.log('\nVERIFYING ->')
  await db.QUERY({ DOCUMENTS: ['3'] }).then(console.log)
  await db.DOCUMENT_COUNT().then(console.log)
  await db.ALL_DOCUMENTS().then(console.log)

  console.log('\nDELETING ->')
  await db.DELETE(['3']).then(console.log)

  console.log('\nCONFIRM DELETE ->')
  await db.QUERY({ DOCUMENTS: ['3'] }).then(console.log)
  await db.QUERY({ SEARCH: ['Who'] }).then(console.log)
  await db.DOCUMENT_COUNT().then(console.log)
  await db.ALL_DOCUMENTS().then(console.log)

})()
