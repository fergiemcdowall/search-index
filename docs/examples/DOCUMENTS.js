(async () => {

  const si = require('../../')
  const db = await si({ name: 'nodeQuickstart' })
  
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
  ])

  console.log('\nuse DOCUMENTS with SEARCH  (JSON style) ->')
  await db.read({ SEARCH: ['Rolling'] }, { DOCUMENTS: true }).then(console.log)

  console.log('\nuse DOCUMENTS with SEARCH (promise style) ->')
  await db.read({ SEARCH: ['Rolling'] }).then(db.DOCUMENTS).then(console.log)

  console.log('\nuse DOCUMENTS with with specified IDs ->')
  await db.read({ DOCUMENTS: [{_id:1}, {_id:'two'}] }).then(console.log)

  console.log('\nuse DOCUMENTS with with specified IDs, one of which does not exist ->')
  await db.read({ DOCUMENTS: [{_id:1}, {_id:9999}, {_id:'two'}] }).then(console.log)

})()
