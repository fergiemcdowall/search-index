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

  console.log('\nAND ->')
  await db.read({ AND: ['Rolling'] }).then(console.log)

  console.log('\nAND and return whole documents ->')
  await db.read({ AND: ['Rolling'] }).then(db.DOCUMENTS).then(console.log)
  
  console.log('\nAND ->')
  await db.read({ AND: ['Rolling', 'Stones'] }).then(console.log)

  console.log('\nAND ->')
  await db.read({ AND: ['Stones', 'Rolling'] }).then(console.log)

  console.log('\nAND ->')
  await db.read({ AND: ['boys'] }).then(console.log)

  console.log('\nAND ->')
  await db.read({ AND: ['The'] }).then(console.log)

  console.log('\nAND with nested OR ->')
  await db.read({
    AND: [
      'The',
      {
        OR: ['Rolling', 'Who']
      }
    ]
  }).then(console.log)

  console.log('\nOR with nested AND ->')
  await db.read({
    OR: [
      { AND: [ 'Rolling', 'Stones' ] },
      { AND: [ 'The', 'Who' ] }
    ]
  }).then(console.log)
  
  console.log('\nAND with scoped search terms ->')
  await db.read({ AND: [
    'description:The',
    'description:rock',
    'bandName:The'
  ] }).then(console.log)
  
})()
