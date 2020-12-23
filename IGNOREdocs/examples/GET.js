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

  console.log('\nGETTING ->')
  await db.read({ GET: ['Rolling']}).then(console.log)

  console.log('\nGETTING (calling .read() with a single string falls back to GET) ->')
  await db.read('Rolling').then(console.log)
  
  console.log('\nGETTING ->')
  await db.read({ GET: ['The']}).then(console.log)
  
})()
