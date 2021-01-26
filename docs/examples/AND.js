(async () => {
  const si = require('../../')
  const db = await si({ name: 'nodeQuickstart' })
  const print = txt => console.log(JSON.stringify(txt, null, 2))

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

  console.log('\nAND with one clause ->')
  await db.QUERY({ AND: ['Rolling'] }).then(print)

  console.log('\nAND and return whole documents ->')
  await db.QUERY({ AND: ['Rolling'] }, { DOCUMENTS: true }).then(print)

  console.log('\nAND ->')
  await db.QUERY({ AND: ['Rolling', 'Stones'] }).then(print)

  console.log('\nAND ->')
  await db.QUERY({ AND: ['Stones', 'Rolling'] }).then(print)

  console.log('\nAND ->')
  await db.QUERY({ AND: ['boys'] }).then(print)

  console.log('\nAND with nested OR ->')
  await db.QUERY({
    AND: [
      'The',
      {
        OR: ['Rolling', 'Who']
      }
    ]
  }).then(print)

  console.log('\nOR with nested AND ->')
  await db.QUERY({
    OR: [
      { AND: ['Rolling', 'Stones'] },
      { AND: ['The', 'Who'] }
    ]
  }).then(print)

  console.log('\nAND with scoped search terms expressed as strings ->')
  await db.QUERY({
    AND: [
      'description:The',
      'description:rock',
      'bandName:The'
    ]
  }).then(print)


  console.log('\nAND with scoped search terms expressed as objects ->')
  await db.QUERY({
    AND: [
      {
        FIELD: 'description',
        VALUE: 'the'
      },
      {
        FIELD: 'description',
        VALUE: 'rock'
      },
      {
        FIELD: 'bandname',
        VALUE: 'the'
      }
    ]
  }).then(print)


})()
