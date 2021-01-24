(async () => {
  const si = require('../../../../')
  const db = await si({ name: 'nodeQuickstart' })
  const readline = require('readline').createInterface({
    input: process.stdin,
    output: process.stdout
  })

  await db.PUT([{
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
  }])

  const q = () => readline.question('ENTER A (CASE SENSITIVE) SEARCH TERM -> ', (term) => {
    db.QUERY({ SEARCH: term.split(' ') })
      .then(results => console.log(JSON.stringify(results, null, 2)))
      .then(q)
  })

  q()
})()
