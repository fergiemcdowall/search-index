(async () => {
  const si = require('../../src/main')
  const db = await si({ name: 'TMP-nodeCLI' })
  const readline = require('readline').createInterface({
    input: process.stdin,
    output: process.stdout
  })

  const data = [{
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
  }]
  await db.PUT(data)

  console.log(data)
  const q = () => readline.question('ENTER A SEARCH TERM -> ', (term) => {
    db.QUERY({ SEARCH: term.split(' ') })
      .then(results => console.log(JSON.stringify(results, null, 2)))
      .then(q)
  })

  q()
})()
