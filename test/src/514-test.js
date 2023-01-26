const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + '514'

test('create a search index', t => {
  t.plan(1)
  si({ name: indexName, storeVectors: true }).then(db => {
    global[indexName] = db
    t.pass('ok')
  })
})

test('can add data', t => {
  const data = [
    {
      _id: 1,
      bandName: 'The Beatles',
      description: 'The nice boys of pop'
    },
    {
      _id: 'two',
      bandName: 'The Rolling Stones',
      description: 'The bad boys of rock'
    },
    {
      _id: 3,
      bandName: 'The Who',
      description: 'Nearly as good as Led Zeppelin'
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('simple _AND with 1 clause', async t => {
  t.plan(3)
  await global[indexName].SEARCH(['Zeppelin']).then(res =>
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 3,
          _match: [{ FIELD: 'description', VALUE: 'zeppelin', SCORE: '0.50' }],
          _score: 0.69
        }
      ],
      RESULT_LENGTH: 1
    })
  )

  await global[indexName]
    .DELETE(3)
    .then(res =>
      t.deepEqual(res, [{ _id: 3, operation: 'DELETE', status: 'DELETED' }])
    )

  await global[indexName].SEARCH(['Zeppelin']).then(res =>
    t.deepEqual(res, {
      RESULT: [],
      RESULT_LENGTH: 0
    })
  )
})
