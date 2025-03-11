import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '514'

const global = {}

test('create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = new SearchIndex({
      name: indexName,
      storeVectors: true
    })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
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

test('simple _SEARCH with 1 clause', async t => {
  t.plan(3)
  await global[indexName].SEARCH(['Zeppelin']).then(res =>
    t.deepEqual(res, {
      QUERY: { AND: ['Zeppelin'] },
      OPTIONS: { SCORE: { TYPE: 'TFIDF' }, SORT: true },
      RESULT: [
        {
          _id: 3,
          _match: [{ FIELD: 'description', VALUE: 'zeppelin', SCORE: '0.50' }],
          _score: 0.69
        }
      ],
      RESULT_LENGTH: 1,
      PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
    })
  )

  await global[indexName]
    .DELETE(3)
    .then(res =>
      t.deepEqual(res, [{ _id: 3, operation: 'DELETE', status: 'DELETED' }])
    )

  await global[indexName].SEARCH(['Zeppelin']).then(res =>
    t.deepEqual(res, {
      QUERY: { AND: ['Zeppelin'] },
      OPTIONS: { SCORE: { TYPE: 'TFIDF' }, SORT: true },
      RESULT: [],
      RESULT_LENGTH: 0,
      PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 0, DOC_OFFSET: 0 }
    })
  )
})
