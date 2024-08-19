import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'non-ascii-chars-test'
const global = {}

test('create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({ name: indexName })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add data', t => {
  const data = [
    {
      _id: 0,
      text: 'brønnøysundregisterene er gøy'
    },
    {
      _id: 1,
      text: 'En two tre fire blåtind fem seks'
    },
    {
      _id: 2,
      text: 'godt gjæret øl'
    },
    {
      _id: 3,
      text: 'A ticket to 大阪 costs ¥2000.'
    },
    {
      _id: 4,
      text: 'Приключения Алисы в Стране чудес'
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('match brønnøysundregisterene', t => {
  t.plan(1)
  global[indexName]._AND(['brønnøysundregisterene']).then(res => {
    t.deepEqual(res, [
      {
        _id: 0,
        _match: [
          { FIELD: 'text', VALUE: 'brønnøysundregisterene', SCORE: '1.00' }
        ]
      }
    ])
  })
})

test('match blåtind', t => {
  t.plan(1)
  global[indexName]._AND(['blåtind']).then(res => {
    t.deepEqual(res, [
      { _id: 1, _match: [{ FIELD: 'text', VALUE: 'blåtind', SCORE: '1.00' }] }
    ])
  })
})

test('match gjæret øl', t => {
  t.plan(1)
  global[indexName]._AND(['gjæret', 'øl']).then(res => {
    t.deepEqual(res, [
      {
        _id: 2,
        _match: [
          { FIELD: 'text', VALUE: 'gjæret', SCORE: '1.00' },
          { FIELD: 'text', VALUE: 'øl', SCORE: '1.00' }
        ]
      }
    ])
  })
})

test('match 大阪 costs 2000', t => {
  t.plan(1)
  global[indexName]._AND(['大阪', 'costs', '2000']).then(res => {
    t.deepEqual(res, [
      {
        _id: 3,
        _match: [
          { FIELD: 'text', VALUE: '2000', SCORE: '1.00' },
          { FIELD: 'text', VALUE: 'costs', SCORE: '1.00' },
          { FIELD: 'text', VALUE: '大阪', SCORE: '1.00' }
        ]
      }
    ])
  })
})

test('Приключения Алисы в стране чудес', t => {
  t.plan(1)
  global[indexName]._AND(['стране', 'чудес']).then(res => {
    t.deepEqual(res, [
      {
        _id: 4,
        _match: [
          { FIELD: 'text', VALUE: 'стране', SCORE: '1.00' },
          { FIELD: 'text', VALUE: 'чудес', SCORE: '1.00' }
        ]
      }
    ])
  })
})
