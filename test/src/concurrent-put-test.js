import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'

test('can concurrently PUT', async function (t) {
  t.plan(3)

  const size = 10

  const docs = new Array(size)
    .fill()
    .map((doc, i) => ({ text: 'this is some test text', _id: i }))

  const si = new SearchIndex({
    name: sandbox + 'concurrency'
  })
  t.ok(si)

  t.deepEquals(
    await Promise.all(docs.map(doc => si.PUT([doc]))),
    new Array(size)
      .fill()
      .map((res, i) => [{ _id: i, operation: 'PUT', status: 'CREATED' }])
  )

  t.deepEquals(
    await si.QUERY({
      AND: ['this']
    }),
    {
      QUERY: { AND: ['this'] },
      OPTIONS: {},
      RESULT: new Array(size).fill().map((item, i) => ({
        _id: i,
        _match: [{ FIELD: 'text', VALUE: 'this', SCORE: '1.00' }]
      })),
      RESULT_LENGTH: size,
      PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
    }
  )
})
