import test from 'tape'
import { MemoryLevel } from 'memory-level'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'memdown-test'

const data = [
  {
    _id: 'a',
    title: 'quite a cool document',
    body: {
      text: 'this document is really cool cool cool',
      metadata: 'coolness documentness'
    },
    importantNumber: 5000
  },
  {
    _id: 'b',
    title: 'quite a cool document',
    body: {
      text: 'this document is really cool bananas',
      metadata: 'coolness documentness'
    },
    importantNumber: 500
  },
  {
    _id: 'c',
    title: 'something different',
    body: {
      text: 'something totally different',
      metadata: 'coolness documentness'
    },
    importantNumber: 200
  }
]

test('create a search-index with memory-level', async t => {
  t.plan(2)

  const idx = new SearchIndex({
    Level: MemoryLevel,
    name: indexName
  })

  const res = await idx.PUT(data)

  t.deepEqual(res, [
    { _id: 'a', status: 'CREATED', operation: 'PUT' },
    { _id: 'b', status: 'CREATED', operation: 'PUT' },
    { _id: 'c', status: 'CREATED', operation: 'PUT' }
  ])

  const res2 = await idx.SEARCH([
    'body.text:cool',
    'body.text:really',
    'body.text:bananas'
  ])

  t.deepEquals(res2, {
    QUERY: { AND: ['body.text:cool', 'body.text:really', 'body.text:bananas'] },
    OPTIONS: { SCORE: { TYPE: 'TFIDF' }, SORT: true },
    RESULT: [
      {
        _id: 'b',
        _match: [
          { FIELD: 'body.text', VALUE: 'bananas', SCORE: '1.00' },
          { FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' },
          { FIELD: 'body.text', VALUE: 'really', SCORE: '1.00' }
        ],
        _score: 4.16
      }
    ],
    RESULT_LENGTH: 1,
    PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
  })
})
