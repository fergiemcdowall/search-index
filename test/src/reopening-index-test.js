import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'reopening'
const global = {}

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

test('can create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = new SearchIndex({ name: indexName })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add some data', t => {
  t.plan(1)
  // global[indexName].PUT(data).then(t.pass)
  const { PUT } = global[indexName]
  PUT(data).then(t.pass)
})

test('simple SEARCH with 1 clause', t => {
  t.plan(1)
  global[indexName].SEARCH(['document']).then(res => {
    t.deepEqual(res, {
      QUERY: { AND: ['document'] },
      OPTIONS: { SCORE: { TYPE: 'TFIDF' }, SORT: true },
      RESULT: [
        {
          _id: 'b',
          _match: [
            { FIELD: 'body.text', VALUE: 'document', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'document', SCORE: '1.00' }
          ],
          _score: 1.39
        },
        {
          _id: 'a',
          _match: [
            { FIELD: 'body.text', VALUE: 'document', SCORE: '0.33' },
            { FIELD: 'title', VALUE: 'document', SCORE: '1.00' }
          ],
          _score: 0.92
        }
      ],
      RESULT_LENGTH: 2,
      PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
    })
  })
})

test('closing instance', t => {
  t.plan(1)
  global[indexName].INDEX.STORE.close().then(() => {
    global[indexName] = null
    t.ok('closed')
  })
})

test('confirm index is closed', t => {
  t.plan(1)
  t.equals(global[indexName], null)
})

test('reopen index', async t => {
  t.plan(1)
  try {
    global[indexName] = new SearchIndex({ name: indexName })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('simple SEARCH with 1 clause', t => {
  t.plan(1)

  global[indexName].EVENTS.on('ready', () =>
    global[indexName].SEARCH(['document']).then(res => {
      t.deepEqual(res, {
        QUERY: { AND: ['document'] },
        OPTIONS: { SCORE: { TYPE: 'TFIDF' }, SORT: true },
        RESULT: [
          {
            _id: 'b',
            _match: [
              { FIELD: 'body.text', VALUE: 'document', SCORE: '1.00' },
              { FIELD: 'title', VALUE: 'document', SCORE: '1.00' }
            ],
            _score: 1.39
          },
          {
            _id: 'a',
            _match: [
              { FIELD: 'body.text', VALUE: 'document', SCORE: '0.33' },
              { FIELD: 'title', VALUE: 'document', SCORE: '1.00' }
            ],
            _score: 0.92
          }
        ],
        RESULT_LENGTH: 2,
        PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
      })
    })
  )
})
