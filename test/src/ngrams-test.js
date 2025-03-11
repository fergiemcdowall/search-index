import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'

const docs = [
  {
    _id: 0,
    line1: 'Are you going to Scarborough Fair?',
    line2: 'Parsley, sage, rosemary, and thyme',
    line3: 'Remember me to one who lives there'
  },
  {
    _id: 1,
    line1: 'Tell her to make me a cambric shirt (in the deep forest green)',
    line2: 'Parsley, sage, rosemary, and thyme',
    line3: '(Tracing of sparrow on snow-crested ground)'
  }
]

// TODO: can test with no fields
// TODO: can join with other characters than space
test('create a search index with ngrams', async function (t) {
  t.plan(9)

  const si = await new SearchIndex({
    name: sandbox + 'ngrams1',
    ngrams: {
      fields: ['line2', 'line3'],
      lengths: [1, 2, 3]
    }
  })
  t.ok(si.PUT)
  t.ok(si.DICTIONARY)

  t.deepEquals(
    await si.PUT(docs, {
      storeRawDocs: false
    }),
    [
      { _id: 0, status: 'CREATED', operation: 'PUT' },
      { _id: 1, status: 'CREATED', operation: 'PUT' }
    ]
  )

  t.deepEquals(await si.DICTIONARY('who'), {
    RESULT: ['who', 'who lives', 'who lives there'],
    OPTIONS: {}
  })
  t.deepEquals(await si.DICTIONARY('are'), {
    RESULT: ['are'],
    OPTIONS: {}
  })
  t.deepEquals(await si.DICTIONARY('xxx'), {
    RESULT: [],
    OPTIONS: {}
  })
  t.deepEquals(await si.DICTIONARY('parsley'), {
    RESULT: ['parsley', 'parsley sage', 'parsley sage rosemary'],
    OPTIONS: {}
  })
  t.deepEquals(await si.DICTIONARY('lives'), {
    RESULT: ['lives', 'lives there'],
    OPTIONS: {}
  })
  t.deepEquals(await si.QUERY('lives there'), {
    QUERY: 'lives there',
    OPTIONS: {},
    RESULT: [
      {
        _id: 0,
        _match: [{ FIELD: 'line3', VALUE: 'lives there', SCORE: '1.00' }]
      }
    ],
    RESULT_LENGTH: 1,
    PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
  })
})

test('create a search index with ngrams (no fields specified)', async function (t) {
  t.plan(8)

  const si = new SearchIndex({
    name: sandbox + 'ngrams2',
    ngrams: {
      lengths: [1, 2, 3]
    }
  })
  t.ok(si.PUT)

  t.deepEquals(await si.PUT(docs), [
    { _id: 0, status: 'CREATED', operation: 'PUT' },
    { _id: 1, status: 'CREATED', operation: 'PUT' }
  ])

  t.deepEquals(await si.DICTIONARY('who'), {
    RESULT: ['who', 'who lives', 'who lives there'],
    OPTIONS: {}
  })
  t.deepEquals(await si.DICTIONARY('are'), {
    RESULT: ['are', 'are you', 'are you going'],
    OPTIONS: {}
  })
  t.deepEquals(await si.DICTIONARY('xxx'), {
    RESULT: [],
    OPTIONS: {}
  })
  t.deepEquals(await si.DICTIONARY('parsley'), {
    RESULT: ['parsley', 'parsley sage', 'parsley sage rosemary'],
    OPTIONS: {}
  })
  t.deepEquals(await si.DICTIONARY('lives'), {
    RESULT: ['lives', 'lives there'],
    OPTIONS: {}
  })
  t.deepEquals(await si.QUERY('lives there'), {
    QUERY: 'lives there',
    OPTIONS: {},
    RESULT: [
      {
        _id: 0,
        _match: [{ FIELD: 'line3', VALUE: 'lives there', SCORE: '1.00' }]
      }
    ],
    RESULT_LENGTH: 1,
    PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
  })
})

test('create a search index with ngrams (no fields specified, custom join)', async function (t) {
  t.plan(8)

  const si = new SearchIndex({
    name: sandbox + 'ngrams3',
    ngrams: {
      lengths: [1, 2, 3],
      join: '$'
    }
  })
  t.ok(si.PUT)

  t.deepEquals(await si.PUT(docs), [
    { _id: 0, status: 'CREATED', operation: 'PUT' },
    { _id: 1, status: 'CREATED', operation: 'PUT' }
  ])

  t.deepEquals(await si.DICTIONARY('who'), {
    RESULT: ['who', 'who$lives', 'who$lives$there'],
    OPTIONS: {}
  })
  t.deepEquals(await si.DICTIONARY('are'), {
    RESULT: ['are', 'are$you', 'are$you$going'],
    OPTIONS: {}
  })
  t.deepEquals(await si.DICTIONARY('xxx'), { RESULT: [], OPTIONS: {} })
  t.deepEquals(await si.DICTIONARY('parsley'), {
    RESULT: ['parsley', 'parsley$sage', 'parsley$sage$rosemary'],
    OPTIONS: {}
  })
  t.deepEquals(await si.DICTIONARY('lives'), {
    RESULT: ['lives', 'lives$there'],
    OPTIONS: {}
  })
  t.deepEquals(await si.QUERY('lives$there'), {
    QUERY: 'lives$there',
    OPTIONS: {},
    RESULT: [
      {
        _id: 0,
        _match: [{ FIELD: 'line3', VALUE: 'lives$there', SCORE: '1.00' }]
      }
    ],
    RESULT_LENGTH: 1,
    PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
  })
})
