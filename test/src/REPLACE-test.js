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

test('create a search index with synonyms (can be in all fields)', async function (t) {
  t.plan(8)

  const si = new SearchIndex({
    name: sandbox + 'REPLACE1',
    replace: {
      values: {
        parsley: ['herb'],
        sage: ['herb'],
        rosemary: ['herb'],
        sheep: ['animal', 'livestock'],
        sparrow: ['animal', 'bird']
      }
    }
  })
  t.ok(si.PUT)
  t.ok(si.DICTIONARY)

  t.deepEquals(await si.PUT(docs), [
    { _id: 0, status: 'CREATED', operation: 'PUT' },
    { _id: 1, status: 'CREATED', operation: 'PUT' }
  ])

  t.deepEquals(await si.DICTIONARY('animal'), ['animal'])
  t.deepEquals(await si.DICTIONARY('herb'), ['herb'])
  t.deepEquals(await si.DICTIONARY('livestock'), [])

  t.deepEquals(await si.QUERY('sparrow'), {
    RESULT: [
      { _id: 1, _match: [{ FIELD: 'line3', VALUE: 'sparrow', SCORE: '1.00' }] }
    ],
    RESULT_LENGTH: 1,
    PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
  })

  t.deepEquals(await si.QUERY('bird'), {
    RESULT: [
      { _id: 1, _match: [{ FIELD: 'line3', VALUE: 'bird', SCORE: '1.00' }] }
    ],
    RESULT_LENGTH: 1,
    PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
  })
})

test('create a search index with synonyms (specific fields)', async function (t) {
  t.plan(6)

  const si = await new SearchIndex({
    name: sandbox + 'REPLACE2',
    replace: {
      fields: ['line1'],
      values: {
        me: ['myself']
      }
    }
  })
  t.ok(si.PUT)
  t.ok(si.DICTIONARY)

  t.deepEquals(await si.PUT(docs), [
    { _id: 0, status: 'CREATED', operation: 'PUT' },
    { _id: 1, status: 'CREATED', operation: 'PUT' }
  ])

  t.deepEquals(await si.DICTIONARY('myself'), ['myself'])

  t.deepEquals(await si.QUERY('me'), {
    RESULT: [
      { _id: 1, _match: [{ FIELD: 'line1', VALUE: 'me', SCORE: '1.00' }] },
      { _id: 0, _match: [{ FIELD: 'line3', VALUE: 'me', SCORE: '1.00' }] }
    ],
    RESULT_LENGTH: 2,
    PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
  })

  t.deepEquals(await si.QUERY('myself'), {
    RESULT: [
      { _id: 1, _match: [{ FIELD: 'line1', VALUE: 'myself', SCORE: '1.00' }] }
    ],
    RESULT_LENGTH: 1,
    PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
  })
})
