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

  const { PUT, DICTIONARY, QUERY } = await new SearchIndex({
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
  t.ok(PUT)
  t.ok(DICTIONARY)

  t.deepEquals(await PUT(docs), [
    { _id: 0, status: 'CREATED', operation: 'PUT' },
    { _id: 1, status: 'CREATED', operation: 'PUT' }
  ])

  t.deepEquals(await DICTIONARY('animal'), ['animal'])
  t.deepEquals(await DICTIONARY('herb'), ['herb'])
  t.deepEquals(await DICTIONARY('livestock'), [])

  t.deepEquals(await QUERY('sparrow'), {
    RESULT: [
      { _id: 1, _match: [{ FIELD: 'line3', VALUE: 'sparrow', SCORE: '1.00' }] }
    ],
    RESULT_LENGTH: 1
  })

  t.deepEquals(await QUERY('bird'), {
    RESULT: [
      { _id: 1, _match: [{ FIELD: 'line3', VALUE: 'bird', SCORE: '1.00' }] }
    ],
    RESULT_LENGTH: 1
  })
})

test('create a search index with synonyms (specific fields)', async function (t) {
  t.plan(6)

  const { PUT, DICTIONARY, QUERY } = await new SearchIndex({
    name: sandbox + 'REPLACE2',
    replace: {
      fields: ['line1'],
      values: {
        me: ['myself']
      }
    }
  })
  t.ok(PUT)
  t.ok(DICTIONARY)

  t.deepEquals(await PUT(docs), [
    { _id: 0, status: 'CREATED', operation: 'PUT' },
    { _id: 1, status: 'CREATED', operation: 'PUT' }
  ])

  t.deepEquals(await DICTIONARY('myself'), ['myself'])

  t.deepEquals(await QUERY('me'), {
    RESULT: [
      { _id: 1, _match: [{ FIELD: 'line1', VALUE: 'me', SCORE: '1.00' }] },
      { _id: 0, _match: [{ FIELD: 'line3', VALUE: 'me', SCORE: '1.00' }] }
    ],
    RESULT_LENGTH: 2
  })

  t.deepEquals(await QUERY('myself'), {
    RESULT: [
      { _id: 1, _match: [{ FIELD: 'line1', VALUE: 'myself', SCORE: '1.00' }] }
    ],
    RESULT_LENGTH: 1
  })
})
