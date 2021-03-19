const si = require('../../')
const test = require('tape')

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

test('create a search index with ngrams', async function (t) {
  t.plan(9)

  const { PUT, DICTIONARY, QUERY } = await si({
    name: sandbox + 'ngrams1',
    ngrams: {
      fields: ['line2', 'line3'],
      lengths: [1, 2, 3]
    }
  })
  t.ok(PUT)
  t.ok(DICTIONARY)

  t.deepEquals(await PUT(docs, {
    storeRawDocs: false
  }), [
    { _id: '0', status: 'CREATED', operation: 'PUT' },
    { _id: '1', status: 'CREATED', operation: 'PUT' }
  ])

  t.deepEquals(await DICTIONARY('who'), [
    'who', 'who lives', 'who lives there'
  ])
  t.deepEquals(await DICTIONARY('are'), ['are'])
  t.deepEquals(await DICTIONARY('xxx'), [])
  t.deepEquals(await DICTIONARY('parsley'), [
    'parsley', 'parsley sage', 'parsley sage rosemary'
  ])
  t.deepEquals(await DICTIONARY('lives'), ['lives', 'lives there'])
  t.deepEquals(await QUERY('lives there'), {
    RESULT: [
      { _id: '0', _match: ['line3:lives there#1.00'] }
    ],
    RESULT_LENGTH: 1
  })
})
