const si = require('../../')
const test = require('tape')
const sw = require('stopword')

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

test('can alter order of tokenization pipeline', async function (t) {
  t.plan(6)

  const { PUT, DELETE, DICTIONARY, SPLIT, LOWCASE, NGRAMS, STOPWORDS, SCORE_TFIDF } = await si({
    name: sandbox + 'pipeline'
  })
  t.ok(PUT)

  t.deepEquals(await PUT(docs, {
    stopwords: sw.en,
    ngrams: {
      fields: ['line2'],
      lengths: [1, 2]
    },
    tokenizerPipeline: [
      SPLIT,
      LOWCASE,
      NGRAMS,
      STOPWORDS,
      SCORE_TFIDF
    ]
  }), [
    { _id: '0', status: 'CREATED', operation: 'PUT' },
    { _id: '1', status: 'CREATED', operation: 'PUT' }
  ])

  t.deepEquals(await DICTIONARY({
    FIELD: 'line2'
  }), [
    'and thyme', 'parsley', 'parsley sage',
    'rosemary', 'rosemary and', 'sage',
    'sage rosemary', 'thyme'
  ])

  t.deepEquals(await DELETE(0, 1), [
    { _id: '0', operation: 'DELETE', status: 'DELETED' },
    { _id: '1', operation: 'DELETE', status: 'DELETED' }
  ])

  t.deepEquals(await PUT(docs, {
    stopwords: sw.en,
    ngrams: {
      fields: ['line2'],
      lengths: [1, 2]
    },
    tokenizerPipeline: [
      SPLIT,
      LOWCASE,
      STOPWORDS,
      NGRAMS,
      SCORE_TFIDF
    ]
  }), [
    { _id: '0', status: 'CREATED', operation: 'PUT' },
    { _id: '1', status: 'CREATED', operation: 'PUT' }
  ])

  // because STOPWORDS came before NGRAMS in pipeline, ngrams do not
  // contain stopwords (no 'and thyme' for example)
  t.deepEquals(await DICTIONARY({
    FIELD: 'line2'
  }), [
    'parsley', 'parsley sage', 'rosemary',
    'rosemary thyme', 'sage', 'sage rosemary',
    'thyme'
  ])
})
