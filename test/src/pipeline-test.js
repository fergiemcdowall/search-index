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

  const { PUT, DELETE, DICTIONARY, TOKENIZATION_PIPELINE_STAGES } = await si({
    name: sandbox + 'pipeline'
  })
  t.ok(PUT)

  t.deepEquals(await PUT(docs, {
    stopwords: sw.en,
    ngrams: {
      fields: ['line2'],
      lengths: [1, 2]
    },
    tokenizationPipeline: [
      TOKENIZATION_PIPELINE_STAGES.SPLIT,
      TOKENIZATION_PIPELINE_STAGES.LOWCASE,
      TOKENIZATION_PIPELINE_STAGES.NGRAMS,
      TOKENIZATION_PIPELINE_STAGES.STOPWORDS,
      TOKENIZATION_PIPELINE_STAGES.SCORE_TERM_FREQUENCY
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
    tokenizationPipeline: [
      TOKENIZATION_PIPELINE_STAGES.SPLIT,
      TOKENIZATION_PIPELINE_STAGES.LOWCASE,
      TOKENIZATION_PIPELINE_STAGES.STOPWORDS,
      TOKENIZATION_PIPELINE_STAGES.NGRAMS,
      TOKENIZATION_PIPELINE_STAGES.SCORE_TERM_FREQUENCY
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

// TODO: add custom pipeline stage

test('can add custom pipeline stage', async function (t) {
  t.plan(4)

  const { PUT, DICTIONARY, TOKENIZATION_PIPELINE_STAGES } = await si({
    name: sandbox + 'pipeline2'
  })
  t.ok(PUT)

  t.deepEquals(await PUT(docs, {
    tokenizationPipeline: [
      TOKENIZATION_PIPELINE_STAGES.SPLIT,
      TOKENIZATION_PIPELINE_STAGES.LOWCASE,
      TOKENIZATION_PIPELINE_STAGES.NGRAMS,
      (tokens, field, ops) => [...tokens, field.split('').reverse().join('')],
      TOKENIZATION_PIPELINE_STAGES.STOPWORDS,
      TOKENIZATION_PIPELINE_STAGES.SCORE_TERM_FREQUENCY
    ]
  }), [
    { _id: '0', status: 'CREATED', operation: 'PUT' },
    { _id: '1', status: 'CREATED', operation: 'PUT' }
  ])

  t.deepEquals(await DICTIONARY({
    FIELD: 'line1'
  }), [
    '1enil', 'a', 'are', 'cambric', 'deep', 'fair', 'forest', 'going', 'green', 'her', 'in', 'make', 'me', 'scarborough', 'shirt', 'tell', 'the', 'to', 'you'
  ])
  t.deepEquals(await DICTIONARY({
    FIELD: 'line2'
  }), [
    '2enil', 'and', 'parsley', 'rosemary', 'sage', 'thyme'
  ])
})
