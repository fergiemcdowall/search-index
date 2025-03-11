import { eng } from 'stopword'
import test from 'tape'
import { SearchIndex } from 'search-index'
import { stemmer } from 'stemmer'

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

  const { DICTIONARY, DELETE, PUT, TOKENIZATION_PIPELINE_STAGES } =
    new SearchIndex({
      name: sandbox + 'pipeline'
    })
  t.ok(PUT)

  const { SPLIT, LOWCASE, NGRAMS, STOPWORDS, SCORE_TERM_FREQUENCY } =
    TOKENIZATION_PIPELINE_STAGES

  t.deepEquals(
    await PUT(docs, {
      stopwords: eng,
      ngrams: {
        fields: ['line2'],
        lengths: [1, 2]
      },
      tokenizer: (tokens, field, ops) =>
        SPLIT([tokens, field, ops])
          .then(LOWCASE)
          .then(NGRAMS)
          .then(STOPWORDS)
          .then(SCORE_TERM_FREQUENCY)
          .then(([tokens]) => tokens)
    }),
    [
      { _id: 0, status: 'CREATED', operation: 'PUT' },
      { _id: 1, status: 'CREATED', operation: 'PUT' }
    ]
  )

  t.deepEquals(
    await DICTIONARY({
      FIELD: 'line2'
    }),
    {
      RESULT: [
        'and thyme',
        'parsley',
        'parsley sage',
        'rosemary',
        'rosemary and',
        'sage',
        'sage rosemary',
        'thyme'
      ],
      OPTIONS: {}
    }
  )

  t.deepEquals(await DELETE(0, 1), [
    { _id: 0, operation: 'DELETE', status: 'DELETED' },
    { _id: 1, operation: 'DELETE', status: 'DELETED' }
  ])

  t.deepEquals(
    await PUT(docs, {
      stopwords: eng,
      ngrams: {
        fields: ['line2'],
        lengths: [1, 2]
      },
      tokenizer: (tokens, field, ops) =>
        SPLIT([tokens, field, ops])
          .then(LOWCASE)
          .then(STOPWORDS)
          .then(NGRAMS)
          .then(SCORE_TERM_FREQUENCY)
          .then(([tokens]) => tokens)
    }),
    [
      { _id: 0, status: 'CREATED', operation: 'PUT' },
      { _id: 1, status: 'CREATED', operation: 'PUT' }
    ]
  )

  // because STOPWORDS came before NGRAMS in pipeline, ngrams do not
  // contain stopwords (no 'and thyme' for example)
  t.deepEquals(
    await DICTIONARY({
      FIELD: 'line2'
    }),
    {
      RESULT: [
        'parsley',
        'parsley sage',
        'rosemary',
        'rosemary thyme',
        'sage',
        'sage rosemary',
        'thyme'
      ],
      OPTIONS: {}
    }
  )
})

test('can add custom pipeline stage', async function (t) {
  t.plan(4)

  const { DICTIONARY, PUT, TOKENIZATION_PIPELINE_STAGES } = new SearchIndex({
    name: sandbox + 'pipeline2'
  })

  const { SPLIT, LOWCASE, NGRAMS, STOPWORDS, SCORE_TERM_FREQUENCY } =
    TOKENIZATION_PIPELINE_STAGES

  t.ok(PUT)

  t.deepEquals(
    await PUT(docs, {
      tokenizer: (tokens, field, ops) =>
        SPLIT([tokens, field, ops])
          .then(LOWCASE)
          .then(NGRAMS)
          .then(([tokens, field, ops]) => [
            [field.split('').reverse().join(''), ...tokens],
            field,
            ops
          ])
          .then(STOPWORDS)
          .then(SCORE_TERM_FREQUENCY)
          .then(([tokens]) => tokens)
    }),
    [
      { _id: 0, status: 'CREATED', operation: 'PUT' },
      { _id: 1, status: 'CREATED', operation: 'PUT' }
    ]
  )

  t.deepEquals(
    await DICTIONARY({
      FIELD: 'line1'
    }),
    {
      RESULT: [
        '1enil',
        'a',
        'are',
        'cambric',
        'deep',
        'fair',
        'forest',
        'going',
        'green',
        'her',
        'in',
        'make',
        'me',
        'scarborough',
        'shirt',
        'tell',
        'the',
        'to',
        'you'
      ],
      OPTIONS: {}
    }
  )
  t.deepEquals(
    await DICTIONARY({
      FIELD: 'line2'
    }),
    {
      RESULT: ['2enil', 'and', 'parsley', 'rosemary', 'sage', 'thyme'],
      OPTIONS: {}
    }
  )
})

test('can add custom pipeline stage (stemmer)', async function (t) {
  t.plan(3)

  const {
    DICTIONARY,
    PUT,
    TOKENIZATION_PIPELINE_STAGES: {
      SPLIT,
      LOWCASE,
      NGRAMS,
      STOPWORDS,
      SCORE_TERM_FREQUENCY
    }
  } = new SearchIndex({
    name: sandbox + 'pipeline3'
  })

  t.ok(PUT)

  t.deepEquals(
    await PUT(docs, {
      stopwords: eng,
      tokenizer: (tokens, field, ops) =>
        SPLIT([tokens, field, ops])
          .then(LOWCASE)
          .then(NGRAMS)
          .then(STOPWORDS)
          .then(([tokens, field, ops]) => [tokens.map(stemmer), field, ops])
          .then(SCORE_TERM_FREQUENCY)
          .then(([tokens]) => tokens)
    }),
    [
      { _id: 0, status: 'CREATED', operation: 'PUT' },
      { _id: 1, status: 'CREATED', operation: 'PUT' }
    ]
  )

  t.deepEquals(
    await DICTIONARY({
      FIELD: 'line3'
    }),
    {
      RESULT: [
        'crest',
        'ground',
        'live',
        'on',
        'rememb',
        'snow',
        'sparrow',
        'trace'
      ],
      OPTIONS: {}
    }
  )
})
