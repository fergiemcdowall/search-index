import stemmer from 'stemmer'
import sw from 'stopword'
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

test('can alter order of tokenization pipeline', async function (t) {
  t.plan(6)

  const si = new SearchIndex({
    name: sandbox + 'pipeline'
  })
  t.ok(si.PUT)

  t.deepEquals(
    await si.PUT(docs, {
      stopwords: sw.eng,
      ngrams: {
        fields: ['line2'],
        lengths: [1, 2]
      },
      tokenizer: (tokens, field, ops) =>
        si.TOKENIZATION_PIPELINE_STAGES.SPLIT([tokens, field, ops])
          .then(si.TOKENIZATION_PIPELINE_STAGES.LOWCASE)
          .then(si.TOKENIZATION_PIPELINE_STAGES.NGRAMS)
          .then(si.TOKENIZATION_PIPELINE_STAGES.STOPWORDS)
          .then(si.TOKENIZATION_PIPELINE_STAGES.SCORE_TERM_FREQUENCY)
          .then(([tokens]) => tokens)
    }),
    [
      { _id: 0, status: 'CREATED', operation: 'PUT' },
      { _id: 1, status: 'CREATED', operation: 'PUT' }
    ]
  )

  t.deepEquals(
    await si.DICTIONARY({
      FIELD: 'line2'
    }),
    [
      'and thyme',
      'parsley',
      'parsley sage',
      'rosemary',
      'rosemary and',
      'sage',
      'sage rosemary',
      'thyme'
    ]
  )

  t.deepEquals(await si.DELETE(0, 1), [
    { _id: 0, operation: 'DELETE', status: 'DELETED' },
    { _id: 1, operation: 'DELETE', status: 'DELETED' }
  ])

  t.deepEquals(
    await si.PUT(docs, {
      stopwords: sw.eng,
      ngrams: {
        fields: ['line2'],
        lengths: [1, 2]
      },
      tokenizer: (tokens, field, ops) =>
        si.TOKENIZATION_PIPELINE_STAGES.SPLIT([tokens, field, ops])
          .then(si.TOKENIZATION_PIPELINE_STAGES.LOWCASE)
          .then(si.TOKENIZATION_PIPELINE_STAGES.STOPWORDS)
          .then(si.TOKENIZATION_PIPELINE_STAGES.NGRAMS)
          .then(si.TOKENIZATION_PIPELINE_STAGES.SCORE_TERM_FREQUENCY)
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
    await si.DICTIONARY({
      FIELD: 'line2'
    }),
    [
      'parsley',
      'parsley sage',
      'rosemary',
      'rosemary thyme',
      'sage',
      'sage rosemary',
      'thyme'
    ]
  )
})

// test('can add custom pipeline stage', async function (t) {
//   t.plan(4)

//   const si = new SearchIndex({
//     name: sandbox + 'pipeline2'
//   })
//   t.ok(si.PUT)

//   t.deepEquals(
//     await si.PUT(docs, {
//       tokenizer: (tokens, field, ops) =>
//         si.TOKENIZATION_PIPELINE_STAGES.SPLIT([tokens, field, ops])
//           .then(si.TOKENIZATION_PIPELINE_STAGES.LOWCASE)
//           .then(si.TOKENIZATION_PIPELINE_STAGES.NGRAMS)
//           .then(([tokens, field, ops]) => [
//             [field.split('').reverse().join(''), ...tokens],
//             field,
//             ops
//           ])
//           .then(si.TOKENIZATION_PIPELINE_STAGES.STOPWORDS)
//           .then(si.TOKENIZATION_PIPELINE_STAGES.SCORE_TERM_FREQUENCY)
//           .then(([tokens]) => tokens)
//     }),
//     [
//       { _id: 0, status: 'CREATED', operation: 'PUT' },
//       { _id: 1, status: 'CREATED', operation: 'PUT' }
//     ]
//   )

//   t.deepEquals(
//     await si.DICTIONARY({
//       FIELD: 'line1'
//     }),
//     [
//       '1enil',
//       'a',
//       'are',
//       'cambric',
//       'deep',
//       'fair',
//       'forest',
//       'going',
//       'green',
//       'her',
//       'in',
//       'make',
//       'me',
//       'scarborough',
//       'shirt',
//       'tell',
//       'the',
//       'to',
//       'you'
//     ]
//   )
//   t.deepEquals(
//     await si.DICTIONARY({
//       FIELD: 'line2'
//     }),
//     ['2enil', 'and', 'parsley', 'rosemary', 'sage', 'thyme']
//   )
// })

// test('can add custom pipeline stage (stemmer)', async function (t) {
//   t.plan(3)

//   const si = new SearchIndex({
//     name: sandbox + 'pipeline3'
//   })
//   t.ok(si.PUT)

//   t.deepEquals(
//     await si.PUT(docs, {
//       stopwords: sw.eng,
//       tokenizer: (tokens, field, ops) =>
//         si.TOKENIZATION_PIPELINE_STAGES.SPLIT([tokens, field, ops])
//           .then(si.TOKENIZATION_PIPELINE_STAGES.LOWCASE)
//           .then(si.TOKENIZATION_PIPELINE_STAGES.NGRAMS)
//           .then(si.TOKENIZATION_PIPELINE_STAGES.STOPWORDS)
//           .then(([tokens, field, ops]) => [tokens.map(stemmer), field, ops])
//           .then(si.TOKENIZATION_PIPELINE_STAGES.SCORE_TERM_FREQUENCY)
//           .then(([tokens]) => tokens)
//     }),
//     [
//       { _id: 0, status: 'CREATED', operation: 'PUT' },
//       { _id: 1, status: 'CREATED', operation: 'PUT' }
//     ]
//   )

//   t.deepEquals(
//     await si.DICTIONARY({
//       FIELD: 'line3'
//     }),
//     ['crest', 'ground', 'live', 'on', 'rememb', 'snow', 'sparrow', 'trace']
//   )
// })
