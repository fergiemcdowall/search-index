const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'

let i = 0

test('create a search index with no character normalisation', async t => {
  const data = ['jeg bør finne en ting', 'jeg bor i Oslo']
  const { PUT, SEARCH } = await si({
    name: sandbox + 'char-norm-' + ++i
  })
  const ids = (await PUT(data)).map(status => status._id)
  t.deepEquals(await SEARCH(['bor']), {
    RESULT: [
      {
        _id: ids[1],
        _match: [{ FIELD: 'body', VALUE: 'bor', SCORE: '1.00' }],
        _score: 1.1
      }
    ],
    RESULT_LENGTH: 1
  })
})

test('create a search index with query side character normalisation (_GET)', async t => {
  const swapØtoO = token =>
    new Promise(resolve => {
      token.VALUE.GTE = token.VALUE.GTE.replace(/ø/g, 'o')
      token.VALUE.LTE = token.VALUE.LTE.replace(/ø/g, 'o')
      return resolve(token)
    })

  const data = ['jeg bør finne en ting', 'jeg bor i Oslo']
  const { PUT, _GET } = await si({
    name: sandbox + 'char-norm-' + ++i
  })
  const ids = (await PUT(data)).map(status => status._id)
  t.deepEquals(await _GET('bør', swapØtoO), [
    {
      _id: ids[1],
      _match: [{ FIELD: 'body', VALUE: 'bor', SCORE: '1.00' }]
    }
  ])
})

test('create a search index with query side character normalisation (_AND)', async t => {
  const swapØtoO = token =>
    new Promise(resolve => {
      token.VALUE.GTE = token.VALUE.GTE.replace(/ø/g, 'o')
      token.VALUE.LTE = token.VALUE.LTE.replace(/ø/g, 'o')
      return resolve(token)
    })

  const data = ['jeg bør finne en ting', 'jeg bor i Oslo']
  const { PUT, _AND } = await si({
    name: sandbox + 'char-norm-' + ++i
  })
  const ids = (await PUT(data)).map(status => status._id)
  t.deepEquals(await _AND(['bør'], swapØtoO), [
    {
      _id: ids[1],
      _match: [{ FIELD: 'body', VALUE: 'bor', SCORE: '1.00' }]
    }
  ])
})

test('create a search index with query side character normalisation (QUERY)', async t => {
  const swapØtoO = token =>
    new Promise(resolve => {
      console.log('in PIPELINE')
      token.VALUE.GTE = token.VALUE.GTE.replace(/ø/g, 'o')
      token.VALUE.LTE = token.VALUE.LTE.replace(/ø/g, 'o')
      return resolve(token)
    })

  const data = ['jeg bør finne en ting', 'jeg bor i Oslo']
  const { PUT, QUERY } = await si({
    name: sandbox + 'char-norm-' + ++i
  })
  const ids = (await PUT(data)).map(status => status._id)
  t.deepEquals(
    await QUERY(
      {
        AND: ['bør']
      },
      {
        PIPELINE: swapØtoO
      }
    ),
    {
      RESULT: [
        {
          _id: ids[1],
          _match: [{ FIELD: 'body', VALUE: 'bor', SCORE: '1.00' }]
        }
      ],
      RESULT_LENGTH: 1
    }
  )
})

test('create a search index with query side character normalisation (SEARCH)', async t => {
  const swapØtoO = token =>
    new Promise(resolve => {
      token.VALUE.GTE = token.VALUE.GTE.replace(/ø/g, 'o')
      token.VALUE.LTE = token.VALUE.LTE.replace(/ø/g, 'o')
      return resolve(token)
    })

  const data = ['jeg bør finne en ting', 'jeg bor i Oslo']
  const { PUT, SEARCH } = await si({
    name: sandbox + 'char-norm-' + ++i
  })
  const ids = (await PUT(data)).map(status => status._id)
  t.deepEquals(
    await SEARCH(['bør'], {
      PIPELINE: swapØtoO
    }),
    {
      RESULT: [
        {
          _id: ids[1],
          _match: [{ FIELD: 'body', VALUE: 'bor', SCORE: '1.00' }],
          _score: 1.1
        }
      ],
      RESULT_LENGTH: 1
    }
  )
})

test('create a search index with query and index side character normalisation', async t => {
  const swapØtoO = token =>
    new Promise(resolve => {
      token.VALUE.GTE = token.VALUE.GTE.replace(/ø/g, 'o')
      token.VALUE.LTE = token.VALUE.LTE.replace(/ø/g, 'o')
      return resolve(token)
    })

  const data = ['jeg bør finne en ting', 'jeg bor i Oslo']
  const { PUT, SEARCH, TOKENIZATION_PIPELINE_STAGES } = await si({
    name: sandbox + 'char-norm-' + ++i
  })
  const ids = (
    await PUT(data, {
      tokenizer: (tokens, field, ops) =>
        TOKENIZATION_PIPELINE_STAGES.SPLIT([tokens, field, ops])
          .then(TOKENIZATION_PIPELINE_STAGES.LOWCASE)
          .then(([tokens, field, ops]) => [
            tokens.map(t => t.replace(/ø/g, 'o')),
            field,
            ops
          ])
          .then(TOKENIZATION_PIPELINE_STAGES.SCORE_TERM_FREQUENCY)
          .then(([tokens]) => tokens)
    })
  ).map(status => status._id)
  t.deepEquals(await SEARCH(['bor'], swapØtoO), {
    RESULT: [
      {
        _id: ids[0],
        _match: [{ FIELD: 'body', VALUE: 'bor', SCORE: '1.00' }],
        _score: 0.41
      },
      {
        _id: ids[1],
        _match: [{ FIELD: 'body', VALUE: 'bor', SCORE: '1.00' }],
        _score: 0.41
      }
    ],
    RESULT_LENGTH: 2
  })
})
