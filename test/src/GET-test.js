const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_GET'

test('create a search index', t => {
  t.plan(1)
  si({ name: indexName }).then(db => {
    global[indexName] = db
    t.pass('ok')
  })
})

test('can add data', t => {
  const data = [
    {
      _id: '0',
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      extraField: 'EXTRA FIELD- w00t!'
    },
    {
      _id: '1',
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: '2',
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    },
    {
      _id: '3',
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW',
      extraField: 'EXTRA FIELD- w00t!'
    },
    {
      _id: '4',
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: '5',
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    },
    {
      _id: '6',
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW',
      extraField: 'EXTRA FIELD- w00t!'
    },
    {
      _id: '7',
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla'
    },
    {
      _id: '8',
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla'
    },
    {
      _id: '9',
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('simple _GET', t => {
  t.plan(1)
  global[indexName]._GET('make:volvo').then(res => {
    t.deepEqual(res, [
      {
        _id: '4',
        _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
      },
      {
        _id: '5',
        _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
      },
      { _id: '8', _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] }
    ])
  })
})

test('simple _GET', t => {
  t.plan(1)
  global[indexName]
    ._GET({
      FIELD: 'make',
      VALUE: 'volvo'
    })
    .then(res => {
      t.deepEqual(res, [
        {
          _id: '4',
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: '5',
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        },
        { _id: '8', _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }] }
      ])
    })
})

test('_GET over 2 fields', t => {
  t.plan(1)
  global[indexName]
    ._GET({
      FIELD: ['make', 'brand'],
      VALUE: 'volvo'
    })
    .then(res => {
      t.deepEqual(res, [
        {
          _id: '4',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }
          ]
        },
        {
          _id: '5',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }
          ]
        },
        {
          _id: '8',
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: '0',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: '1',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: '2',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: '9',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }]
        }
      ])
    })
})

test('_GET over all fields', t => {
  t.plan(1)
  const sort = (a, b) => (a._id < b._id ? -1 : a._id > b._id ? 1 : 0)
  global[indexName]
    ._GET({
      VALUE: 'volvo'
    })
    .then(res => {
      t.deepEqual(res.sort(sort), [
        {
          _id: '0',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'volvo', SCORE: '1.00' }
          ]
        },
        {
          _id: '1',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'volvo', SCORE: '1.00' }
          ]
        },
        {
          _id: '2',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: '3',
          _match: [{ FIELD: 'manufacturer', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: '4',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'manufacturer', VALUE: 'volvo', SCORE: '1.00' }
          ]
        },
        {
          _id: '5',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }
          ]
        },
        {
          _id: '8',
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: '9',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }]
        }
      ])
    })
})

test('simple _GET', t => {
  t.plan(1)
  global[indexName]
    ._GET({
      FIELD: 'make',
      VALUE: {
        GTE: 'a',
        LTE: 'c'
      }
    })
    .then(res => {
      t.deepEqual(res, [
        { _id: '1', _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }] },
        { _id: '7', _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }] },
        { _id: '9', _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }] }
      ])
    })
})

test('simple QUERY using json with QUERY', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      GET: {
        FIELD: 'make',
        VALUE: {
          GTE: 'a',
          LTE: 'c'
        }
      }
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '1',
            _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
          },
          {
            _id: '7',
            _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }]
          },
          { _id: '9', _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }] }
        ],
        RESULT_LENGTH: 3
      })
    })
})

test('QUERY by specifying a FIELD but no VALUE', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        GET: {
          FIELD: 'extrafield'
        }
      },
      {
        SCORE: {
          TYPE: 'SUM'
        },
        SORT: true
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '0',
            _match: [
              { FIELD: 'extrafield', VALUE: 'extra', SCORE: '1.00' },
              { FIELD: 'extrafield', VALUE: 'field', SCORE: '1.00' },
              { FIELD: 'extrafield', VALUE: 'w00t', SCORE: '1.00' }
            ],
            _score: 3
          },
          {
            _id: '3',
            _match: [
              { FIELD: 'extrafield', VALUE: 'extra', SCORE: '1.00' },
              { FIELD: 'extrafield', VALUE: 'field', SCORE: '1.00' },
              { FIELD: 'extrafield', VALUE: 'w00t', SCORE: '1.00' }
            ],
            _score: 3
          },
          {
            _id: '6',
            _match: [
              { FIELD: 'extrafield', VALUE: 'extra', SCORE: '1.00' },
              { FIELD: 'extrafield', VALUE: 'field', SCORE: '1.00' },
              { FIELD: 'extrafield', VALUE: 'w00t', SCORE: '1.00' }
            ],
            _score: 3
          }
        ],
        RESULT_LENGTH: 3
      })
    })
})

test('create a search index with query and index side character normalisation', async t => {
  const data = [{ text: 'jeg bør finne en ting' }, { text: 'jeg bor i Oslo' }]
  const { PUT, _GET, INDEX, TOKENIZATION_PIPELINE_STAGES } = await si({
    name: sandbox + 'GET-2'
  })
  const ids = (
    await PUT(data, {
      tokenizer: (tokens, field, ops) =>
        TOKENIZATION_PIPELINE_STAGES.SPLIT([tokens, field, ops])
          .then(TOKENIZATION_PIPELINE_STAGES.LOWCASE)
          .then(([tokens, field, ops]) =>
            Promise.resolve([tokens.map(t => t.replace(/ø/g, 'o'))])
          )
          .then(TOKENIZATION_PIPELINE_STAGES.SCORE_TERM_FREQUENCY)
          .then(([tokens, field, ops]) => tokens)
    })
  ).map(status => status._id)

  t.deepEquals(await INDEX.STORE.get(['IDX', 'text', ['bor', '1.00']], INDEX.LEVEL_OPTIONS), ids)
  try {
    await INDEX.STORE.get(['IDX', 'text', ['bør', '1.00']], INDEX.LEVEL_OPTIONS)
    t.fail('that key should not be in the database')
  } catch (e) {
    t.ok(e instanceof Error)
  }

  const swapØtoO = token =>
    new Promise(resolve => {
      token.VALUE.GTE = token.VALUE.GTE.replace(/ø/g, 'o')
      token.VALUE.LTE = token.VALUE.LTE.replace(/ø/g, 'o')
      return resolve(token)
    })

  t.deepEquals(await _GET('bør', swapØtoO), [
    {
      _id: ids[0],
      _match: [{ FIELD: 'text', VALUE: 'bor', SCORE: '1.00' }]
    },
    {
      _id: ids[1],
      _match: [{ FIELD: 'text', VALUE: 'bor', SCORE: '1.00' }]
    }
  ])
})

test('create a search index with query and index side character normalisation (QUERY GET)', async t => {
  const data = ['jeg bør finne en ting', 'jeg bor i Oslo']
  const { PUT, QUERY, INDEX, TOKENIZATION_PIPELINE_STAGES } = await si({
    name: sandbox + 'GET-3'
  })
  const ids = (
    await PUT(data, {
      tokenizer: (tokens, field, ops) =>
        TOKENIZATION_PIPELINE_STAGES.SPLIT([tokens, field, ops])
          .then(TOKENIZATION_PIPELINE_STAGES.LOWCASE)
          .then(([tokens, field, ops]) =>
            Promise.resolve([tokens.map(t => t.replace(/ø/g, 'o'))])
          )
          .then(TOKENIZATION_PIPELINE_STAGES.SCORE_TERM_FREQUENCY)
          .then(([tokens, field, ops]) => tokens)
    })
  ).map(status => status._id)

  t.deepEquals(await INDEX.STORE.get(['IDX', 'body', ['bor', '1.00']], INDEX.LEVEL_OPTIONS), ids)
  try {
    await INDEX.STORE.get(['IDX', 'body', ['bør', '1.00']], INDEX.LEVEL_OPTIONS)
    t.fail('that key should not be in the database')
  } catch (e) {
    t.ok(e instanceof Error)
  }

  const swapØtoO = token =>
    new Promise(resolve => {
      token.VALUE.GTE = token.VALUE.GTE.replace(/ø/g, 'o')
      token.VALUE.LTE = token.VALUE.LTE.replace(/ø/g, 'o')
      return resolve(token)
    })

  t.deepEquals(
    await QUERY(
      { GET: 'bør' },
      {
        PIPELINE: swapØtoO
      }
    ),
    {
      RESULT: [
        {
          _id: ids[0],
          _match: [{ FIELD: 'body', VALUE: 'bor', SCORE: '1.00' }]
        },
        {
          _id: ids[1],
          _match: [{ FIELD: 'body', VALUE: 'bor', SCORE: '1.00' }]
        }
      ],
      RESULT_LENGTH: 2
    }
  )
})

// TODO: an example of QUERY that shows more than one hit in _match
