const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'nulltest'

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
      brand: null,
      extraField: 'EXTRA FIELD- w00t!'
    },
    {
      _id: '1',
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: [null]
    },
    {
      _id: '2',
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('get all docs with null values with DOCUMENT', t => {
  t.plan(1)
  const { DOCUMENTS } = global[indexName]
  DOCUMENTS().then(res => {
    t.deepEqual(res, [
      {
        _id: '0',
        _doc: {
          _id: '0',
          make: 'Tesla',
          manufacturer: 'Volvo',
          brand: null,
          extraField: 'EXTRA FIELD- w00t!'
        }
      },
      {
        _id: '1',
        _doc: { _id: '1', make: 'BMW', manufacturer: 'Volvo', brand: [null] }
      },
      {
        _id: '2',
        _doc: { _id: '2', make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo' }
      }
    ])
  })
})

test('GET all docs with null values', t => {
  t.plan(1)
  const { QUERY } = global[indexName]
  QUERY({
    AND: [
      {
        FIELD: 'brand'
      }
    ]
  }).then(res => {
    t.deepEqual(res, {
      RESULT: [
        { _id: '0', _match: [{ FIELD: 'brand', VALUE: null, SCORE: '1.00' }] },
        { _id: '1', _match: [{ FIELD: 'brand', VALUE: null, SCORE: '1.00' }] },
        {
          _id: '2',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }]
        }
      ],
      RESULT_LENGTH: 3
    })
  })
})
