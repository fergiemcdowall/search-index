import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_PAGE'
const global = {}

test('create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({ name: indexName })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add data', t => {
  const data = [
    {
      _id: 0,
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: 1,
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: 2,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    },
    {
      _id: 3,
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW'
    },
    {
      _id: 4,
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: 5,
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    },
    {
      _id: 6,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW'
    },
    {
      _id: 7,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla'
    },
    {
      _id: 8,
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla'
    },
    {
      _id: 9,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo'
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('get page 2 (called "1": count from "0") with page size of 3', t => {
  t.plan(1)
  global[indexName]
    .DOCUMENTS()
    .then(docs =>
      global[indexName]._PAGE(docs, {
        NUMBER: 1,
        SIZE: 3
      })
    )
    .then(res => {
      t.deepEqual(res, [
        {
          _id: 3,
          _doc: { _id: 3, make: 'Tesla', manufacturer: 'Volvo', brand: 'BMW' }
        },
        {
          _id: 4,
          _doc: {
            _id: 4,
            make: 'Volvo',
            manufacturer: 'Volvo',
            brand: 'Volvo'
          }
        },
        {
          _id: 5,
          _doc: {
            _id: 5,
            make: 'Volvo',
            manufacturer: 'Tesla',
            brand: 'Volvo'
          }
        }
      ])
    })
})

test('get all', t => {
  t.plan(1)
  global[indexName].QUERY({ FIELD: 'make' }).then(res => {
    t.deepEqual(res, {
      RESULT: [
        { _id: 1, _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }] },
        { _id: 7, _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }] },
        { _id: 9, _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '1.00' }] },
        {
          _id: 0,
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
        },
        {
          _id: 2,
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
        },
        {
          _id: 3,
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
        },
        {
          _id: 6,
          _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
        },
        {
          _id: 4,
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: 5,
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        },
        {
          _id: 8,
          _match: [{ FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }]
        }
      ],
      RESULT_LENGTH: 10,
      PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
    })
  })
})

test('get page 2 (called "1": count from "0") with page size of 3 (JSON)', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      { FIELD: 'make' },
      {
        PAGE: {
          NUMBER: 1,
          SIZE: 3
        }
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: 0,
            _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
          },
          {
            _id: 2,
            _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
          },
          {
            _id: 3,
            _match: [{ FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }]
          }
        ],
        RESULT_LENGTH: 10,
        PAGING: { NUMBER: 1, SIZE: 3, TOTAL: 4, DOC_OFFSET: 3 }
      })
    })
})

test('get last page with page size of 4', t => {
  t.plan(1)
  global[indexName]
    .DOCUMENTS()
    .then(docs =>
      global[indexName]._PAGE(docs, {
        NUMBER: -1,
        SIZE: 4
      })
    )
    .then(res => {
      t.deepEqual(res, [
        {
          _id: 6,
          _doc: { _id: 6, make: 'Tesla', manufacturer: 'Tesla', brand: 'BMW' }
        },
        {
          _id: 7,
          _doc: { _id: 7, make: 'BMW', manufacturer: 'Tesla', brand: 'Tesla' }
        },
        {
          _id: 8,
          _doc: { _id: 8, make: 'Volvo', manufacturer: 'BMW', brand: 'Tesla' }
        },
        {
          _id: 9,
          _doc: { _id: 9, make: 'BMW', manufacturer: 'Tesla', brand: 'Volvo' }
        }
      ])
    })
})
