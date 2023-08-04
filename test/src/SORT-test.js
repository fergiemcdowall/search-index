import test from 'tape'

const { SearchIndex } = await import(
  '../../src/' + process.env.SI_TEST_ENTRYPOINT
)

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_SORT'
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
      _id: '0',
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      price: 3000
    },
    {
      _id: '1',
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      price: 12000
    },
    {
      _id: '2',
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      price: 14000
    },
    {
      _id: '3',
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW',
      price: 140000
    },
    {
      _id: '4',
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      price: 1000
    },
    {
      _id: '5',
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      price: 2000
    },
    {
      _id: '6',
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW',
      price: 500
    },
    {
      _id: '7',
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla',
      price: 5000
    },
    {
      _id: '8',
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla',
      price: 100
    },
    {
      _id: '9',
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      price: 1100
    }
  ]
  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('SORT ALPHABETIC DESCENDING BY SCORE (default)', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        FIELD: 'brand'
      },
      {
        SCORE: {
          FIELDS: ['brand'],
          TYPE: 'VALUE'
        },
        SORT: {
          TYPE: 'ALPHABETIC',
          DIRECTION: 'DESCENDING'
        }
      }
    )
    .then(({ RESULT }) => {
      t.deepEqual(RESULT, [
        {
          _id: '0',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }],
          _score: 'volvo'
        },
        {
          _id: '1',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }],
          _score: 'volvo'
        },
        {
          _id: '2',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }],
          _score: 'volvo'
        },
        {
          _id: '4',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }],
          _score: 'volvo'
        },
        {
          _id: '5',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }],
          _score: 'volvo'
        },
        {
          _id: '9',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }],
          _score: 'volvo'
        },
        {
          _id: '7',
          _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 'tesla'
        },
        {
          _id: '8',
          _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 'tesla'
        },
        {
          _id: '3',
          _match: [{ FIELD: 'brand', VALUE: 'bmw', SCORE: '1.00' }],
          _score: 'bmw'
        },
        {
          _id: '6',
          _match: [{ FIELD: 'brand', VALUE: 'bmw', SCORE: '1.00' }],
          _score: 'bmw'
        }
      ])
    })
})

test('SORT ALPHABETIC ASCENDING', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        FIELD: 'brand'
      },
      {
        SCORE: {
          FIELDS: ['brand'],
          TYPE: 'VALUE'
        },
        SORT: {
          TYPE: 'ALPHABETIC',
          DIRECTION: 'ASCENDING'
        }
      }
    )
    .then(({ RESULT }) => {
      t.deepEqual(RESULT, [
        {
          _id: '3',
          _match: [{ FIELD: 'brand', VALUE: 'bmw', SCORE: '1.00' }],
          _score: 'bmw'
        },
        {
          _id: '6',
          _match: [{ FIELD: 'brand', VALUE: 'bmw', SCORE: '1.00' }],
          _score: 'bmw'
        },
        {
          _id: '7',
          _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 'tesla'
        },
        {
          _id: '8',
          _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 'tesla'
        },
        {
          _id: '0',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }],
          _score: 'volvo'
        },
        {
          _id: '1',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }],
          _score: 'volvo'
        },
        {
          _id: '2',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }],
          _score: 'volvo'
        },
        {
          _id: '4',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }],
          _score: 'volvo'
        },
        {
          _id: '5',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }],
          _score: 'volvo'
        },
        {
          _id: '9',
          _match: [{ FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' }],
          _score: 'volvo'
        }
      ])
    })
})

test('SORT NUMERIC ASCENDING', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        FIELD: 'price'
      },
      {
        SCORE: {
          FIELDS: ['price'],
          TYPE: 'VALUE'
        },
        SORT: {
          TYPE: 'NUMERIC',
          DIRECTION: 'ASCENDING'
        }
      }
    )
    .then(({ RESULT }) => {
      t.deepEqual(RESULT, [
        {
          _id: '8',
          _match: [{ FIELD: 'price', VALUE: 100, SCORE: 100 }],
          _score: '100'
        },
        {
          _id: '6',
          _match: [{ FIELD: 'price', VALUE: 500, SCORE: 500 }],
          _score: '500'
        },
        {
          _id: '4',
          _match: [{ FIELD: 'price', VALUE: 1000, SCORE: 1000 }],
          _score: '1000'
        },
        {
          _id: '9',
          _match: [{ FIELD: 'price', VALUE: 1100, SCORE: 1100 }],
          _score: '1100'
        },
        {
          _id: '5',
          _match: [{ FIELD: 'price', VALUE: 2000, SCORE: 2000 }],
          _score: '2000'
        },
        {
          _id: '0',
          _match: [{ FIELD: 'price', VALUE: 3000, SCORE: 3000 }],
          _score: '3000'
        },
        {
          _id: '7',
          _match: [{ FIELD: 'price', VALUE: 5000, SCORE: 5000 }],
          _score: '5000'
        },
        {
          _id: '1',
          _match: [{ FIELD: 'price', VALUE: 12000, SCORE: 12000 }],
          _score: '12000'
        },
        {
          _id: '2',
          _match: [{ FIELD: 'price', VALUE: 14000, SCORE: 14000 }],
          _score: '14000'
        },
        {
          _id: '3',
          _match: [{ FIELD: 'price', VALUE: 140000, SCORE: 140000 }],
          _score: '140000'
        }
      ])
    })
})

test('SORT on price when other fields are also selected', t => {
  t.plan(1)
  global[indexName]
    .SEARCH(
      [
        {
          FIELD: ['price', 'brand']
        }
      ],
      {
        SCORE: {
          FIELDS: ['price'],
          TYPE: 'VALUE'
        },
        SORT: {
          TYPE: 'NUMERIC',
          DIRECTION: 'DESCENDING'
        }
      }
    )
    .then(({ RESULT }) => {
      t.deepEqual(RESULT, [
        {
          _id: '3',
          _match: [
            { FIELD: 'brand', VALUE: 'bmw', SCORE: '1.00' },
            { FIELD: 'price', VALUE: 140000, SCORE: 140000 }
          ],
          _score: '140000'
        },
        {
          _id: '2',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'price', VALUE: 14000, SCORE: 14000 }
          ],
          _score: '14000'
        },
        {
          _id: '1',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'price', VALUE: 12000, SCORE: 12000 }
          ],
          _score: '12000'
        },
        {
          _id: '7',
          _match: [
            { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
            { FIELD: 'price', VALUE: 5000, SCORE: 5000 }
          ],
          _score: '5000'
        },
        {
          _id: '0',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'price', VALUE: 3000, SCORE: 3000 }
          ],
          _score: '3000'
        },
        {
          _id: '5',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'price', VALUE: 2000, SCORE: 2000 }
          ],
          _score: '2000'
        },
        {
          _id: '9',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'price', VALUE: 1100, SCORE: 1100 }
          ],
          _score: '1100'
        },
        {
          _id: '4',
          _match: [
            { FIELD: 'brand', VALUE: 'volvo', SCORE: '1.00' },
            { FIELD: 'price', VALUE: 1000, SCORE: 1000 }
          ],
          _score: '1000'
        },
        {
          _id: '6',
          _match: [
            { FIELD: 'brand', VALUE: 'bmw', SCORE: '1.00' },
            { FIELD: 'price', VALUE: 500, SCORE: 500 }
          ],
          _score: '500'
        },
        {
          _id: '8',
          _match: [
            { FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' },
            { FIELD: 'price', VALUE: 100, SCORE: 100 }
          ],
          _score: '100'
        }
      ])
    })
})
