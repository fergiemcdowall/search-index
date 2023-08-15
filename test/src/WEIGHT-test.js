import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'

const docs = [
  {
    _id: 0,
    make: 'Tesla',
    manufacturer: 'Volvo',
    brand: 'Volvo',
    colour: 'yellow'
  },
  {
    _id: 1,
    make: 'BMW',
    manufacturer: 'Volvo',
    brand: 'Volvo',
    colour: 'red'
  },
  {
    _id: 2,
    make: 'Tesla',
    manufacturer: 'Tesla',
    brand: 'Volvo',
    colour: 'blue'
  },
  {
    _id: 3,
    make: 'Tesla',
    manufacturer: 'Volvo',
    brand: 'BMW',
    colour: 'red'
  },
  {
    _id: 4,
    make: 'Volvo',
    manufacturer: 'Volvo',
    brand: 'Volvo',
    colour: 'red'
  },
  {
    _id: 5,
    make: 'Volvo',
    manufacturer: 'Tesla',
    brand: 'Volvo',
    colour: 'blue'
  },
  {
    _id: 6,
    make: 'Tesla',
    manufacturer: 'Tesla',
    brand: 'BMW',
    colour: 'yellow'
  },
  {
    _id: 7,
    make: 'BMW',
    manufacturer: 'Tesla',
    brand: 'Tesla',
    colour: 'yellow'
  },
  {
    _id: 8,
    make: 'Volvo',
    manufacturer: 'BMW',
    brand: 'Tesla',
    colour: 'blue'
  },
  {
    _id: 9,
    make: 'BMW',
    manufacturer: 'Tesla',
    brand: 'Volvo',
    colour: 'red'
  }
]

test('create a search index to test WEIGHT', async function (t) {
  t.plan(7)

  const { PUT, QUERY, SEARCH } = await new SearchIndex({
    name: sandbox + 'WEIGHT'
  })
  t.ok(PUT)

  const indexingResult = await PUT(docs)
  t.deepEquals([...new Set(indexingResult.map(ir => ir.status))], ['CREATED'])

  t.deepEquals(
    await QUERY(
      {
        AND: ['bmw']
      },
      {
        SCORE: 'TFIDF',
        WEIGHT: [
          {
            FIELD: 'make',
            WEIGHT: 2
          }
        ],
        SORT: true
      }
    ),
    {
      RESULT: [
        {
          _id: 1,
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '2.00' }],
          _score: 1.21
        },
        {
          _id: 7,
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '2.00' }],
          _score: 1.21
        },
        {
          _id: 9,
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '2.00' }],
          _score: 1.21
        },
        {
          _id: 3,
          _match: [{ FIELD: 'brand', VALUE: 'bmw', SCORE: '1.00' }],
          _score: 0.61
        },
        {
          _id: 6,
          _match: [{ FIELD: 'brand', VALUE: 'bmw', SCORE: '1.00' }],
          _score: 0.61
        },
        {
          _id: 8,
          _match: [{ FIELD: 'manufacturer', VALUE: 'bmw', SCORE: '1.00' }],
          _score: 0.61
        }
      ],
      RESULT_LENGTH: 6
    }
  )

  // as above but with SEARCH
  t.deepEquals(
    await SEARCH(['bmw'], {
      WEIGHT: [
        {
          FIELD: 'make',
          WEIGHT: 2
        }
      ]
    }),
    {
      RESULT: [
        {
          _id: 1,
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '2.00' }],
          _score: 1.21
        },
        {
          _id: 7,
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '2.00' }],
          _score: 1.21
        },
        {
          _id: 9,
          _match: [{ FIELD: 'make', VALUE: 'bmw', SCORE: '2.00' }],
          _score: 1.21
        },
        {
          _id: 3,
          _match: [{ FIELD: 'brand', VALUE: 'bmw', SCORE: '1.00' }],
          _score: 0.61
        },
        {
          _id: 6,
          _match: [{ FIELD: 'brand', VALUE: 'bmw', SCORE: '1.00' }],
          _score: 0.61
        },
        {
          _id: 8,
          _match: [{ FIELD: 'manufacturer', VALUE: 'bmw', SCORE: '1.00' }],
          _score: 0.61
        }
      ],
      RESULT_LENGTH: 6
    }
  )

  t.deepEquals(
    await QUERY(
      {
        AND: [
          'red',
          {
            OR: ['make:bmw', 'make:volvo', 'make:tesla']
          }
        ]
      },
      {
        SCORE: 'SUM',
        WEIGHT: [
          {
            FIELD: 'make',
            VALUE: 'bmw',
            WEIGHT: 2
          }
        ],
        SORT: true
      }
    ),
    {
      RESULT: [
        {
          _id: 1,
          _match: [
            { FIELD: 'colour', VALUE: 'red', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'bmw', SCORE: '2.00' }
          ],
          _score: 3.03
        },
        {
          _id: 9,
          _match: [
            { FIELD: 'colour', VALUE: 'red', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'bmw', SCORE: '2.00' }
          ],
          _score: 3.03
        },
        {
          _id: 3,
          _match: [
            { FIELD: 'colour', VALUE: 'red', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'tesla', SCORE: '1.00' }
          ],
          _score: 2.02
        },
        {
          _id: 4,
          _match: [
            { FIELD: 'colour', VALUE: 'red', SCORE: '1.00' },
            { FIELD: 'make', VALUE: 'volvo', SCORE: '1.00' }
          ],
          _score: 2.02
        }
      ],
      RESULT_LENGTH: 4
    }
  )

  t.deepEquals(
    await QUERY(
      {
        OR: ['colour:red', 'brand:tesla']
      },
      {
        SCORE: {
          TYPE: 'PRODUCT'
        },
        WEIGHT: [
          {
            VALUE: 'red',
            WEIGHT: 3
          }
        ],
        SORT: true
      }
    ),
    {
      RESULT: [
        {
          _id: 1,
          _match: [{ FIELD: 'colour', VALUE: 'red', SCORE: '3.00' }],
          _score: 3
        },
        {
          _id: 3,
          _match: [{ FIELD: 'colour', VALUE: 'red', SCORE: '3.00' }],
          _score: 3
        },
        {
          _id: 4,
          _match: [{ FIELD: 'colour', VALUE: 'red', SCORE: '3.00' }],
          _score: 3
        },
        {
          _id: 9,
          _match: [{ FIELD: 'colour', VALUE: 'red', SCORE: '3.00' }],
          _score: 3
        },
        {
          _id: 7,
          _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 1
        },
        {
          _id: 8,
          _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '1.00' }],
          _score: 1
        }
      ],
      RESULT_LENGTH: 6
    }
  )

  t.deepEquals(
    await QUERY(
      {
        OR: ['colour:red', 'brand:tesla']
      },
      {
        SCORE: {
          TYPE: 'PRODUCT'
        },
        WEIGHT: [
          {
            VALUE: 'red',
            WEIGHT: 3
          },
          {
            VALUE: 'tesla',
            WEIGHT: 0.2
          }
        ],
        SORT: true
      }
    ),
    {
      RESULT: [
        {
          _id: 1,
          _match: [{ FIELD: 'colour', VALUE: 'red', SCORE: '3.00' }],
          _score: 3
        },
        {
          _id: 3,
          _match: [{ FIELD: 'colour', VALUE: 'red', SCORE: '3.00' }],
          _score: 3
        },
        {
          _id: 4,
          _match: [{ FIELD: 'colour', VALUE: 'red', SCORE: '3.00' }],
          _score: 3
        },
        {
          _id: 9,
          _match: [{ FIELD: 'colour', VALUE: 'red', SCORE: '3.00' }],
          _score: 3
        },
        {
          _id: 7,
          _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '0.20' }],
          _score: 0.2
        },
        {
          _id: 8,
          _match: [{ FIELD: 'brand', VALUE: 'tesla', SCORE: '0.20' }],
          _score: 0.2
        }
      ],
      RESULT_LENGTH: 6
    }
  )
})
