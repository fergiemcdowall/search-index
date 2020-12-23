const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_AGGREGATE'

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

test('simple aggregation', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    AND: ['make:bmw']
  }, {
    BUCKETS: [{
      FIELD: 'make',
      VALUE: {
        GTE: 'a',
        LTE: 'u'
      }
    }]
  }).then(res => {
    t.deepEqual(res, {
      BUCKETS: [
        {
          FIELD: ['make'],
          VALUE: { GTE: 'a', LTE: 'u' },
          _id: [
            '1', '7', '9'
          ]
        }
      ],
      RESULT_LENGTH: 3,
      RESULT: [
        { _id: '1', _match: ['make:bmw#1.00'] },
        { _id: '7', _match: ['make:bmw#1.00'] },
        { _id: '9', _match: ['make:bmw#1.00'] }
      ]
    })
  })
})

test('simple AGGREGATE (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    AND: ['make:bmw', 'manufacturer:tesla']
  }, {
    BUCKETS: [{
      FIELD: 'make',
      VALUE: {
        GTE: 'a',
        LTE: 'u'
      }
    }]
  }).then(res => {
    t.deepEqual(res, {
      BUCKETS: [
        {
          FIELD: ['make'],
          VALUE: { GTE: 'a', LTE: 'u' },
          _id: [
            '7', '9'
          ]
        }
      ],
      RESULT: [
        {
          _id: '7',
          _match: [
            'make:bmw#1.00', 'manufacturer:tesla#1.00']
        },
        {
          _id: '9',
          _match: [
            'make:bmw#1.00', 'manufacturer:tesla#1.00']
        }
      ],
      RESULT_LENGTH: 2
    })
  })
})

test('simple AGGREGATE (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    OR: ['brand:tesla', 'manufacturer:tesla']
  }, {
    BUCKETS: [{
      FIELD: 'make',
      VALUE: {
        GTE: 'a',
        LTE: 'u'
      }
    }]
  }).then(res => {
    t.deepEqual(res, {
      RESULT: [
        { _id: '2', _match: ['manufacturer:tesla#1.00'] },
        { _id: '5', _match: ['manufacturer:tesla#1.00'] },
        { _id: '6', _match: ['manufacturer:tesla#1.00'] },
        { _id: '7', _match: ['brand:tesla#1.00', 'manufacturer:tesla#1.00'] },
        { _id: '8', _match: ['brand:tesla#1.00'] },
        { _id: '9', _match: ['manufacturer:tesla#1.00'] }
      ],
      BUCKETS: [
        { FIELD: ['make'], VALUE: { GTE: 'a', LTE: 'u' }, _id: ['2', '6', '7', '9'] }],
      RESULT_LENGTH: 6
    })
  })
})

test('simple aggregation', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    GET: 'make:bmw'
  }, {
    FACETS: [{
      FIELD: 'make',
      VALUE: {
        GTE: 'a',
        LTE: 'u'
      }
    }]
  }).then(res => {
    t.deepEqual(res, {
      RESULT: [
        { _id: '1', _match: ['make:bmw#1.00'] },
        { _id: '7', _match: ['make:bmw#1.00'] },
        { _id: '9', _match: ['make:bmw#1.00'] }
      ],
      RESULT_LENGTH: 3,
      FACETS: [
        { FIELD: 'make', VALUE: 'bmw', _id: ['1', '7', '9'] },
        { FIELD: 'make', VALUE: 'tesla', _id: [] }
      ]

    })
  })
})

test('simple AGGREGATE, using DISTINCT (JSON)', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    GET: {
      FIELD: ['make'],
      VALUE: {
        GTE: 'a',
        LTE: 'c'
      }
    }
  }, {
    FACETS: [{
      FIELD: 'make',
      VALUE: {
        GTE: 'a',
        LTE: 'u'
      }
    }]
  }).then(res => {
    t.deepEqual(res, {
      FACETS: [
        { FIELD: 'make', VALUE: 'bmw', _id: ['1', '7', '9'] },
        { FIELD: 'make', VALUE: 'tesla', _id: [] }
      ],
      RESULT: [
        { _id: '1', _match: ['make:bmw#1.00'] },
        { _id: '7', _match: ['make:bmw#1.00'] },
        { _id: '9', _match: ['make:bmw#1.00'] }
      ],
      RESULT_LENGTH: 3
    })
  })
})

test('simple aggregation', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    GET: {
      FIELD: 'make',
      VALUE: 'bmw'
    }
  }, {
    FACETS: [{
      FIELD: 'make',
      VALUE: {
        GTE: 'a',
        LTE: 'u'
      }
    }]
  }).then(res => {
    t.deepEqual(res, {
      FACETS: [
        { FIELD: 'make', VALUE: 'bmw', _id: ['1', '7', '9'] },
        { FIELD: 'make', VALUE: 'tesla', _id: [] }
      ],
      RESULT: [
        { _id: '1', _match: ['make:bmw#1.00'] },
        { _id: '7', _match: ['make:bmw#1.00'] },
        { _id: '9', _match: ['make:bmw#1.00'] }
      ],
      RESULT_LENGTH: 3
    })
  })
})

test('simple aggregation, return full documents', t => {
  const { QUERY } = global[indexName]
  t.plan(1)
  QUERY({
    GET: {
      FIELD: 'make',
      VALUE: 'bmw'
    }
  }, {
    DOCUMENTS: true,
    FACETS: [{
      FIELD: 'make',
      VALUE: {
        GTE: 'a',
        LTE: 'u'
      }
    }]
  }).then(res => {
    t.deepEqual(res, {
      FACETS: [
        { FIELD: 'make', VALUE: 'bmw', _id: ['1', '7', '9'] },
        { FIELD: 'make', VALUE: 'tesla', _id: [] }
      ],
      RESULT: [
        {
          _id: '1',
          _match: ['make:bmw#1.00'],
          _doc: {
            _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo'
          }
        },
        {
          _id: '7',
          _match: ['make:bmw#1.00'],
          _doc: {
            _id: 7,
            make: 'BMW',
            manufacturer: 'Tesla',
            brand: 'Tesla'
          }
        },
        {
          _id: '9',
          _match: ['make:bmw#1.00'],
          _doc: {
            _id: 9, make: 'BMW', manufacturer: 'Tesla', brand: 'Volvo'
          }
        }
      ],
      RESULT_LENGTH: 3
    })
  })
})
