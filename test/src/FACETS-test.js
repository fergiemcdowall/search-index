import test from 'tape'

const { SearchIndex } = await import(
  '../../src/' + process.env.SI_TEST_ENTRYPOINT
)

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'FACETS'
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
      colour: 'yellow'
    },
    {
      _id: '1',
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      colour: 'red'
    },
    {
      _id: '2',
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      colour: 'blue'
    },
    {
      _id: '3',
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW',
      colour: 'red'
    },
    {
      _id: '4',
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      colour: 'red'
    },
    {
      _id: '5',
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      colour: 'blue'
    },
    {
      _id: '6',
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW',
      colour: 'yellow'
    },
    {
      _id: '7',
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla',
      colour: 'yellow'
    },
    {
      _id: '8',
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla',
      colour: 'blue'
    },
    {
      _id: '9',
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      colour: 'red'
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('simple FACETS', t => {
  const { FACETS } = global[indexName]
  t.plan(1)
  FACETS({
    FIELD: 'colour'
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'colour', VALUE: 'blue', _id: ['2', '5', '8'] },
      { FIELD: 'colour', VALUE: 'red', _id: ['1', '3', '4', '9'] },
      { FIELD: 'colour', VALUE: 'yellow', _id: ['0', '6', '7'] }
    ])
  })
})

test('simple FACETS with range', t => {
  const { FACETS } = global[indexName]
  t.plan(1)
  FACETS({
    FIELD: 'colour',
    VALUE: {
      GTE: 'a',
      LTE: 'c'
    }
  }).then(res => {
    t.deepEqual(res, [{ FIELD: 'colour', VALUE: 'blue', _id: ['2', '5', '8'] }])
  })
})

test('simple FACETS with range', t => {
  const { FACETS } = global[indexName]
  t.plan(1)
  FACETS({
    FIELD: 'colour',
    VALUE: {
      GTE: 'c'
    }
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'colour', VALUE: 'red', _id: ['1', '3', '4', '9'] },
      { FIELD: 'colour', VALUE: 'yellow', _id: ['0', '6', '7'] }
    ])
  })
})

test('simple FACETS with range', t => {
  const { FACETS } = global[indexName]
  t.plan(1)
  FACETS({
    FIELD: 'colour',
    VALUE: {
      LTE: 'c'
    }
  }).then(res => {
    t.deepEqual(res, [{ FIELD: 'colour', VALUE: 'blue', _id: ['2', '5', '8'] }])
  })
})

test('simple FACETS', t => {
  const { FACETS } = global[indexName]
  t.plan(1)
  FACETS({
    FIELD: 'colour'
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'colour', VALUE: 'blue', _id: ['2', '5', '8'] },
      { FIELD: 'colour', VALUE: 'red', _id: ['1', '3', '4', '9'] },
      { FIELD: 'colour', VALUE: 'yellow', _id: ['0', '6', '7'] }
    ])
  })
})

test('simple FACETS', t => {
  const { FACETS } = global[indexName]
  t.plan(1)
  FACETS({
    VALUE: 'red'
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'colour', VALUE: 'red', _id: ['1', '3', '4', '9'] }
    ])
  })
})

test('simple FACETS', t => {
  const { FACETS } = global[indexName]
  t.plan(1)
  FACETS({
    VALUE: 'volvo'
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'brand', VALUE: 'volvo', _id: ['0', '1', '2', '4', '5', '9'] },
      { FIELD: 'make', VALUE: 'volvo', _id: ['4', '5', '8'] },
      { FIELD: 'manufacturer', VALUE: 'volvo', _id: ['0', '1', '3', '4'] }
    ])
  })
})

test('simple FACETS', t => {
  const { FACETS } = global[indexName]
  t.plan(1)
  FACETS({
    FIELD: 'brand'
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'brand', VALUE: 'bmw', _id: ['3', '6'] },
      { FIELD: 'brand', VALUE: 'tesla', _id: ['7', '8'] },
      { FIELD: 'brand', VALUE: 'volvo', _id: ['0', '1', '2', '4', '5', '9'] }
    ])
  })
})

test('FACETS on 2 fields', t => {
  const { FACETS } = global[indexName]
  t.plan(1)
  FACETS({
    FIELD: ['brand', 'colour']
  }).then(res => {
    t.deepEqual(res, [
      { FIELD: 'brand', VALUE: 'bmw', _id: ['3', '6'] },
      { FIELD: 'brand', VALUE: 'tesla', _id: ['7', '8'] },
      { FIELD: 'brand', VALUE: 'volvo', _id: ['0', '1', '2', '4', '5', '9'] },
      { FIELD: 'colour', VALUE: 'blue', _id: ['2', '5', '8'] },
      { FIELD: 'colour', VALUE: 'red', _id: ['1', '3', '4', '9'] },
      { FIELD: 'colour', VALUE: 'yellow', _id: ['0', '6', '7'] }
    ])
  })
})

test('FACETS on 2 fields with GTE/LTE', t => {
  const { FACETS } = global[indexName]
  t.plan(1)
  FACETS(
    {
      FIELD: ['brand'],
      VALUE: {
        GTE: 'f'
      }
    },
    {
      FIELD: ['colour'],
      VALUE: {
        LTE: 'x'
      }
    }
  ).then(res => {
    t.deepEqual(res, [
      { FIELD: 'brand', VALUE: 'tesla', _id: ['7', '8'] },
      { FIELD: 'brand', VALUE: 'volvo', _id: ['0', '1', '2', '4', '5', '9'] },
      { FIELD: 'colour', VALUE: 'blue', _id: ['2', '5', '8'] },
      { FIELD: 'colour', VALUE: 'red', _id: ['1', '3', '4', '9'] }
    ])
  })
})
