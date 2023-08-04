import test from 'tape'
import { EntryStream } from 'level-read-stream'

const { SearchIndex } = await import(
  '../../src/' + process.env.SI_TEST_ENTRYPOINT
)

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'PUT_RAW_DOCUMENTS'

const carData = [
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
  }
]

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
  t.plan(1)
  global[indexName].PUT(carData).then(response =>
    t.deepEquals(response, [
      { _id: 0, status: 'CREATED', operation: 'PUT' },
      { _id: 1, status: 'CREATED', operation: 'PUT' },
      { _id: 2, status: 'CREATED', operation: 'PUT' }
    ])
  )
})

test('DOC_RAWs are inserted as expected', t => {
  const indexEntries = [
    {
      key: ['DOC_RAW', 0],
      value: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', 1],
      value: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', 2],
      value: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo' }
    }
  ]
  t.plan(indexEntries.length)
  new EntryStream(global[indexName].INDEX.STORE, {
    gte: ['DOC_RAW', null],
    lte: ['DOC_RAW', undefined],
    ...global[indexName].INDEX.LEVEL_OPTIONS
  }).on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})

test('can add raw document', t => {
  t.plan(1)
  global[indexName]
    .PUT_RAW(
      [
        {
          text: 'this is an altered raw doc'
        }
      ],
      [1]
    )
    .then(response =>
      t.deepEquals(response, [{ _id: 1, status: 'OK', operation: '_PUT_RAW' }])
    )
})

test('Verify that PUT_RAW has updated the raw document', t => {
  const indexEntries = [
    {
      key: ['DOC_RAW', 0],
      value: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', 1],
      value: { text: 'this is an altered raw doc' }
    },
    {
      key: ['DOC_RAW', 2],
      value: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo' }
    }
  ]
  t.plan(indexEntries.length)
  new EntryStream(global[indexName].INDEX.STORE, {
    gte: ['DOC_RAW', null],
    lte: ['DOC_RAW', undefined],
    ...global[indexName].INDEX.LEVEL_OPTIONS
  }).on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})

test('can add raw document with external API (no underscore)', t => {
  t.plan(1)
  global[indexName]
    .PUT_RAW(
      [
        {
          text: 'this is an altered raw doc for 2'
        }
      ],
      [2]
    )
    .then(response =>
      t.deepEquals(response, [{ _id: 2, status: 'OK', operation: '_PUT_RAW' }])
    )
})

test('Verify that PUT_RAW has created an appropriate index', t => {
  const indexEntries = [
    {
      key: ['DOC_RAW', 0],
      value: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', 1],
      value: { text: 'this is an altered raw doc' }
    },
    {
      key: ['DOC_RAW', 2],
      value: { text: 'this is an altered raw doc for 2' }
    }
  ]
  t.plan(indexEntries.length)
  new EntryStream(global[indexName].INDEX.STORE, {
    gte: ['DOC_RAW', null],
    lte: ['DOC_RAW', undefined],
    ...global[indexName].INDEX.LEVEL_OPTIONS
  }).on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})
