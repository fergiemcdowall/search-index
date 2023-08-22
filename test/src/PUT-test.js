import test from 'tape'
import { EntryStream } from 'level-read-stream'
import { SearchIndex } from 'search-index'
import { packageVersion } from '../../src/version.js'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'PUT'
const global = {}
const carData = [
  {
    _id: '0',
    make: 'Tesla',
    manufacturer: 'Volvo',
    brand: 'Volvo'
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
  }
]

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
      { _id: '0', status: 'CREATED', operation: 'PUT' },
      { _id: '1', status: 'CREATED', operation: 'PUT' },
      { _id: '2', status: 'CREATED', operation: 'PUT' }
    ])
  )
})

test('Verify that PUT has created an appropriate index (_PUT_1)', t => {
  const indexEntries = [
    {
      key: ['CREATED_WITH'],
      value: 'search-index@' + packageVersion
    },
    {
      key: ['DOC', '0'],
      value: {
        _id: '0',
        make: [['tesla', '1.00']],
        manufacturer: [['volvo', '1.00']],
        brand: [['volvo', '1.00']]
      }
    },
    {
      key: ['DOC', '1'],
      value: {
        _id: '1',
        make: [['bmw', '1.00']],
        manufacturer: [['volvo', '1.00']],
        brand: [['volvo', '1.00']]
      }
    },
    {
      key: ['DOC', '2'],
      value: {
        _id: '2',
        make: [['tesla', '1.00']],
        manufacturer: [['tesla', '1.00']],
        brand: [['volvo', '1.00']]
      }
    },
    { key: ['DOCUMENT_COUNT'], value: 3 },
    {
      key: ['DOC_RAW', '0'],
      value: { _id: '0', make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', '1'],
      value: { _id: '1', make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', '2'],
      value: { _id: '2', make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo' }
    },
    { key: ['FIELD', 'brand'], value: 'brand' },
    { key: ['FIELD', 'make'], value: 'make' },
    { key: ['FIELD', 'manufacturer'], value: 'manufacturer' },
    {
      key: ['IDX', 'brand', ['volvo', '1.00']],
      value: ['0', '1', '2']
    },
    { key: ['IDX', 'make', ['bmw', '1.00']], value: ['1'] },
    { key: ['IDX', 'make', ['tesla', '1.00']], value: ['0', '2'] },
    { key: ['IDX', 'manufacturer', ['tesla', '1.00']], value: ['2'] },
    {
      key: ['IDX', 'manufacturer', ['volvo', '1.00']],
      value: ['0', '1']
    }
  ]
  t.plan(indexEntries.length)
  new EntryStream(global[indexName].INDEX.STORE, {
    lt: ['~']
  }).on('data', d => {
    // console.log(d)
    t.deepEquals(d, indexEntries.shift())
  })
})

const autoGeneratedIds = []

test('can add data', t => {
  const data = [
    'this is a really interesting document',
    'this is a document about bananas',
    'This document is mostly oranges oranges oranges, not bananas.'
  ]
  t.plan(9)
  global[indexName].PUT(data).then(response =>
    response.forEach(item => {
      t.equals(item.operation, 'PUT')
      t.equals(item.status, 'CREATED')
      // remove this because its now ok to have numbers as _ids
      t.match(item._id, /\d{13}-\d/gm, 'id has correct format')
      autoGeneratedIds.push(item._id)
    })
  )
})

test('Verify that PUT has created an appropriate index (_PUT_1 again)', t => {
  const indexEntries = [
    {
      key: ['CREATED_WITH'],
      value: 'search-index@' + packageVersion
    },
    {
      key: ['DOC', '0'],
      value: {
        _id: '0',
        make: [['tesla', '1.00']],
        manufacturer: [['volvo', '1.00']],
        brand: [['volvo', '1.00']]
      }
    },
    {
      key: ['DOC', '1'],
      value: {
        _id: '1',
        make: [['bmw', '1.00']],
        manufacturer: [['volvo', '1.00']],
        brand: [['volvo', '1.00']]
      }
    },
    {
      key: ['DOC', autoGeneratedIds[0]],
      value: {
        body: [
          ['a', '1.00'],
          ['document', '1.00'],
          ['interesting', '1.00'],
          ['is', '1.00'],
          ['really', '1.00'],
          ['this', '1.00']
        ],
        _id: autoGeneratedIds[0]
      }
    },
    {
      key: ['DOC', autoGeneratedIds[1]],
      value: {
        body: [
          ['a', '1.00'],
          ['about', '1.00'],
          ['bananas', '1.00'],
          ['document', '1.00'],
          ['is', '1.00'],
          ['this', '1.00']
        ],
        _id: autoGeneratedIds[1]
      }
    },
    {
      key: ['DOC', autoGeneratedIds[2]],
      value: {
        body: [
          ['bananas', '0.33'],
          ['document', '0.33'],
          ['is', '0.33'],
          ['mostly', '0.33'],
          ['not', '0.33'],
          ['oranges', '1.00'],
          ['this', '0.33']
        ],
        _id: autoGeneratedIds[2]
      }
    },
    {
      key: ['DOC', '2'],
      value: {
        _id: '2',
        make: [['tesla', '1.00']],
        manufacturer: [['tesla', '1.00']],
        brand: [['volvo', '1.00']]
      }
    },
    { key: ['DOCUMENT_COUNT'], value: 6 },
    {
      key: ['DOC_RAW', '0'],
      value: { _id: '0', make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', '1'],
      value: { _id: '1', make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', autoGeneratedIds[0]],
      value: 'this is a really interesting document'
    },
    {
      key: ['DOC_RAW', autoGeneratedIds[1]],
      value: 'this is a document about bananas'
    },
    {
      key: ['DOC_RAW', autoGeneratedIds[2]],
      value: 'This document is mostly oranges oranges oranges, not bananas.'
    },
    {
      key: ['DOC_RAW', '2'],
      value: { _id: '2', make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo' }
    },
    { key: ['FIELD', 'body'], value: 'body' },
    { key: ['FIELD', 'brand'], value: 'brand' },
    { key: ['FIELD', 'make'], value: 'make' },
    { key: ['FIELD', 'manufacturer'], value: 'manufacturer' },
    {
      key: ['IDX', 'body', ['a', '1.00']],
      value: [autoGeneratedIds[0], autoGeneratedIds[1]]
    },
    {
      key: ['IDX', 'body', ['about', '1.00']],
      value: [autoGeneratedIds[1]]
    },
    {
      key: ['IDX', 'body', ['bananas', '0.33']],
      value: [autoGeneratedIds[2]]
    },
    {
      key: ['IDX', 'body', ['bananas', '1.00']],
      value: [autoGeneratedIds[1]]
    },
    {
      key: ['IDX', 'body', ['document', '0.33']],
      value: [autoGeneratedIds[2]]
    },
    {
      key: ['IDX', 'body', ['document', '1.00']],
      value: [autoGeneratedIds[0], autoGeneratedIds[1]]
    },
    {
      key: ['IDX', 'body', ['interesting', '1.00']],
      value: [autoGeneratedIds[0]]
    },
    {
      key: ['IDX', 'body', ['is', '0.33']],
      value: [autoGeneratedIds[2]]
    },
    {
      key: ['IDX', 'body', ['is', '1.00']],
      value: [autoGeneratedIds[0], autoGeneratedIds[1]]
    },
    {
      key: ['IDX', 'body', ['mostly', '0.33']],
      value: [autoGeneratedIds[2]]
    },
    {
      key: ['IDX', 'body', ['not', '0.33']],
      value: [autoGeneratedIds[2]]
    },
    {
      key: ['IDX', 'body', ['oranges', '1.00']],
      value: [autoGeneratedIds[2]]
    },
    {
      key: ['IDX', 'body', ['really', '1.00']],
      value: [autoGeneratedIds[0]]
    },
    {
      key: ['IDX', 'body', ['this', '0.33']],
      value: [autoGeneratedIds[2]]
    },
    {
      key: ['IDX', 'body', ['this', '1.00']],
      value: [autoGeneratedIds[0], autoGeneratedIds[1]]
    },
    {
      key: ['IDX', 'brand', ['volvo', '1.00']],
      value: ['0', '1', '2']
    },
    { key: ['IDX', 'make', ['bmw', '1.00']], value: ['1'] },
    { key: ['IDX', 'make', ['tesla', '1.00']], value: ['0', '2'] },
    { key: ['IDX', 'manufacturer', ['tesla', '1.00']], value: ['2'] },
    {
      key: ['IDX', 'manufacturer', ['volvo', '1.00']],
      value: ['0', '1']
    }
  ]
  t.plan(indexEntries.length)
  new EntryStream(global[indexName].INDEX.STORE, {
    lt: ['~']
  }).on('data', d => {
    //      console.log(d)
    return t.deepEqual(d, indexEntries.shift())
  })
})

const indexName2 = sandbox + '_PUT-2'

test('create another search index', async t => {
  t.plan(1)
  try {
    global[indexName2] = await new SearchIndex({ name: indexName2 })
    t.ok(indexName)
  } catch (e) {
    t.error(e)
  }
})

test('can add data with skipFields specified', t => {
  t.plan(1)
  global[indexName2]
    .PUT(carData, {
      skipFields: ['make']
    })
    .then(response =>
      t.deepEquals(response, [
        { _id: '0', status: 'CREATED', operation: 'PUT' },
        { _id: '1', status: 'CREATED', operation: 'PUT' },
        { _id: '2', status: 'CREATED', operation: 'PUT' }
      ])
    )
})

test('Verify that PUT has created an appropriate index (_PUT_2)', t => {
  const indexEntries = [
    {
      key: ['CREATED_WITH'],
      value: 'search-index@' + packageVersion
    },
    {
      key: ['DOC', '0'],
      value: {
        _id: '0',
        make: [],
        manufacturer: [['volvo', '1.00']],
        brand: [['volvo', '1.00']]
      }
    },
    {
      key: ['DOC', '1'],
      value: {
        _id: '1',
        make: [],
        manufacturer: [['volvo', '1.00']],
        brand: [['volvo', '1.00']]
      }
    },
    {
      key: ['DOC', '2'],
      value: {
        _id: '2',
        make: [],
        manufacturer: [['tesla', '1.00']],
        brand: [['volvo', '1.00']]
      }
    },
    { key: ['DOCUMENT_COUNT'], value: 3 },
    {
      key: ['DOC_RAW', '0'],
      value: { _id: '0', make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', '1'],
      value: { _id: '1', make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo' }
    },
    {
      key: ['DOC_RAW', '2'],
      value: { _id: '2', make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo' }
    },
    { key: ['FIELD', 'brand'], value: 'brand' },
    // { key: ['FIELD', 'make'], value: 'make' },
    { key: ['FIELD', 'manufacturer'], value: 'manufacturer' },
    {
      key: ['IDX', 'brand', ['volvo', '1.00']],
      value: ['0', '1', '2']
    },
    // { key: ['IDX', 'make', [[]]], value: ['0', '1', '2'] },
    { key: ['IDX', 'manufacturer', ['tesla', '1.00']], value: ['2'] },
    {
      key: ['IDX', 'manufacturer', ['volvo', '1.00']],
      value: ['0', '1']
    }
  ]
  t.plan(indexEntries.length)
  new EntryStream(global[indexName2].INDEX.STORE, {
    lt: ['~']
  }).on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})

const indexName3 = sandbox + '_PUT-3'

test('create another search index', async t => {
  t.plan(1)
  try {
    global[indexName3] = await new SearchIndex({ name: indexName3 })
    t.ok(indexName)
  } catch (e) {
    t.error(e)
  }
})

test('can add data', t => {
  t.plan(1)
  global[indexName3]
    .PUT(
      [
        {
          _id: '0',
          make: 'Tesla',
          info: {
            manufacturer: 'Volvo',
            brand: 'Volvo'
          }
        },
        {
          _id: '1',
          make: 'BMW',
          info: {
            manufacturer: 'Volvo',
            brand: 'Volvo'
          }
        },
        {
          _id: '2',
          make: 'Tesla',
          info: {
            manufacturer: 'Tesla',
            brand: 'Volvo'
          }
        }
      ],
      {
        doNotIndexField: ['info.manufacturer']
      }
    )
    .then(response =>
      t.deepEquals(response, [
        { _id: '0', status: 'CREATED', operation: 'PUT' },
        { _id: '1', status: 'CREATED', operation: 'PUT' },
        { _id: '2', status: 'CREATED', operation: 'PUT' }
      ])
    )
})

test('Verify that PUT has created an appropriate index (_PUT_3)', t => {
  const indexEntries = [
    {
      key: ['CREATED_WITH'],
      value: 'search-index@' + packageVersion
    },
    {
      key: ['DOC', '0'],
      value: {
        _id: '0',
        make: [['tesla', '1.00']],
        info: {
          manufacturer: [['volvo', '1.00']],
          brand: [['volvo', '1.00']]
        }
      }
    },
    {
      key: ['DOC', '1'],
      value: {
        _id: '1',
        make: [['bmw', '1.00']],
        info: {
          manufacturer: [['volvo', '1.00']],
          brand: [['volvo', '1.00']]
        }
      }
    },
    {
      key: ['DOC', '2'],
      value: {
        _id: '2',
        make: [['tesla', '1.00']],
        info: {
          manufacturer: [['tesla', '1.00']],
          brand: [['volvo', '1.00']]
        }
      }
    },
    { key: ['DOCUMENT_COUNT'], value: 3 },
    {
      key: ['DOC_RAW', '0'],
      value: {
        _id: '0',
        make: 'Tesla',
        info: { manufacturer: 'Volvo', brand: 'Volvo' }
      }
    },
    {
      key: ['DOC_RAW', '1'],
      value: {
        _id: '1',
        make: 'BMW',
        info: { manufacturer: 'Volvo', brand: 'Volvo' }
      }
    },
    {
      key: ['DOC_RAW', '2'],
      value: {
        _id: '2',
        make: 'Tesla',
        info: { manufacturer: 'Tesla', brand: 'Volvo' }
      }
    },
    { key: ['FIELD', 'info.brand'], value: 'info.brand' },
    { key: ['FIELD', 'make'], value: 'make' },
    {
      key: ['IDX', 'info.brand', ['volvo', '1.00']],
      value: ['0', '1', '2']
    },
    { key: ['IDX', 'make', ['bmw', '1.00']], value: ['1'] },
    { key: ['IDX', 'make', ['tesla', '1.00']], value: ['0', '2'] }
  ]
  t.plan(indexEntries.length)
  new EntryStream(global[indexName3].INDEX.STORE, {
    lt: ['~']
  }).on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})

const indexName4 = sandbox + '_PUT-4'

test('create another search index', async t => {
  t.plan(1)
  try {
    global[indexName4] = await new SearchIndex({ name: indexName4 })
    t.ok(indexName)
  } catch (e) {
    t.error(e)
  }
})

test('can add data JSON', t => {
  t.plan(1)
  global[indexName4]
    .PUT(
      [
        {
          _id: '0',
          make: 'Tesla',
          info: {
            manufacturer: 'Volvo',
            brand: 'Volvo'
          }
        },
        {
          _id: '1',
          make: 'BMW',
          info: {
            manufacturer: 'Volvo',
            brand: 'Volvo'
          }
        },
        {
          _id: '2',
          make: 'Tesla',
          info: {
            manufacturer: 'Tesla',
            brand: 'Volvo'
          }
        }
      ],
      {
        doNotIndexField: ['info.manufacturer']
      }
    )
    .then(response =>
      t.deepEquals(response, [
        { _id: '0', status: 'CREATED', operation: 'PUT' },
        { _id: '1', status: 'CREATED', operation: 'PUT' },
        { _id: '2', status: 'CREATED', operation: 'PUT' }
      ])
    )
})

test('Verify that PUT has created an appropriate index (_PUT_4)', t => {
  const indexEntries = [
    {
      key: ['CREATED_WITH'],
      value: 'search-index@' + packageVersion
    },
    {
      key: ['DOC', '0'],
      value: {
        _id: '0',
        make: [['tesla', '1.00']],
        info: {
          manufacturer: [['volvo', '1.00']],
          brand: [['volvo', '1.00']]
        }
      }
    },
    {
      key: ['DOC', '1'],
      value: {
        _id: '1',
        make: [['bmw', '1.00']],
        info: {
          manufacturer: [['volvo', '1.00']],
          brand: [['volvo', '1.00']]
        }
      }
    },
    {
      key: ['DOC', '2'],
      value: {
        _id: '2',
        make: [['tesla', '1.00']],
        info: {
          manufacturer: [['tesla', '1.00']],
          brand: [['volvo', '1.00']]
        }
      }
    },
    { key: ['DOCUMENT_COUNT'], value: 3 },
    {
      key: ['DOC_RAW', '0'],
      value: {
        _id: '0',
        make: 'Tesla',
        info: { manufacturer: 'Volvo', brand: 'Volvo' }
      }
    },
    {
      key: ['DOC_RAW', '1'],
      value: {
        _id: '1',
        make: 'BMW',
        info: { manufacturer: 'Volvo', brand: 'Volvo' }
      }
    },
    {
      key: ['DOC_RAW', '2'],
      value: {
        _id: '2',
        make: 'Tesla',
        info: { manufacturer: 'Tesla', brand: 'Volvo' }
      }
    },
    { key: ['FIELD', 'info.brand'], value: 'info.brand' },
    { key: ['FIELD', 'make'], value: 'make' },
    {
      key: ['IDX', 'info.brand', ['volvo', '1.00']],
      value: ['0', '1', '2']
    },
    { key: ['IDX', 'make', ['bmw', '1.00']], value: ['1'] },
    { key: ['IDX', 'make', ['tesla', '1.00']], value: ['0', '2'] }
  ]
  t.plan(indexEntries.length)
  new EntryStream(global[indexName4].INDEX.STORE, {
    lt: ['~']
  }).on('data', d => {
    //      console.log(d)
    t.deepEquals(d, indexEntries.shift())
  })
})

const indexName5 = sandbox + '_PUT-5'

test('create another search index', async t => {
  t.plan(1)
  try {
    global[indexName5] = await new SearchIndex({ name: indexName5 })
    t.ok(indexName)
  } catch (e) {
    t.error(e)
  }
})

test('can add data JSON', t => {
  t.plan(1)
  global[indexName5]
    .PUT(
      [
        {
          _id: '0',
          make: 'Tesla',
          info: {
            manufacturer: {
              foo: 'XXX',
              bar: 'XXX'
            },
            brand: 'Volvo'
          }
        },
        {
          _id: '1',
          make: 'BMW',
          info: {
            manufacturer: {
              foo: 'XXX',
              bar: 'XXX'
            },
            brand: 'Volvo'
          }
        },
        {
          _id: '2',
          make: 'Tesla',
          info: {
            manufacturer: {
              foo: 'XXX',
              bar: 'XXX'
            },
            brand: 'Volvo'
          }
        }
      ],
      {
        doNotIndexField: ['info.manufacturer']
      }
    )
    .then(response =>
      t.deepEquals(response, [
        { _id: '0', status: 'CREATED', operation: 'PUT' },
        { _id: '1', status: 'CREATED', operation: 'PUT' },
        { _id: '2', status: 'CREATED', operation: 'PUT' }
      ])
    )
})

test('Verify that PUT has created an appropriate index (doesnt index children of DO_NOT_INDEX_FIELD)', t => {
  const indexEntries = [
    {
      key: ['CREATED_WITH'],
      value: 'search-index@' + packageVersion
    },
    {
      key: ['DOC', '0'],
      value: {
        _id: '0',
        make: [['tesla', '1.00']],
        info: {
          manufacturer: { foo: [['xxx', '1.00']], bar: [['xxx', '1.00']] },
          brand: [['volvo', '1.00']]
        }
      }
    },
    {
      key: ['DOC', '1'],
      value: {
        _id: '1',
        make: [['bmw', '1.00']],
        info: {
          manufacturer: { foo: [['xxx', '1.00']], bar: [['xxx', '1.00']] },
          brand: [['volvo', '1.00']]
        }
      }
    },
    {
      key: ['DOC', '2'],
      value: {
        _id: '2',
        make: [['tesla', '1.00']],
        info: {
          manufacturer: { foo: [['xxx', '1.00']], bar: [['xxx', '1.00']] },
          brand: [['volvo', '1.00']]
        }
      }
    },
    { key: ['DOCUMENT_COUNT'], value: 3 },
    {
      key: ['DOC_RAW', '0'],
      value: {
        _id: '0',
        make: 'Tesla',
        info: { manufacturer: { foo: 'XXX', bar: 'XXX' }, brand: 'Volvo' }
      }
    },
    {
      key: ['DOC_RAW', '1'],
      value: {
        _id: '1',
        make: 'BMW',
        info: { manufacturer: { foo: 'XXX', bar: 'XXX' }, brand: 'Volvo' }
      }
    },
    {
      key: ['DOC_RAW', '2'],
      value: {
        _id: '2',
        make: 'Tesla',
        info: { manufacturer: { foo: 'XXX', bar: 'XXX' }, brand: 'Volvo' }
      }
    },
    { key: ['FIELD', 'info.brand'], value: 'info.brand' },
    { key: ['FIELD', 'make'], value: 'make' },
    {
      key: ['IDX', 'info.brand', ['volvo', '1.00']],
      value: ['0', '1', '2']
    },
    { key: ['IDX', 'make', ['bmw', '1.00']], value: ['1'] },
    { key: ['IDX', 'make', ['tesla', '1.00']], value: ['0', '2'] }
  ]
  t.plan(indexEntries.length)
  new EntryStream(global[indexName5].INDEX.STORE, {
    lt: ['~']
  }).on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})

const indexName6 = sandbox + '_PUT-6'

test('create another search index', async t => {
  t.plan(1)
  try {
    global[indexName6] = await new SearchIndex({ name: indexName6 })
    t.ok(indexName)
  } catch (e) {
    t.error(e)
  }
})

test('can handle empty fields', t => {
  t.plan(1)
  global[indexName6]
    .PUT(
      [
        {
          _id: '0',
          make: 'Tesla',
          info: {
            manufacturer: {
              foo: 'XXX',
              bar: 'XXX'
            },
            brand: ''
          }
        },
        {
          _id: '1',
          make: null,
          info: {
            manufacturer: {
              foo: 'XXX',
              bar: 'XXX'
            },
            brand: false
          }
        },
        {
          _id: '2',
          make: 0,
          info: {
            manufacturer: {
              foo: 'XXX',
              bar: 'XXX'
            },
            brand: 'Volvo'
          }
        }
      ],
      {
        doNotIndexField: ['info.manufacturer']
      }
    )
    .then(response =>
      t.deepEquals(response, [
        { _id: '0', status: 'CREATED', operation: 'PUT' },
        { _id: '1', status: 'CREATED', operation: 'PUT' },
        { _id: '2', status: 'CREATED', operation: 'PUT' }
      ])
    )
})

test('docs look good', t => {
  t.plan(1)
  global[indexName6].ALL_DOCUMENTS().then(response =>
    t.deepEquals(response, [
      {
        _id: '0',
        _doc: {
          _id: '0',
          make: 'Tesla',
          info: { manufacturer: { foo: 'XXX', bar: 'XXX' }, brand: '' }
        }
      },
      {
        _id: '1',
        _doc: {
          _id: '1',
          make: null,
          info: { manufacturer: { foo: 'XXX', bar: 'XXX' }, brand: false }
        }
      },
      {
        _id: '2',
        _doc: {
          _id: '2',
          make: 0,
          info: { manufacturer: { foo: 'XXX', bar: 'XXX' }, brand: 'Volvo' }
        }
      }
    ])
  )
})
