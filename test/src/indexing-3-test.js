const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'indexing-3'

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
      extraField: 'EXTRA FIELD- w00t!',
      aRandomArray: [
        'this',
        'should',
        'index',
        {
          so: 'should',
          these: 'values'
        }
      ]
    },
    {
      _id: '1',
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: [null],
      nestedValues: {
        foo: 'bar',
        boom: 'diggy'
      },
      bands: [
        {
          drums: 'ringo',
          vocals: 'john'
        },
        {
          drums: 'charlie',
          vocals: 'mick',
          equipment: ['bass', 'drums', 'guitar']
        }
      ]
    }
  ]
  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('get all docs with null values with DOCUMENT', t => {
  t.plan(1)
  global[indexName].DOCUMENTS().then(documents => {
    t.deepEqual(documents, [
      {
        _id: '0',
        _doc: {
          _id: '0',
          make: 'Tesla',
          manufacturer: 'Volvo',
          brand: null,
          extraField: 'EXTRA FIELD- w00t!',
          aRandomArray: [
            'this',
            'should',
            'index',
            { so: 'should', these: 'values' }
          ]
        }
      },
      {
        _id: '1',
        _doc: {
          _id: '1',
          make: 'BMW',
          manufacturer: 'Volvo',
          brand: [null],
          nestedValues: { foo: 'bar', boom: 'diggy' },
          bands: [
            { drums: 'ringo', vocals: 'john' },
            {
              drums: 'charlie',
              vocals: 'mick',
              equipment: ['bass', 'drums', 'guitar']
            }
          ]
        }
      }
    ])
  })
})
