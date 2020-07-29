import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'FIELDS'

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
      "_id": 0,
      "make": "Tesla",
      "manufacturer": "Volvo",
      "brand": "Volvo"
    },
    {
      "_id": 1,
      "make": "BMW",
      "manufacturer": "Volvo",
      "brand": "Volvo",
      "wheels": {
        "tyres": "goodyear",
        "rims": "alloy"
      }
    },
    {
      "_id": 2,
      "make": "Tesla",
      "manufacturer": "Tesla",
      "brand": "Volvo",
      "colours": [
        {
          "roof": {
            "color": [ 'red', 'yellow' ],
            "type": "gloss"
          }
        },
        {
          "bonnet": {
            "color": [ 'green', 'yellow' ],
            "type": "matt"
          }
        }
      ]
    },
    {
      "_id": 3,
      "make": "Tesla",
      "manufacturer": "Volvo",
      "brand": "BMW"
    }
  ]
  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})


test('simple FIELDS', t => {
  t.plan(1)
  global[indexName].FIELDS().then(res => {
    t.deepEqual(res, [
      'brand',
      'colours.bonnet.color',
      'colours.bonnet.type',
      'colours.roof.color',
      'colours.roof.type',
      'make',
      'manufacturer',
      'wheels.rims',
      'wheels.tyres'
    ])
  })
})

test('simple FIELDS (JSON)', t => {
  t.plan(1)
  global[indexName].QUERY({
    FIELDS: true
  }).then(res => {
    t.deepEqual(res, [
      'brand',
      'colours.bonnet.color',
      'colours.bonnet.type',
      'colours.roof.color',
      'colours.roof.type',
      'make',
      'manufacturer',
      'wheels.rims',
      'wheels.tyres'
    ])
  })
})

