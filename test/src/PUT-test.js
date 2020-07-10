import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'PUT'

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
      "brand": "Volvo"
    },
    {
      "_id": 2,
      "make": "Tesla",
      "manufacturer": "Tesla",
      "brand": "Volvo"
    }
  ]
  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})


test('Verify that PUT has created an appropriate index', t => {
  const indexEntries = [
    {"key":"brand:volvo#1.00","value":["0","1","2"]},
    {"key":"make:bmw#1.00","value":["1"]},
    {"key":"make:tesla#1.00","value":["0","2"]},
    {"key":"manufacturer:tesla#1.00","value":["2"]},
    {"key":"manufacturer:volvo#1.00","value":["0","1"]},
    {"key":"￮DOCUMENT_COUNT￮","value":3},
    {"key":"￮DOC_RAW￮0￮","value":{"_id":0,"make":"Tesla","manufacturer":"Volvo","brand":"Volvo"}},
    {"key":"￮DOC_RAW￮1￮","value":{"_id":1,"make":"BMW","manufacturer":"Volvo","brand":"Volvo"}},
    {"key":"￮DOC_RAW￮2￮","value":{"_id":2,"make":"Tesla","manufacturer":"Tesla","brand":"Volvo"}},
    {"key":"￮DOC￮0￮","value":{"_id":"0","make":["tesla#1.00"],"manufacturer":["volvo#1.00"],"brand":["volvo#1.00"]}},
    {"key":"￮DOC￮1￮","value":{"_id":"1","make":["bmw#1.00"],"manufacturer":["volvo#1.00"],"brand":["volvo#1.00"]}},
    {"key":"￮DOC￮2￮","value":{"_id":"2","make":["tesla#1.00"],"manufacturer":["tesla#1.00"],"brand":["volvo#1.00"]}},
    {"key":"￮FIELD￮brand￮","value":"brand"},
    {"key":"￮FIELD￮make￮","value":"make"},
    {"key":"￮FIELD￮manufacturer￮","value":"manufacturer"},
  ]
  t.plan(indexEntries.length)
  global[indexName].INDEX.STORE.createReadStream().on('data', d => {
    t.deepEquals(d, indexEntries.shift())
  })
})


