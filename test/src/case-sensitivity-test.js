import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const caseSensitivityTest = sandbox + 'caseSensitivityTest'
const caseInsensitivityTest = sandbox + 'caseInsensitivityTest'

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
  },
  {
    "_id": 3,
    "make": "Tesla",
    "manufacturer": "Volvo",
    "brand": "BMW"
  },
  {
    "_id": 4,
    "make": "Volvo",
    "manufacturer": "Volvo",
    "brand": "Volvo"
  },
  {
    "_id": 5,
    "make": "Volvo",
    "manufacturer": "Tesla",
    "brand": "Volvo"
  },
  {
    "_id": 6,
    "make": "Tesla",
    "manufacturer": "Tesla",
    "brand": "BMW"
  },
  {
    "_id": 7,
    "make": "BMW",
    "manufacturer": "Tesla",
    "brand": "Tesla"
  },
  {
    "_id": 8,
    "make": "Volvo",
    "manufacturer": "BMW",
    "brand": "Tesla"
  },
  {
    "_id": 9,
    "maKE": "BMW",
    "manufacturer": "Tesla",
    "brand": "Volvo"
  }
]


test('create a case sensitive search index', t => {
  t.plan(1)
  si({
    name: caseSensitivityTest,
    caseSensitive: true
  }).then(db => {
    global[caseSensitivityTest] = db    
    t.pass('ok')
  })
})

test('can add data to case sensitive index', t => {
  t.plan(1)
  global[caseSensitivityTest]._PUT(data).then(t.pass)
})

test('Match maKE:BMW', t => {
  t.plan(1)
  global[caseSensitivityTest].GET({
    AND: [ 'maKE:BMW' ]
  }).then(res => {
    t.deepEqual(res, [
      { _id: '9', _match: [ 'maKE:BMW#1.00' ] }
    ])
  })
})

test('Match make:bmw', t => {
  t.plan(1)
  global[caseSensitivityTest].GET({
    AND: [ 'make:bmw' ]
  }).then(res => {
    t.deepEqual(res, [
    ])
  })
})

test('Match make:BMW', t => {
  t.plan(1)
  global[caseSensitivityTest].GET({
    AND: [ 'make:BMW' ]
  }).then(res => {
    t.deepEqual(res, [
      { _id: '1', _match: [ 'make:BMW#1.00' ] },
      { _id: '7', _match: [ 'make:BMW#1.00' ] }
    ])
  })
})

test('create a case insensitive search index', t => {
  t.plan(1)
  si({
    name: caseInsensitivityTest,
    caseSensitive: false
  }).then(db => {
    global[caseInsensitivityTest] = db    
    t.pass('ok')
  })
})

test('can add data to case insensitive index', t => {
  t.plan(1)
  global[caseInsensitivityTest]._PUT(data).then(t.pass)
})

test('Match maKE:BMW', t => {
  t.plan(1)
  global[caseInsensitivityTest].GET({
    AND: [ 'maKE:BMW' ]
  }).then(res => {
    t.deepEqual(res, [
      { _id: '1', _match: [ 'make:bmw#1.00' ] },
      { _id: '7', _match: [ 'make:bmw#1.00' ] },
      { _id: '9', _match: [ 'make:bmw#1.00' ] }
    ])
  })
})

test('Match make:bmw', t => {
  t.plan(1)
  global[caseInsensitivityTest].GET({
    AND: [ 'make:bmw' ]
  }).then(res => {
    t.deepEqual(res, [
      { _id: '1', _match: [ 'make:bmw#1.00' ] },
      { _id: '7', _match: [ 'make:bmw#1.00' ] },
      { _id: '9', _match: [ 'make:bmw#1.00' ] }
    ])
  })
})

test('Match make:BMW', t => {
  t.plan(1)
  global[caseInsensitivityTest].GET({
    AND: [ 'make:BMW' ]
  }).then(res => {
    t.deepEqual(res, [
      { _id: '1', _match: [ 'make:bmw#1.00' ] },
      { _id: '7', _match: [ 'make:bmw#1.00' ] },
      { _id: '9', _match: [ 'make:bmw#1.00' ] }
    ])
  })
})
