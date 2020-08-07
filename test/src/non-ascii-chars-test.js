import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'non-ascii-chars-test'

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
      "text": "brønnøysundregisterene er gøy"
    },
    {
      "_id": 1,
      "text": "En two tre fire blåtind fem seks"
    },
    {
      "_id": 2,
      "text": "godt gjæret øl"
    },
    {
      "_id": 3,
      "text": "A ticket to 大阪 costs ¥2000."
    },
    {
      "_id": 4,
      "text": "Приключения Алисы в Стране чудес"
    }
  ]

  t.plan(1)
  global[indexName]._PUT(data).then(t.pass)
})

test('match brønnøysundregisterene', t => {
  t.plan(1)
  global[indexName]._AND('brønnøysundregisterene').then(res => {
    t.deepEqual(res, [
      { _id: '0', _match: [ 'text:brønnøysundregisterene#1.00' ] } 
    ])
  })
})

test('match blåtind', t => {
  t.plan(1)
  global[indexName]._AND('blåtind').then(res => {
    t.deepEqual(res, [
      { _id: '1', _match: [ 'text:blåtind#1.00' ] } 
    ])
  })
})

test('match gjæret øl', t => {
  t.plan(1)
  global[indexName]._AND('gjæret', 'øl').then(res => {
    t.deepEqual(res, [
      { _id: '2', _match: [ 'text:gjæret#1.00', 'text:øl#1.00' ] } 
    ])
  })
})

test('match 大阪 costs 2000', t => {
  t.plan(1)
  global[indexName]._AND('大阪', 'costs', '2000').then(res => {
    t.deepEqual(res, [
      { _id: '3', _match: [
        'text:大阪#1.00', 'text:costs#1.00', 'text:2000#1.00'
      ] } 
    ])
  })
})

test('Приключения Алисы в стране чудес', t => {
  t.plan(1)
  global[indexName]._AND('стране', 'чудес').then(res => {
    t.deepEqual(res, [
      { _id: '4', _match: [ 'text:стране#1.00', 'text:чудес#1.00' ] } 
    ])
  })
})

