const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_SORT'

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
      brand: 'Volvo',
      price: 3000
    },
    {
      _id: 1,
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      price: 12000
    },
    {
      _id: 2,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      price: 14000
    },
    {
      _id: 3,
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW',
      price: 140000
    },
    {
      _id: 4,
      make: 'Volvo',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      price: 1000
    },
    {
      _id: 5,
      make: 'Volvo',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      price: 2000
    },
    {
      _id: 6,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'BMW',
      price: 500
    },
    {
      _id: 7,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Tesla',
      price: 5000
    },
    {
      _id: 8,
      make: 'Volvo',
      manufacturer: 'BMW',
      brand: 'Tesla',
      price: 100
    },
    {
      _id: 9,
      make: 'BMW',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      price: 1100
    }
  ]

  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('_SORT ALPHABETIC DESCENDING', t => {
  t.plan(1)
  global[indexName].DOCUMENTS().then(
    docs => global[indexName]._SORT(docs, {
      TYPE: 'ALPHABETIC',
      DIRECTION: 'DESCENDING',
      FIELD: '_doc.make'
    })
  ).then(res => {
    t.deepEqual(res, [

      { _id: 4, _doc: { _id: 4, make: 'Volvo', manufacturer: 'Volvo', brand: 'Volvo', price: 1000 } },
      { _id: 5, _doc: { _id: 5, make: 'Volvo', manufacturer: 'Tesla', brand: 'Volvo', price: 2000 } },
      { _id: 8, _doc: { _id: 8, make: 'Volvo', manufacturer: 'BMW', brand: 'Tesla', price: 100 } },
      { _id: 0, _doc: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo', price: 3000 } },
      { _id: 2, _doc: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo', price: 14000 } },
      { _id: 3, _doc: { _id: 3, make: 'Tesla', manufacturer: 'Volvo', brand: 'BMW', price: 140000 } },
      { _id: 6, _doc: { _id: 6, make: 'Tesla', manufacturer: 'Tesla', brand: 'BMW', price: 500 } },
      { _id: 1, _doc: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo', price: 12000 } },
      { _id: 7, _doc: { _id: 7, make: 'BMW', manufacturer: 'Tesla', brand: 'Tesla', price: 5000 } },
      { _id: 9, _doc: { _id: 9, make: 'BMW', manufacturer: 'Tesla', brand: 'Volvo', price: 1100 } }

    ])
  })
})

test('_SORT ALPHABETIC ASCENDING', t => {
  t.plan(1)
  global[indexName].DOCUMENTS().then(
    docs => global[indexName]._SORT(docs, {
      TYPE: 'ALPHABETIC',
      DIRECTION: 'ASCENDING',
      FIELD: '_doc.make'
    })
  ).then(res => {
    t.deepEqual(res, [
      { _id: 1, _doc: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo', price: 12000 } },
      { _id: 7, _doc: { _id: 7, make: 'BMW', manufacturer: 'Tesla', brand: 'Tesla', price: 5000 } },
      { _id: 9, _doc: { _id: 9, make: 'BMW', manufacturer: 'Tesla', brand: 'Volvo', price: 1100 } },
      { _id: 0, _doc: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo', price: 3000 } },
      { _id: 2, _doc: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo', price: 14000 } },
      { _id: 3, _doc: { _id: 3, make: 'Tesla', manufacturer: 'Volvo', brand: 'BMW', price: 140000 } },
      { _id: 6, _doc: { _id: 6, make: 'Tesla', manufacturer: 'Tesla', brand: 'BMW', price: 500 } },
      { _id: 4, _doc: { _id: 4, make: 'Volvo', manufacturer: 'Volvo', brand: 'Volvo', price: 1000 } },
      { _id: 5, _doc: { _id: 5, make: 'Volvo', manufacturer: 'Tesla', brand: 'Volvo', price: 2000 } },
      { _id: 8, _doc: { _id: 8, make: 'Volvo', manufacturer: 'BMW', brand: 'Tesla', price: 100 } }
    ])
  })
})

test('_SORT NUMERIC ASCENDING', t => {
  t.plan(1)
  global[indexName].DOCUMENTS().then(
    docs => global[indexName]._SORT(docs, {
      TYPE: 'NUMERIC',
      DIRECTION: 'ASCENDING',
      FIELD: '_doc.price'
    })
  ).then(res => {
    t.deepEqual(res, [
      { _id: 8, _doc: { _id: 8, make: 'Volvo', manufacturer: 'BMW', brand: 'Tesla', price: 100 } },
      { _id: 6, _doc: { _id: 6, make: 'Tesla', manufacturer: 'Tesla', brand: 'BMW', price: 500 } },
      { _id: 4, _doc: { _id: 4, make: 'Volvo', manufacturer: 'Volvo', brand: 'Volvo', price: 1000 } },
      { _id: 9, _doc: { _id: 9, make: 'BMW', manufacturer: 'Tesla', brand: 'Volvo', price: 1100 } },
      { _id: 5, _doc: { _id: 5, make: 'Volvo', manufacturer: 'Tesla', brand: 'Volvo', price: 2000 } },
      { _id: 0, _doc: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo', price: 3000 } },
      { _id: 7, _doc: { _id: 7, make: 'BMW', manufacturer: 'Tesla', brand: 'Tesla', price: 5000 } },
      { _id: 1, _doc: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo', price: 12000 } },
      { _id: 2, _doc: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo', price: 14000 } },
      { _id: 3, _doc: { _id: 3, make: 'Tesla', manufacturer: 'Volvo', brand: 'BMW', price: 140000 } }
    ])
  })
})

test('_SORT NUMERIC DESCENDING', t => {
  t.plan(1)
  global[indexName].DOCUMENTS().then(
    docs => global[indexName]._SORT(docs, {
      TYPE: 'NUMERIC',
      DIRECTION: 'DESCENDING',
      FIELD: '_doc.price'
    })
  ).then(res => {
    t.deepEqual(res, [
      { _id: 3, _doc: { _id: 3, make: 'Tesla', manufacturer: 'Volvo', brand: 'BMW', price: 140000 } },
      { _id: 2, _doc: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo', price: 14000 } },
      { _id: 1, _doc: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo', price: 12000 } },
      { _id: 7, _doc: { _id: 7, make: 'BMW', manufacturer: 'Tesla', brand: 'Tesla', price: 5000 } },
      { _id: 0, _doc: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo', price: 3000 } },
      { _id: 5, _doc: { _id: 5, make: 'Volvo', manufacturer: 'Tesla', brand: 'Volvo', price: 2000 } },
      { _id: 9, _doc: { _id: 9, make: 'BMW', manufacturer: 'Tesla', brand: 'Volvo', price: 1100 } },
      { _id: 4, _doc: { _id: 4, make: 'Volvo', manufacturer: 'Volvo', brand: 'Volvo', price: 1000 } },
      { _id: 6, _doc: { _id: 6, make: 'Tesla', manufacturer: 'Tesla', brand: 'BMW', price: 500 } },
      { _id: 8, _doc: { _id: 8, make: 'Volvo', manufacturer: 'BMW', brand: 'Tesla', price: 100 } }
    ])
  })
})

test('SORT NUMERIC DESCENDING (JSON)', t => {
  t.plan(1)
  global[indexName].QUERY({
    DOCUMENTS: []
  }, {
    SORT: {
      TYPE: 'NUMERIC',
      DIRECTION: 'DESCENDING',
      FIELD: '_doc.price'
    }
  }).then(res => {
    t.deepEqual(res, {
      RESULT: [
        { _id: 3, _doc: { _id: 3, make: 'Tesla', manufacturer: 'Volvo', brand: 'BMW', price: 140000 } },
        { _id: 2, _doc: { _id: 2, make: 'Tesla', manufacturer: 'Tesla', brand: 'Volvo', price: 14000 } },
        { _id: 1, _doc: { _id: 1, make: 'BMW', manufacturer: 'Volvo', brand: 'Volvo', price: 12000 } },
        { _id: 7, _doc: { _id: 7, make: 'BMW', manufacturer: 'Tesla', brand: 'Tesla', price: 5000 } },
        { _id: 0, _doc: { _id: 0, make: 'Tesla', manufacturer: 'Volvo', brand: 'Volvo', price: 3000 } },
        { _id: 5, _doc: { _id: 5, make: 'Volvo', manufacturer: 'Tesla', brand: 'Volvo', price: 2000 } },
        { _id: 9, _doc: { _id: 9, make: 'BMW', manufacturer: 'Tesla', brand: 'Volvo', price: 1100 } },
        { _id: 4, _doc: { _id: 4, make: 'Volvo', manufacturer: 'Volvo', brand: 'Volvo', price: 1000 } },
        { _id: 6, _doc: { _id: 6, make: 'Tesla', manufacturer: 'Tesla', brand: 'BMW', price: 500 } },
        { _id: 8, _doc: { _id: 8, make: 'Volvo', manufacturer: 'BMW', brand: 'Tesla', price: 100 } }
      ],
      RESULT_LENGTH: 10
    })
  })
})

test('SORT on _match.price without fetching documents', t => {
  t.plan(1)
  global[indexName].SEARCH([{
    FIELD: 'price'
  }], {
    SORT: {
      TYPE: 'NUMERIC',
      DIRECTION: 'DESCENDING',
      FIELD: '_match.price'
    }
  }).then(({ RESULT }) => {
    t.deepEqual(RESULT, [
      { _id: '3', _match: ['price:140000#1.00'], _score: 0.1 },
      { _id: '2', _match: ['price:14000#1.00'], _score: 0.1 },
      { _id: '1', _match: ['price:12000#1.00'], _score: 0.1 },
      { _id: '7', _match: ['price:5000#1.00'], _score: 0.1 },
      { _id: '0', _match: ['price:3000#1.00'], _score: 0.1 },
      { _id: '5', _match: ['price:2000#1.00'], _score: 0.1 },
      { _id: '9', _match: ['price:1100#1.00'], _score: 0.1 },
      { _id: '4', _match: ['price:1000#1.00'], _score: 0.1 },
      { _id: '6', _match: ['price:500#1.00'], _score: 0.1 },
      { _id: '8', _match: ['price:100#1.00'], _score: 0.1 }
    ])
  })
})

test('gracefully handle SORT on field that doesnt exist', t => {
  t.plan(1)
  global[indexName].SEARCH([{
    FIELD: 'price'
  }], {
    SORT: {
      TYPE: 'NUMERIC',
      DIRECTION: 'DESCENDING',
      FIELD: '_match.DOESNOTEXIST'
    }
  }).then(({ RESULT }) => {
    t.deepEqual(RESULT, [
      { _id: '0', _match: ['price:3000#1.00'], _score: 0.1 },
      { _id: '1', _match: ['price:12000#1.00'], _score: 0.1 },
      { _id: '2', _match: ['price:14000#1.00'], _score: 0.1 },
      { _id: '3', _match: ['price:140000#1.00'], _score: 0.1 },
      { _id: '4', _match: ['price:1000#1.00'], _score: 0.1 },
      { _id: '5', _match: ['price:2000#1.00'], _score: 0.1 },
      { _id: '6', _match: ['price:500#1.00'], _score: 0.1 },
      { _id: '7', _match: ['price:5000#1.00'], _score: 0.1 },
      { _id: '8', _match: ['price:100#1.00'], _score: 0.1 },
      { _id: '9', _match: ['price:1100#1.00'], _score: 0.1 }

    ])
  })
})

// TODO: SORT should default to sort on _score (make test)
// TODO: nicely handle SORT fields that dont exist
