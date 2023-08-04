import test from 'tape'

const { SearchIndex } = await import(
  '../../src/' + process.env.SI_TEST_ENTRYPOINT
)

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_DOCUMENT_VECTORS'

const data = [
  {
    _id: '7',
    text: 'The Beatles’ legendary animated hit film YELLOW SUBMARINE is returning to cinemas across the UK, Ireland and the U.S. this summer.'
  },
  {
    _id: '8',
    text: 'Paul has confirmed his first live dates of 2018 headlining the Austin City Limits Music Festival. The festival will take place across the weekends of 5-7th and 12-14th October. Other artists set to appear include Metallica, Childish Gambino, Arctic Monkeys, Travis Scott, Odesza, The National and more.'
  },
  {
    _id: '9',
    nestedStuff: {
      one: 'the first',
      two: 'the second',
      array: [1, 2, 3, 'four']
    },
    text: 'Paul invites you on a musical journey to Egypt Station, estimated time of arrival Friday 7th September 7, 2018 by way of Capitol Records.'
  }
]
const global = {}

test('create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({
      name: indexName,
      caseSensitive: false,
      storeVectors: true
    })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add data', t => {
  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('DOCUMENT_VECTORS()', t => {
  t.plan(1)
  global[indexName].DOCUMENT_VECTORS('9').then(vec => {
    t.deepEqual(vec, [
      {
        _id: '9',
        nestedStuff: {
          one: [
            ['first', '1.00'],
            ['the', '1.00']
          ],
          two: [
            ['second', '1.00'],
            ['the', '1.00']
          ],
          array: [[1, 1], [2, 2], [3, 3], [['four', '1.00']]]
        },
        text: [
          ['2018', '0.50'],
          ['7', '0.50'],
          ['7th', '0.50'],
          ['a', '0.50'],
          ['arrival', '0.50'],
          ['by', '0.50'],
          ['capitol', '0.50'],
          ['egypt', '0.50'],
          ['estimated', '0.50'],
          ['friday', '0.50'],
          ['invites', '0.50'],
          ['journey', '0.50'],
          ['musical', '0.50'],
          ['of', '1.00'],
          ['on', '0.50'],
          ['paul', '0.50'],
          ['records', '0.50'],
          ['september', '0.50'],
          ['station', '0.50'],
          ['time', '0.50'],
          ['to', '0.50'],
          ['way', '0.50'],
          ['you', '0.50']
        ]
      }
    ])
  })
})

test('DOCUMENT_VECTORS()', t => {
  t.plan(1)
  global[indexName].DOCUMENT_VECTORS('3', '7', '9').then(vec => {
    t.deepEqual(vec, [
      null,
      {
        _id: '7',
        text: [
          ['across', '0.33'],
          ['and', '0.33'],
          ['animated', '0.33'],
          ['beatles', '0.33'],
          ['cinemas', '0.33'],
          ['film', '0.33'],
          ['hit', '0.33'],
          ['ireland', '0.33'],
          ['is', '0.33'],
          ['legendary', '0.33'],
          ['returning', '0.33'],
          ['s', '0.33'],
          ['submarine', '0.33'],
          ['summer', '0.33'],
          ['the', '1.00'],
          ['this', '0.33'],
          ['to', '0.33'],
          ['u', '0.33'],
          ['uk', '0.33'],
          ['yellow', '0.33']
        ]
      },
      {
        _id: '9',
        nestedStuff: {
          one: [
            ['first', '1.00'],
            ['the', '1.00']
          ],
          two: [
            ['second', '1.00'],
            ['the', '1.00']
          ],
          array: [[1, 1], [2, 2], [3, 3], [['four', '1.00']]]
        },
        text: [
          ['2018', '0.50'],
          ['7', '0.50'],
          ['7th', '0.50'],
          ['a', '0.50'],
          ['arrival', '0.50'],
          ['by', '0.50'],
          ['capitol', '0.50'],
          ['egypt', '0.50'],
          ['estimated', '0.50'],
          ['friday', '0.50'],
          ['invites', '0.50'],
          ['journey', '0.50'],
          ['musical', '0.50'],
          ['of', '1.00'],
          ['on', '0.50'],
          ['paul', '0.50'],
          ['records', '0.50'],
          ['september', '0.50'],
          ['station', '0.50'],
          ['time', '0.50'],
          ['to', '0.50'],
          ['way', '0.50'],
          ['you', '0.50']
        ]
      }
    ])
  })
})

test('create a search index with storeVectors: false', async t => {
  t.plan(1)
  try {
    global[indexName + '1'] = await new SearchIndex({
      name: indexName + '1',
      caseSensitive: false,
      storeVectors: false
    })
    t.ok(indexName)
  } catch (e) {
    t.error(e)
  }
})

test('DOCUMENT_VECTORS()', t => {
  t.plan(1)
  global[indexName + '1'].DOCUMENT_VECTORS('3', '7', '9').then(vec => {
    t.deepEqual(vec, [null, null, null])
  })
})

test('default is storeVectors: false', async t => {
  t.plan(1)
  try {
    global[indexName + '2'] = await new SearchIndex({
      name: indexName + '2',
      caseSensitive: false
    })
    t.ok(indexName)
  } catch (e) {
    t.error(e)
  }
})

test('DOCUMENT_VECTORS()', t => {
  t.plan(1)
  global[indexName + '2'].DOCUMENT_VECTORS('3', '7', '9').then(vec => {
    t.deepEqual(vec, [null, null, null])
  })
})
