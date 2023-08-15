import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_DOCUMENTS'

const data = [
  {
    _id: '0',
    text: 'The Beatles were an English rock band formed in Liverpool in 1960. With a line-up comprising John Lennon, Paul McCartney, George Harrison and Ringo Starr, they are regarded as the most influential band of all time.'
  },
  {
    _id: '1',
    text: 'We saw the suits in a window and just went in and got them. We all got one, and suddenly that was a uniform. We were going to all these shops and buying little uniforms for ourselves. That’s also why we looked like Beatles; beside the haircut, we were all looking the same'
  },
  {
    _id: '2',
    text: 'Over 50 years after the The Beatles’ ‘White Album’ first stormed the charts, the Fab Four’s iconic double-album achieved 24-time Platinum-certified status – making it the fourth-highest certified release in US history.'
  },
  {
    _id: '3',
    text: 'I was relieved to discover the reality is very different to the myth,” continues Jackson, “it’s simply an amazing historical treasure-trove. Sure, there’s moments of drama - but none of the discord this project has long been associated with. Watching John, Paul, George, and Ringo work together, creating now-classic songs from scratch, is not only fascinating - it’s funny, uplifting and surprisingly intimate'
  },
  {
    _id: '4',
    text: "Paul McCartney will make his eagerly anticipated return to the road with his new 'Freshen Up' tour."
  },
  {
    _id: '5',
    text: "John Lennon's most celebrated solo album will be honoured with a number of special releases on October 5 including the six-disc Imagine – The Ultimate Collection."
  },
  {
    _id: '6',
    text: 'The George Harrison estate is happy to announce HariSongs, a new label created in partnership with Craft Recordings to celebrate the Indian classical music George loved and believed would “help as a balance towards a peaceful daily life.”  HariSongs launches today with two reissues in honour of both Ravi Shankar’s birthday (b. 7th April, 1920) and Ali Akbar Khan’s birthday (b. 14th April, 1922) this month.'
  },
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
    text: 'Paul invites you on a musical journey to Egypt Station, estimated time of arrival Friday 7th September 7, 2018 by way of Capitol Records.'
  }
]
const global = {}

test('create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({
      name: indexName,
      caseSensitive: false
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

test('simple SEARCH with 2 clauses and documents (JSON)', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        AND: ['paul', 'and']
      },
      {
        DOCUMENTS: true,
        SCORE: 'TFIDF',
        SORT: true
      }
    )
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '0',
            _match: [
              { FIELD: 'text', VALUE: 'and', SCORE: '0.50' },
              { FIELD: 'text', VALUE: 'paul', SCORE: '0.50' }
            ],
            _score: 1.3,
            _doc: data[0]
          },
          {
            _id: '3',
            _match: [
              { FIELD: 'text', VALUE: 'and', SCORE: '0.67' },
              { FIELD: 'text', VALUE: 'paul', SCORE: '0.33' }
            ],
            _score: 1.3,
            _doc: data[3]
          },
          {
            _id: '8',
            _match: [
              { FIELD: 'text', VALUE: 'and', SCORE: '0.50' },
              { FIELD: 'text', VALUE: 'paul', SCORE: '0.25' }
            ],
            _score: 0.97,
            _doc: data[8]
          }
        ],
        RESULT_LENGTH: 3
      })
    })
})

test('DOCUMENTS() returns all documents', t => {
  t.plan(1)
  global[indexName].DOCUMENTS().then(documents => {
    t.deepEqual(
      documents,
      data.map(d => ({
        _id: d._id,
        _doc: d
      }))
    )
  })
})

test('ALL_DOCUMENTS() returns all documents', t => {
  t.plan(1)
  global[indexName].ALL_DOCUMENTS().then(documents => {
    t.deepEqual(
      documents,
      data.map(d => ({
        _id: d._id,
        _doc: d
      }))
    )
  })
})

test('ALL_DOCUMENTS(5) returns 5 documents', t => {
  t.plan(1)
  global[indexName].ALL_DOCUMENTS(5).then(documents => {
    t.deepEqual(
      documents,
      data.slice(0, 5).map(d => ({
        _id: d._id,
        _doc: d
      }))
    )
  })
})

test('DOCUMENTS() can return named documents', t => {
  t.plan(1)
  global[indexName].DOCUMENTS('8', '9').then(documents => {
    t.deepEqual(documents, [data[8], data[9]])
  })
})

test('access ALL_DOCUMENTS() through QUERY()', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      ALL_DOCUMENTS: 1
    })
    .then(documents => {
      t.deepEqual(documents, {
        RESULT: [
          {
            _id: data[0]._id,
            _doc: data[0]
          }
        ],
        RESULT_LENGTH: 1
      })
    })
})

test('access ALL_DOCUMENTS() through QUERY() unlimited (-1)', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      ALL_DOCUMENTS: -1
    })
    .then(documents => {
      t.deepEqual(documents, {
        RESULT: data.map(doc => ({
          _id: doc._id,
          _doc: doc
        })),
        RESULT_LENGTH: 10
      })
    })
})

test('access ALL_DOCUMENTS() through QUERY() unlimited (-1) with PAGE', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        ALL_DOCUMENTS: -1
      },
      {
        PAGE: {
          NUMBER: 0,
          SIZE: 2
        }
      }
    )
    .then(documents => {
      t.deepEqual(documents, {
        RESULT: data.slice(0, 2).map(doc => ({
          _id: doc._id,
          _doc: doc
        })),
        RESULT_LENGTH: 10
      })
    })
})
