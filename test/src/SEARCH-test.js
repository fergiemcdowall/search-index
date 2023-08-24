import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + '_SEARCH'
// const sanitise = str => str.replace(/[^0-9a-z ]/gi, '').toLowerCase()
const data = [
  {
    _id: 0,
    text: 'The Beatles were an English rock band formed in Liverpool in 1960. With a line-up comprising John Lennon, Paul McCartney, George Harrison and Ringo Starr, they are regarded as the most influential band of all time.',
    year: '2013'
  },
  {
    _id: 1,
    text: 'We saw the suits in a window and just went in and got them. We all got one, and suddenly that was a uniform. We were going to all these shops and buying little uniforms for ourselves. That’s also why we looked like Beatles; beside the haircut, we were all looking the same',
    year: '2017'
  },
  {
    _id: 2,
    text: 'Over 50 years after the The Beatles’ ‘White Album’ first stormed the charts, the Fab Four’s iconic double-album achieved 24-time Platinum-certified status – making it the fourth-highest certified release in US history.',
    year: '1920'
  },
  {
    _id: 3,
    text: 'I was relieved to discover the reality is very different to the myth,” continues Jackson, “it’s simply an amazing historical treasure-trove. Sure, there’s moments of drama - but none of the discord this project has long been associated with. Watching John, Paul, George, and Ringo work together, creating now-classic songs from scratch, is not only fascinating - it’s funny, uplifting and surprisingly intimate'
  },
  {
    _id: 4,
    text: "Paul McCartney will make his eagerly anticipated return to the road with his new 'Freshen Up' tour.",
    year: '2020'
  },
  {
    _id: 5,
    text: "John Lennon's most celebrated solo album will be honoured with a number of special releases on October 5 including the six-disc Imagine – The Ultimate Collection."
  },
  {
    _id: 6,
    text: 'The George Harrison estate is happy to announce HariSongs, a new label created in partnership with Craft Recordings to celebrate the Indian classical music George loved and believed would “help as a balance towards a peaceful daily life.”  HariSongs launches today with two reissues in honour of both Ravi Shankar’s birthday (b. 7th April, 1920) and Ali Akbar Khan’s birthday (b. 14th April, 1922) this month.',
    year: '2008'
  },
  {
    _id: 7,
    text: 'The Beatles’ legendary animated hit film YELLOW SUBMARINE is returning to cinemas across the UK, Ireland and the U.S. this summer.'
  },
  {
    _id: 8,
    text: 'Paul has confirmed his first live dates of 2018 headlining the Austin City Limits Music Festival. The festival will take place across the weekends of 5-7th and 12-14th October. Other artists set to appear include Metallica, Childish Gambino, Arctic Monkeys, Travis Scott, Odesza, The National and more.'
  },
  {
    _id: 9,
    text: 'Paul invites you on a musical journey to Egypt Station, estimated time of arrival Friday 7th September 7, 2018 by way of Capitol Records.'
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
  global[indexName].PUT(data).then(t.pass)
})

// TODO: can do SEARCH('paul') (single param is not array)

test('simple SEARCH with 1 clause', t => {
  t.plan(1)
  global[indexName].SEARCH(['paul']).then(res => {
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 0,
          _match: [{ FIELD: 'text', VALUE: 'paul', SCORE: '0.50' }],
          _score: 0.39
        },
        {
          _id: 4,
          _match: [{ FIELD: 'text', VALUE: 'paul', SCORE: '0.50' }],
          _score: 0.39
        },
        {
          _id: 9,
          _match: [{ FIELD: 'text', VALUE: 'paul', SCORE: '0.50' }],
          _score: 0.39
        },
        {
          _id: 3,
          _match: [{ FIELD: 'text', VALUE: 'paul', SCORE: '0.33' }],
          _score: 0.26
        },
        {
          _id: 8,
          _match: [{ FIELD: 'text', VALUE: 'paul', SCORE: '0.25' }],
          _score: 0.2
        }
      ],
      RESULT_LENGTH: 5
    })
  })
})

test('simple _SEARCH with 2 clauses', t => {
  t.plan(1)
  global[indexName].SEARCH(['paul', 'musical']).then(res => {
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 9,
          _match: [
            { FIELD: 'text', VALUE: 'musical', SCORE: '0.50' },
            { FIELD: 'text', VALUE: 'paul', SCORE: '0.50' }
          ],
          _score: 2.4
        }
      ],
      RESULT_LENGTH: 1
    })
  })
})

test('simple _SEARCH with 2 clauses and documents', t => {
  t.plan(1)
  global[indexName]
    .SEARCH(['paul', 'musical'], {
      DOCUMENTS: true
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: 9,
            _match: [
              { FIELD: 'text', VALUE: 'musical', SCORE: '0.50' },
              { FIELD: 'text', VALUE: 'paul', SCORE: '0.50' }
            ],
            _score: 2.4,
            _doc: {
              _id: 9,
              text: 'Paul invites you on a musical journey to Egypt Station, estimated time of arrival Friday 7th September 7, 2018 by way of Capitol Records.'
            }
          }
        ],
        RESULT_LENGTH: 1
      })
    })
})

test('simple _SEARCH with 2 clauses', t => {
  t.plan(1)
  global[indexName].SEARCH(['paul', 'and']).then(res => {
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 0,
          _match: [
            { FIELD: 'text', VALUE: 'and', SCORE: '0.50' },
            { FIELD: 'text', VALUE: 'paul', SCORE: '0.50' }
          ],
          _score: 1.3
        },
        {
          _id: 3,
          _match: [
            { FIELD: 'text', VALUE: 'and', SCORE: '0.67' },
            { FIELD: 'text', VALUE: 'paul', SCORE: '0.33' }
          ],
          _score: 1.3
        },
        {
          _id: 8,
          _match: [
            { FIELD: 'text', VALUE: 'and', SCORE: '0.50' },
            { FIELD: 'text', VALUE: 'paul', SCORE: '0.25' }
          ],
          _score: 0.97
        }
      ],
      RESULT_LENGTH: 3
    })
  })
})

test('SEARCH in all fields', t => {
  t.plan(1)
  global[indexName].SEARCH(['1920']).then(res => {
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 2,
          _match: [{ FIELD: 'year', VALUE: '1920', SCORE: '1.00' }],
          _score: 1.7
        },
        {
          _id: 6,
          _match: [{ FIELD: 'text', VALUE: '1920', SCORE: '0.33' }],
          _score: 0.56
        }
      ],
      RESULT_LENGTH: 2
    })
  })
})
