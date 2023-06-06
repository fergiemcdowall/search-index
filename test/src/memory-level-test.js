const { MemoryLevel } = require('memory-level')
const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'memdown-test'

const data = [
  {
    _id: 'a',
    title: 'quite a cool document',
    body: {
      text: 'this document is really cool cool cool',
      metadata: 'coolness documentness'
    },
    importantNumber: 5000
  },
  {
    _id: 'b',
    title: 'quite a cool document',
    body: {
      text: 'this document is really cool bananas',
      metadata: 'coolness documentness'
    },
    importantNumber: 500
  },
  {
    _id: 'c',
    title: 'something different',
    body: {
      text: 'something totally different',
      metadata: 'coolness documentness'
    },
    importantNumber: 200
  }
]

test('create a search-index with memory-level', t => {
  t.plan(2)
  si({
    db: new MemoryLevel(),
    name: indexName
  })
    .then(idx =>
      idx.PUT(data).then(res => {
        t.deepEqual(res, [
          { _id: 'a', status: 'CREATED', operation: 'PUT' },
          { _id: 'b', status: 'CREATED', operation: 'PUT' },
          { _id: 'c', status: 'CREATED', operation: 'PUT' }
        ])
        return idx
      })
    )
    .then(idx => {
      idx
        .SEARCH(['body.text:cool', 'body.text:really', 'body.text:bananas'])
        .then(res =>
          t.deepEquals(res, {
            RESULT: [
              {
                _id: 'b',
                _match: [
                  { FIELD: 'body.text', VALUE: 'bananas', SCORE: '1.00' },
                  { FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' },
                  { FIELD: 'body.text', VALUE: 'really', SCORE: '1.00' }
                ],
                _score: 4.16
              }
            ],
            RESULT_LENGTH: 1
          })
        )
    })
})
