const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'

const docs = [
  {
    _id: 1,
    bandName: 'The Beatles',
    members: {},
    description: 'The nice boys of pop'
  },
  {
    _id: 'two',
    bandName: 'The Rolling Stones',
    description: 'The bad boys of rock'
  },
  {
    _id: 3,
    bandName: 'The Who',
    description: 'Nearly as good as Led Zeppelin'
  }
]

test('set up as per issue #571', async function (t) {
  t.plan(2)

  const { PUT, SEARCH } = await si({
    name: sandbox + '571'
  })

  t.deepEquals(await PUT(docs), [
    { _id: 1, operation: 'PUT', status: 'CREATED' },
    { _id: 'two', operation: 'PUT', status: 'CREATED' },
    { _id: 3, operation: 'PUT', status: 'CREATED' }
  ])

  t.deepEquals(
    await SEARCH(['beatles'], {
      DOCUMENTS: true
    }),
    {
      RESULT: [
        {
          _id: 1,
          _match: [{ FIELD: 'bandname', VALUE: 'beatles', SCORE: '1.00' }],
          _score: 1.39,
          _doc: {
            _id: 1,
            bandName: 'The Beatles',
            members: {},
            description: 'The nice boys of pop'
          }
        }
      ],
      RESULT_LENGTH: 1
    }
  )
})
