import { SearchIndex } from '../../src/main.js'
import test from 'tape'

const sandbox = 'test/sandbox/'

const docs = [
  {
    _id: 1,
    bandName: '*',
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

test('set up as per issue #577', async function (t) {
  t.plan(2)

  const { PUT, SEARCH } = await new SearchIndex({
    name: sandbox + '577',
    tokenSplitRegex: /[\p{L}\d*]+/gu
  })

  t.deepEquals(await PUT(docs), [
    { _id: 1, operation: 'PUT', status: 'CREATED' },
    { _id: 'two', operation: 'PUT', status: 'CREATED' },
    { _id: 3, operation: 'PUT', status: 'CREATED' }
  ])

  t.deepEquals(
    await SEARCH(['*'], {
      DOCUMENTS: true
    }),
    {
      RESULT: [
        {
          _id: 1,
          _match: [{ FIELD: 'bandname', VALUE: '*', SCORE: '1.00' }],
          _score: 1.39,
          _doc: {
            _id: 1,
            bandName: '*',
            description: 'The nice boys of pop'
          }
        }
      ],
      RESULT_LENGTH: 1
    }
  )
})

test('set up as per issue #577', async function (t) {
  t.plan(2)

  const { PUT, SEARCH } = await new SearchIndex({
    name: sandbox + '577-2'
  })

  t.deepEquals(
    await PUT(docs, {
      tokenSplitRegex: /[\p{L}\d*]+/gu
    }),
    [
      { _id: 1, operation: 'PUT', status: 'CREATED' },
      { _id: 'two', operation: 'PUT', status: 'CREATED' },
      { _id: 3, operation: 'PUT', status: 'CREATED' }
    ]
  )

  t.deepEquals(
    await SEARCH(['*'], {
      DOCUMENTS: true
    }),
    {
      RESULT: [
        {
          _id: 1,
          _match: [{ FIELD: 'bandname', VALUE: '*', SCORE: '1.00' }],
          _score: 1.39,
          _doc: {
            _id: 1,
            bandName: '*',
            description: 'The nice boys of pop'
          }
        }
      ],
      RESULT_LENGTH: 1
    }
  )
})
