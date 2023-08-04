import test from 'tape'

const { SearchIndex } = await import(
  '../../src/' + process.env.SI_TEST_ENTRYPOINT
)

const sandbox = 'test/sandbox/'

test('can concurrently PUT', async function (t) {
  t.plan(3)

  const size = 10

  const docs = new Array(size)
    .fill()
    .map((doc, i) => ({ text: 'this is some test text', _id: i }))

  const { PUT, QUERY } = await new SearchIndex({
    name: sandbox + 'concurrency'
  })
  t.ok(PUT)

  t.deepEquals(
    await Promise.all(docs.map(doc => PUT([doc]))),
    new Array(size)
      .fill()
      .map((res, i) => [{ _id: i, operation: 'PUT', status: 'CREATED' }])
  )

  t.deepEquals(
    await QUERY({
      AND: ['this']
    }),
    {
      RESULT: new Array(size).fill().map((item, i) => ({
        _id: i,
        _match: [{ FIELD: 'text', VALUE: 'this', SCORE: '1.00' }]
      })),
      RESULT_LENGTH: size
    }
  )
})
