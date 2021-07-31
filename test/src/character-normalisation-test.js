const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'

let i = 0

test('create a search index with no character normalisation', async t => {
  const data = ['jeg bør finne en ting', 'jeg bor i Oslo']
  const { PUT, SEARCH } = await si({
    name: sandbox + 'char-norm-' + ++i
  })
  const ids = (await PUT(data)).map(status => status._id)
  t.deepEquals(await SEARCH(['bor']), {
    RESULT: [
      {
        _id: ids[1],
        _match: [{ FIELD: 'body', VALUE: 'bor', SCORE: '1.00' }],
        _score: 1.1
      }
    ],
    RESULT_LENGTH: 1
  })
})

test('create a search index with query side character normalisation', async t => {
  const data = ['jeg bør finne en ting', 'jeg bor i Oslo']
  const { PUT, SEARCH } = await si({
    name: sandbox + 'char-norm-' + ++i
  })
  const ids = (await PUT(data)).map(status => status._id)
  t.deepEquals(await SEARCH(['bor']), {
    RESULT: [
      {
        _id: ids[1],
        _match: [{ FIELD: 'body', VALUE: 'bor', SCORE: '1.00' }],
        _score: 1.1
      }
    ],
    RESULT_LENGTH: 1
  })
})

test('create a search index with query and index side character normalisation', async t => {
  const data = ['jeg bør finne en ting', 'jeg bor i Oslo']
  const { PUT, SEARCH } = await si({
    name: sandbox + 'char-norm-' + ++i
  })
  const ids = (await PUT(data)).map(status => status._id)
  t.deepEquals(await SEARCH(['bor']), {
    RESULT: [
      {
        _id: ids[1],
        _match: [{ FIELD: 'body', VALUE: 'bor', SCORE: '1.00' }],
        _score: 1.1
      }
    ],
    RESULT_LENGTH: 2
  })
})
