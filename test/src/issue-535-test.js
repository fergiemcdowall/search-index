import test from 'tape'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'

const docs = [
  { _id: 'qwertyu', idx: 'q' },
  { _id: 'asdfgh', idx: 'a' }
]

test('set up as per issue #535', async function (t) {
  t.plan(7)

  const si = new SearchIndex({
    name: sandbox + '535'
  })
  t.ok(si.PUT)

  t.deepEquals(await si.PUT(docs), [
    { _id: 'qwertyu', status: 'CREATED', operation: 'PUT' },
    { _id: 'asdfgh', status: 'CREATED', operation: 'PUT' }
  ])

  t.deepEquals(await si.SEARCH(['q']), {
    RESULT: [
      {
        _id: 'qwertyu',
        _match: [{ FIELD: 'idx', VALUE: 'q', SCORE: '1.00' }],
        _score: 1.1
      }
    ],
    RESULT_LENGTH: 1,
    PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
  })

  t.ok(await si.FLUSH())

  t.deepEquals(await si.SEARCH(['q']), {
    RESULT: [],
    RESULT_LENGTH: 0,
    PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 0, DOC_OFFSET: 0 }
  })

  t.deepEquals(await si.PUT(docs), [
    { _id: 'qwertyu', status: 'CREATED', operation: 'PUT' },
    { _id: 'asdfgh', status: 'CREATED', operation: 'PUT' }
  ])

  t.deepEquals(await si.SEARCH(['q']), {
    RESULT: [
      {
        _id: 'qwertyu',
        _match: [{ FIELD: 'idx', VALUE: 'q', SCORE: '1.00' }],
        _score: 1.1
      }
    ],
    RESULT_LENGTH: 1,
    PAGING: { NUMBER: 0, SIZE: 20, TOTAL: 1, DOC_OFFSET: 0 }
  })
})
