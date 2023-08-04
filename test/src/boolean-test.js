import test from 'tape'
import wbd from 'world-bank-dataset'

const { SearchIndex } = await import(
  '../../src/' + process.env.SI_TEST_ENTRYPOINT
)

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'BOOLEAN'
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

test('can add some worldbank data', t => {
  const dataSize = 10
  const data = wbd.slice(0, dataSize).map(item => {
    return {
      _id: item._id.$oid,
      // TODO: only first element in array is indexed- why?
      sectorcode: item.sectorcode.split(',').join(' ').split(' '),
      board_approval_month: item.board_approval_month,
      impagency: item.impagency,
      majorsector_percent: item.majorsector_percent,
      mjsector_namecode: item.mjsector_namecode,
      sector_namecode: item.sector_namecode,
      totalamt: item.totalamt
    }
  })
  //  console.log(JSON.stringify(data, null, 2))
  t.plan(1)
  global[indexName].PUT(data).then(result => {
    t.pass('ok')
  })
})

test('_AND', t => {
  t.plan(1)
  global[indexName]._AND(['sectorcode:bz', 'sectorcode:bc']).then(res => {
    t.deepEqual(res, [
      {
        _id: '52b213b38594d8a2be17c789',
        _match: [
          { FIELD: 'sectorcode', VALUE: 'bc', SCORE: '1.00' },
          { FIELD: 'sectorcode', VALUE: 'bz', SCORE: '1.00' }
        ]
      }
    ])
  })
})

test('AND', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      AND: ['sectorcode:bz', 'sectorcode:bc']
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '52b213b38594d8a2be17c789',
            _match: [
              { FIELD: 'sectorcode', VALUE: 'bc', SCORE: '1.00' },
              { FIELD: 'sectorcode', VALUE: 'bz', SCORE: '1.00' }
            ]
          }
        ],
        RESULT_LENGTH: 1
      })
    })
})

test('OR', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      OR: ['sectorcode:ti', 'sectorcode:bz']
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '52b213b38594d8a2be17c782',
            _match: [{ FIELD: 'sectorcode', VALUE: 'ti', SCORE: '1.00' }]
          },
          {
            _id: '52b213b38594d8a2be17c786',
            _match: [{ FIELD: 'sectorcode', VALUE: 'ti', SCORE: '1.00' }]
          },
          {
            _id: '52b213b38594d8a2be17c788',
            _match: [{ FIELD: 'sectorcode', VALUE: 'ti', SCORE: '1.00' }]
          },
          {
            _id: '52b213b38594d8a2be17c781',
            _match: [{ FIELD: 'sectorcode', VALUE: 'bz', SCORE: '1.00' }]
          },
          {
            _id: '52b213b38594d8a2be17c789',
            _match: [{ FIELD: 'sectorcode', VALUE: 'bz', SCORE: '1.00' }]
          }
        ],
        RESULT_LENGTH: 5
      })
    })
})

test('OR', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      OR: ['sectorcode:yz', 'sectorcode:bc']
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '52b213b38594d8a2be17c784',
            _match: [{ FIELD: 'sectorcode', VALUE: 'yz', SCORE: '1.00' }]
          },
          {
            _id: '52b213b38594d8a2be17c789',
            _match: [{ FIELD: 'sectorcode', VALUE: 'bc', SCORE: '1.00' }]
          }
        ],
        RESULT_LENGTH: 2
      })
    })
})

test('OR', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      OR: ['sectorcode:yz', 'sectorcode:bc']
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '52b213b38594d8a2be17c784',
            _match: [{ FIELD: 'sectorcode', VALUE: 'yz', SCORE: '1.00' }]
          },
          {
            _id: '52b213b38594d8a2be17c789',
            _match: [{ FIELD: 'sectorcode', VALUE: 'bc', SCORE: '1.00' }]
          }
        ],
        RESULT_LENGTH: 2
      })
    })
})

test('OR', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      OR: ['sectorcode:fh', 'sectorcode:yw', 'sectorcode:yz']
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '52b213b38594d8a2be17c784',
            _match: [
              { FIELD: 'sectorcode', VALUE: 'fh', SCORE: '1.00' },
              { FIELD: 'sectorcode', VALUE: 'yw', SCORE: '1.00' },
              { FIELD: 'sectorcode', VALUE: 'yz', SCORE: '1.00' }
            ]
          }
        ],
        RESULT_LENGTH: 1
      })
    })
})

test('can _NOT', t => {
  t.plan(1)
  global[indexName]
    ._NOT('sectorcode:ti', 'board_approval_month:november')
    .then(res => {
      t.deepEqual(res, [
        {
          _id: '52b213b38594d8a2be17c786',
          _match: [{ FIELD: 'sectorcode', VALUE: 'ti', SCORE: '1.00' }]
        },
        {
          _id: '52b213b38594d8a2be17c788',
          _match: [{ FIELD: 'sectorcode', VALUE: 'ti', SCORE: '1.00' }]
        }
      ])
    })
})

test('can NOT', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      NOT: {
        INCLUDE: 'sectorcode:ti',
        EXCLUDE: 'board_approval_month:november'
      }
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: '52b213b38594d8a2be17c786',
            _match: [{ FIELD: 'sectorcode', VALUE: 'ti', SCORE: '1.00' }]
          },
          {
            _id: '52b213b38594d8a2be17c788',
            _match: [{ FIELD: 'sectorcode', VALUE: 'ti', SCORE: '1.00' }]
          }
        ],
        RESULT_LENGTH: 2
      })
    })
})
