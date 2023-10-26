import test from 'tape'
import wbd from 'world-bank-dataset'
import { SearchIndex } from 'search-index'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'aggregation-test'
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
      sectorcode: item.sectorcode.split(','),
      board_approval_month: item.board_approval_month,
      impagency: item.impagency,
      majorsector_percent: item.majorsector_percent,
      mjsector_namecode: item.mjsector_namecode,
      sector_namecode: item.sector_namecode,
      totalamt: item.totalamt
    }
  })
  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('can aggregate totalamt using underlying index', t => {
  t.plan(1)
  global[indexName]
    .SEARCH(['board_approval_month:october'], {
      FACETS: [
        {
          FIELD: 'totalamt'
        }
      ]
    })
    .then(result =>
      t.deepEqual(result, {
        RESULT: [
          {
            _id: '52b213b38594d8a2be17c783',
            _match: [
              { FIELD: 'board_approval_month', VALUE: 'october', SCORE: '1.00' }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c784',
            _match: [
              { FIELD: 'board_approval_month', VALUE: 'october', SCORE: '1.00' }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c785',
            _match: [
              { FIELD: 'board_approval_month', VALUE: 'october', SCORE: '1.00' }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c786',
            _match: [
              { FIELD: 'board_approval_month', VALUE: 'october', SCORE: '1.00' }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c787',
            _match: [
              { FIELD: 'board_approval_month', VALUE: 'october', SCORE: '1.00' }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c788',
            _match: [
              { FIELD: 'board_approval_month', VALUE: 'october', SCORE: '1.00' }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c789',
            _match: [
              { FIELD: 'board_approval_month', VALUE: 'october', SCORE: '1.00' }
            ],
            _score: 0.45
          }
        ],
        RESULT_LENGTH: 7,
        FACETS: [
          {
            FIELD: 'totalamt',
            VALUE: 0,
            _id: ['52b213b38594d8a2be17c783', '52b213b38594d8a2be17c787']
          },
          {
            FIELD: 'totalamt',
            VALUE: 10000000,
            _id: ['52b213b38594d8a2be17c785']
          },
          {
            FIELD: 'totalamt',
            VALUE: 13100000,
            _id: ['52b213b38594d8a2be17c784']
          },
          {
            FIELD: 'totalamt',
            VALUE: 160000000,
            _id: ['52b213b38594d8a2be17c788']
          },
          {
            FIELD: 'totalamt',
            VALUE: 200000000,
            _id: ['52b213b38594d8a2be17c789']
          },
          {
            FIELD: 'totalamt',
            VALUE: 500000000,
            _id: ['52b213b38594d8a2be17c786']
          }
        ]
      })
    )
})

test('_BUCKETing', t => {
  t.plan(1)
  global[indexName]
    ._BUCKET({
      FIELD: 'totalamt',
      VALUE: {
        GTE: 0,
        LTE: 7000000
      }
    })
    .then(result =>
      t.deepEqual(result, {
        FIELD: ['totalamt'],
        VALUE: { GTE: 0, LTE: 7000000 },
        _id: [
          '52b213b38594d8a2be17c781',
          '52b213b38594d8a2be17c782',
          '52b213b38594d8a2be17c783',
          '52b213b38594d8a2be17c787'
        ]
      })
    )
})

test('_BUCKETing', t => {
  t.plan(1)
  global[indexName]
    ._BUCKET({
      FIELD: 'totalamt',
      VALUE: {
        GTE: 1,
        LTE: 7000000
      }
    })
    .then(result =>
      t.deepEqual(result, {
        FIELD: ['totalamt'],
        VALUE: { GTE: 1, LTE: 7000000 },
        _id: ['52b213b38594d8a2be17c782']
      })
    )
})

test('_BUCKETing', t => {
  t.plan(1)
  global[indexName]
    ._BUCKET({
      FIELD: 'sectorcode',
      VALUE: {
        GTE: 'b',
        LTE: 'c'
      }
    })
    .then(result =>
      t.deepEqual(result, {
        FIELD: ['sectorcode'],
        VALUE: { GTE: 'b', LTE: 'c' },
        _id: [
          '52b213b38594d8a2be17c780',
          '52b213b38594d8a2be17c781',
          '52b213b38594d8a2be17c789'
        ]
      })
    )
})

test('can aggregate totalamt using custom buckets', t => {
  t.plan(1)
  const b = [
    {
      FIELD: 'totalamt',
      VALUE: { GTE: 0, LTE: 0 }
    },
    {
      FIELD: 'totalamt',
      VALUE: { GTE: 10000000, LTE: 10000000 }
    },
    {
      FIELD: 'totalamt',
      VALUE: { GTE: 200000000, LTE: 200000000 }
    }
  ]

  global[indexName]
    .SEARCH(['board_approval_month:october'], {
      BUCKETS: b
    })
    .then(result =>
      t.deepEqual(result, {
        BUCKETS: [
          {
            FIELD: ['totalamt'],
            VALUE: { GTE: 0, LTE: 0 },
            _id: ['52b213b38594d8a2be17c783', '52b213b38594d8a2be17c787']
          },
          {
            FIELD: ['totalamt'],
            VALUE: { GTE: 10000000, LTE: 10000000 },
            _id: ['52b213b38594d8a2be17c785']
          },
          {
            FIELD: ['totalamt'],
            VALUE: { GTE: 200000000, LTE: 200000000 },
            _id: ['52b213b38594d8a2be17c789']
          }
        ],
        RESULT_LENGTH: 7,
        RESULT: [
          {
            _id: '52b213b38594d8a2be17c783',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c784',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c785',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c786',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c787',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c788',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c789',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          }
        ]
      })
    )
})

test('make some _BUCKETs', t => {
  t.plan(1)
  const b = Promise.all(
    [
      {
        FIELD: 'totalamt',
        VALUE: { GTE: 0, LTE: 0 }
      },
      {
        FIELD: 'totalamt',
        VALUE: { GTE: 10000000, LTE: 10000000 }
      },
      {
        FIELD: 'totalamt',
        VALUE: { GTE: 200000000, LTE: 200000000 }
      }
      // ].map(global[indexName]._BUCKET)
    ].map(res => global[indexName]._BUCKET(res))
  )
  b.then(result =>
    t.deepEqual(result, [
      {
        FIELD: ['totalamt'],
        VALUE: { GTE: 0, LTE: 0 },
        _id: [
          '52b213b38594d8a2be17c781',
          '52b213b38594d8a2be17c783',
          '52b213b38594d8a2be17c787'
        ]
      },
      {
        FIELD: ['totalamt'],
        VALUE: { GTE: 10000000, LTE: 10000000 },
        _id: ['52b213b38594d8a2be17c785']
      },
      {
        FIELD: ['totalamt'],
        VALUE: { GTE: 200000000, LTE: 200000000 },
        _id: ['52b213b38594d8a2be17c789']
      }
    ])
  )
})

test('can aggregate totalamt', t => {
  t.plan(1)
  global[indexName]
    .DISTINCT({
      FIELD: 'impagency'
    })
    .then(result =>
      t.deepEqual(result, [
        { FIELD: 'impagency', VALUE: 'administration' },
        { FIELD: 'impagency', VALUE: 'and' },
        { FIELD: 'impagency', VALUE: 'communications' },
        { FIELD: 'impagency', VALUE: 'departmant' },
        { FIELD: 'impagency', VALUE: 'education' },
        { FIELD: 'impagency', VALUE: 'energy' },
        { FIELD: 'impagency', VALUE: 'finance' },
        { FIELD: 'impagency', VALUE: 'highways' },
        { FIELD: 'impagency', VALUE: 'industry' },
        { FIELD: 'impagency', VALUE: 'intensive' },
        { FIELD: 'impagency', VALUE: 'labor' },
        { FIELD: 'impagency', VALUE: 'ministry' },
        { FIELD: 'impagency', VALUE: 'national' },
        { FIELD: 'impagency', VALUE: 'of' },
        { FIELD: 'impagency', VALUE: 'pmu' },
        { FIELD: 'impagency', VALUE: 'project' },
        { FIELD: 'impagency', VALUE: 'public' },
        { FIELD: 'impagency', VALUE: 'rajasthan' },
        { FIELD: 'impagency', VALUE: 'road' },
        { FIELD: 'impagency', VALUE: 'trade' },
        { FIELD: 'impagency', VALUE: 'transport' },
        { FIELD: 'impagency', VALUE: 'works' }
      ])
    )
})

test('can aggregate totalamt JSON DISTINCT', t => {
  t.plan(1)
  global[indexName]
    .DISTINCT({
      FIELD: 'impagency'
    })
    .then(result =>
      t.deepEqual(result, [
        { FIELD: 'impagency', VALUE: 'administration' },
        { FIELD: 'impagency', VALUE: 'and' },
        { FIELD: 'impagency', VALUE: 'communications' },
        { FIELD: 'impagency', VALUE: 'departmant' },
        { FIELD: 'impagency', VALUE: 'education' },
        { FIELD: 'impagency', VALUE: 'energy' },
        { FIELD: 'impagency', VALUE: 'finance' },
        { FIELD: 'impagency', VALUE: 'highways' },
        { FIELD: 'impagency', VALUE: 'industry' },
        { FIELD: 'impagency', VALUE: 'intensive' },
        { FIELD: 'impagency', VALUE: 'labor' },
        { FIELD: 'impagency', VALUE: 'ministry' },
        { FIELD: 'impagency', VALUE: 'national' },
        { FIELD: 'impagency', VALUE: 'of' },
        { FIELD: 'impagency', VALUE: 'pmu' },
        { FIELD: 'impagency', VALUE: 'project' },
        { FIELD: 'impagency', VALUE: 'public' },
        { FIELD: 'impagency', VALUE: 'rajasthan' },
        { FIELD: 'impagency', VALUE: 'road' },
        { FIELD: 'impagency', VALUE: 'trade' },
        { FIELD: 'impagency', VALUE: 'transport' },
        { FIELD: 'impagency', VALUE: 'works' }
      ])
    )
})

test('can aggregate totalamt', t => {
  t.plan(1)
  global[indexName]
    .DISTINCT({
      FIELD: 'impagency'
    })
    .then(result =>
      Promise.all(result.map(res => global[indexName]._BUCKET(res)))
    )
    .then(result => {
      t.deepEqual(result, [
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'administration', LTE: 'administration' },
          _id: ['52b213b38594d8a2be17c787']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'and', LTE: 'and' },
          _id: [
            '52b213b38594d8a2be17c782',
            '52b213b38594d8a2be17c784',
            '52b213b38594d8a2be17c786'
          ]
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'communications', LTE: 'communications' },
          _id: ['52b213b38594d8a2be17c782']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'departmant', LTE: 'departmant' },
          _id: ['52b213b38594d8a2be17c788']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'education', LTE: 'education' },
          _id: ['52b213b38594d8a2be17c780']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'energy', LTE: 'energy' },
          _id: ['52b213b38594d8a2be17c787']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'finance', LTE: 'finance' },
          _id: ['52b213b38594d8a2be17c781', '52b213b38594d8a2be17c789']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'highways', LTE: 'highways' },
          _id: ['52b213b38594d8a2be17c786']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'industry', LTE: 'industry' },
          _id: ['52b213b38594d8a2be17c784']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'intensive', LTE: 'intensive' },
          _id: ['52b213b38594d8a2be17c783']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'labor', LTE: 'labor' },
          _id: ['52b213b38594d8a2be17c783']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'ministry', LTE: 'ministry' },
          _id: [
            '52b213b38594d8a2be17c780',
            '52b213b38594d8a2be17c781',
            '52b213b38594d8a2be17c782',
            '52b213b38594d8a2be17c784',
            '52b213b38594d8a2be17c786',
            '52b213b38594d8a2be17c789'
          ]
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'national', LTE: 'national' },
          _id: ['52b213b38594d8a2be17c787']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'of', LTE: 'of' },
          _id: [
            '52b213b38594d8a2be17c780',
            '52b213b38594d8a2be17c781',
            '52b213b38594d8a2be17c782',
            '52b213b38594d8a2be17c784',
            '52b213b38594d8a2be17c786',
            '52b213b38594d8a2be17c789'
          ]
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'pmu', LTE: 'pmu' },
          _id: ['52b213b38594d8a2be17c783']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'project', LTE: 'project' },
          _id: ['52b213b38594d8a2be17c783']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'public', LTE: 'public' },
          _id: ['52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'rajasthan', LTE: 'rajasthan' },
          _id: ['52b213b38594d8a2be17c788']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'road', LTE: 'road' },
          _id: ['52b213b38594d8a2be17c786']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'trade', LTE: 'trade' },
          _id: ['52b213b38594d8a2be17c784']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'transport', LTE: 'transport' },
          _id: ['52b213b38594d8a2be17c782', '52b213b38594d8a2be17c786']
        },
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'works', LTE: 'works' },
          _id: ['52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788']
        }
      ])
    })
})

test('can run a query and create facets on impagency', t => {
  t.plan(1)
  global[indexName]
    .SEARCH(['board_approval_month:october'], {
      FACETS: [
        {
          FIELD: 'impagency'
        }
      ]
    })
    .then(result => {
      t.deepEqual(result, {
        FACETS: [
          {
            FIELD: 'impagency',
            VALUE: 'administration',
            _id: ['52b213b38594d8a2be17c787']
          },
          {
            FIELD: 'impagency',
            VALUE: 'and',
            _id: ['52b213b38594d8a2be17c784', '52b213b38594d8a2be17c786']
          },
          {
            FIELD: 'impagency',
            VALUE: 'departmant',
            _id: ['52b213b38594d8a2be17c788']
          },
          {
            FIELD: 'impagency',
            VALUE: 'energy',
            _id: ['52b213b38594d8a2be17c787']
          },
          {
            FIELD: 'impagency',
            VALUE: 'finance',
            _id: ['52b213b38594d8a2be17c789']
          },
          {
            FIELD: 'impagency',
            VALUE: 'highways',
            _id: ['52b213b38594d8a2be17c786']
          },
          {
            FIELD: 'impagency',
            VALUE: 'industry',
            _id: ['52b213b38594d8a2be17c784']
          },
          {
            FIELD: 'impagency',
            VALUE: 'intensive',
            _id: ['52b213b38594d8a2be17c783']
          },
          {
            FIELD: 'impagency',
            VALUE: 'labor',
            _id: ['52b213b38594d8a2be17c783']
          },
          {
            FIELD: 'impagency',
            VALUE: 'ministry',
            _id: [
              '52b213b38594d8a2be17c784',
              '52b213b38594d8a2be17c786',
              '52b213b38594d8a2be17c789'
            ]
          },
          {
            FIELD: 'impagency',
            VALUE: 'national',
            _id: ['52b213b38594d8a2be17c787']
          },
          {
            FIELD: 'impagency',
            VALUE: 'of',
            _id: [
              '52b213b38594d8a2be17c784',
              '52b213b38594d8a2be17c786',
              '52b213b38594d8a2be17c789'
            ]
          },
          {
            FIELD: 'impagency',
            VALUE: 'pmu',
            _id: ['52b213b38594d8a2be17c783']
          },
          {
            FIELD: 'impagency',
            VALUE: 'project',
            _id: ['52b213b38594d8a2be17c783']
          },
          {
            FIELD: 'impagency',
            VALUE: 'public',
            _id: ['52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788']
          },
          {
            FIELD: 'impagency',
            VALUE: 'rajasthan',
            _id: ['52b213b38594d8a2be17c788']
          },
          {
            FIELD: 'impagency',
            VALUE: 'road',
            _id: ['52b213b38594d8a2be17c786']
          },
          {
            FIELD: 'impagency',
            VALUE: 'trade',
            _id: ['52b213b38594d8a2be17c784']
          },
          {
            FIELD: 'impagency',
            VALUE: 'transport',
            _id: ['52b213b38594d8a2be17c786']
          },
          {
            FIELD: 'impagency',
            VALUE: 'works',
            _id: ['52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788']
          }
        ],
        RESULT: [
          {
            _id: '52b213b38594d8a2be17c783',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c784',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c785',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c786',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c787',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c788',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c789',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          }
        ],
        RESULT_LENGTH: 7
      })
    })
})

test('JSON BUCKET', t => {
  t.plan(1)
  global[indexName]
    .BUCKETS({
      FIELD: 'impagency',
      VALUE: 'of'
    })
    .then(result => {
      t.deepEqual(result, [
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'of', LTE: 'of' },
          _id: [
            '52b213b38594d8a2be17c780',
            '52b213b38594d8a2be17c781',
            '52b213b38594d8a2be17c782',
            '52b213b38594d8a2be17c784',
            '52b213b38594d8a2be17c786',
            '52b213b38594d8a2be17c789'
          ]
        }
      ])
    })
})

test('JSON AGGREGATE', t => {
  t.plan(1)
  global[indexName]
    .SEARCH(['board_approval_month:october'], {
      BUCKETS: [
        {
          FIELD: 'impagency',
          VALUE: 'of'
        }
      ]
    })
    .then(result => {
      t.deepEqual(result, {
        BUCKETS: [
          {
            FIELD: ['impagency'],
            VALUE: { GTE: 'of', LTE: 'of' },
            _id: [
              '52b213b38594d8a2be17c784',
              '52b213b38594d8a2be17c786',
              '52b213b38594d8a2be17c789'
            ]
          }
        ],
        RESULT: [
          {
            _id: '52b213b38594d8a2be17c783',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c784',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c785',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c786',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c787',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c788',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          },
          {
            _id: '52b213b38594d8a2be17c789',
            _match: [
              {
                FIELD: 'board_approval_month',
                VALUE: 'october',
                SCORE: '1.00'
              }
            ],
            _score: 0.45
          }
        ],
        RESULT_LENGTH: 7
      })
    })
})

test('can aggregate totalamt using underlying index', t => {
  t.plan(1)
  global[indexName]
    ._BUCKET({
      FIELD: 'impagency',
      VALUE: 'pmu'
    })
    .then(result => {
      t.deepEqual(result, {
        FIELD: ['impagency'],
        VALUE: { GTE: 'pmu', LTE: 'pmu' },
        _id: ['52b213b38594d8a2be17c783']
      })
    })
})

test('can aggregate totalamt using underlying index (JSON BUCKET)', t => {
  t.plan(1)
  global[indexName]
    .BUCKETS({
      FIELD: 'impagency',
      VALUE: 'pmu'
    })
    .then(result => {
      t.deepEqual(result, [
        {
          FIELD: ['impagency'],
          VALUE: { GTE: 'pmu', LTE: 'pmu' },
          _id: ['52b213b38594d8a2be17c783']
        }
      ])
    })
})

test('can aggregate totalamt using underlying index', t => {
  t.plan(1)
  global[indexName]
    ._BUCKET({
      FIELD: 'impagency',
      VALUE: {
        GTE: 'p',
        LTE: 'txx'
      }
    })
    .then(result => {
      t.deepEqual(result, {
        FIELD: ['impagency'],
        VALUE: { GTE: 'p', LTE: 'txx' },
        _id: [
          '52b213b38594d8a2be17c782',
          '52b213b38594d8a2be17c783',
          '52b213b38594d8a2be17c784',
          '52b213b38594d8a2be17c786',
          '52b213b38594d8a2be17c788'
        ]
      })
    })
})

test('can aggregate totalamt using underlying index', t => {
  t.plan(1)
  Promise.all([
    global[indexName]._BUCKET({
      FIELD: 'totalamt',
      VALUE: {
        LTE: 10000000
      }
    }),
    global[indexName]._BUCKET({
      FIELD: 'totalamt',
      VALUE: {
        GTE: 10000001
      }
    })
  ]).then(result => {
    t.deepEqual(result, [
      {
        FIELD: ['totalamt'],
        VALUE: { GTE: null, LTE: 10000000 },
        _id: [
          '52b213b38594d8a2be17c781',
          '52b213b38594d8a2be17c782',
          '52b213b38594d8a2be17c783',
          '52b213b38594d8a2be17c785',
          '52b213b38594d8a2be17c787'
        ]
      },
      {
        FIELD: ['totalamt'],
        VALUE: { GTE: 10000001, LTE: undefined },
        _id: [
          '52b213b38594d8a2be17c780',
          '52b213b38594d8a2be17c784',
          '52b213b38594d8a2be17c786',
          '52b213b38594d8a2be17c788',
          '52b213b38594d8a2be17c789'
        ]
      }
    ])
  })
})
