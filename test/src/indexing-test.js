import test from 'tape'
import { SearchIndex } from 'search-index'

const global = {}
const sandbox = 'test/sandbox/'
const indexName = sandbox + 'indexing-test'

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

test('create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({ name: indexName })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add some data', t => {
  t.plan(1)
  global[indexName].PUT(data).then(() => {
    t.pass('ok')
  })
})

// should be able to get non-tokenised (readable) version of object out of index
test('can search', t => {
  t.plan(1)
  global[indexName]
    .SEARCH(['body.text:cool', 'body.text:really', 'body.text:bananas'])
    .then(res => {
      t.deepEqual(res, {
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
    })
})

// should be able to get non-tokenised (readable) version of object out of index
test('can search with QUERY', t => {
  t.plan(1)
  global[indexName]
    .QUERY(
      {
        AND: ['body.text:cool', 'body.text:really', 'body.text:bananas']
      },
      {
        SCORE: 'TFIDF'
      }
    )
    .then(res => {
      t.deepEqual(res, {
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
    })
})

test('can search in any field', t => {
  t.plan(1)
  global[indexName].SEARCH(['cool', 'really', 'bananas']).then(res => {
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 'b',
          _match: [
            { FIELD: 'body.text', VALUE: 'bananas', SCORE: '1.00' },
            { FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' },
            { FIELD: 'body.text', VALUE: 'really', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'cool', SCORE: '1.00' }
          ],
          _score: 5.55
        }
      ],
      RESULT_LENGTH: 1
    })
  })
})

test('can do 0-hit', t => {
  t.plan(1)
  global[indexName]
    .SEARCH(['cool', 'really', 'sdasdadsasd', 'bananas'])
    .then(res => {
      t.deepEqual(res, { RESULT: [], RESULT_LENGTH: 0 })
    })
})

test('can do a mixture of fielded search and any-field search', t => {
  t.plan(1)
  global[indexName].SEARCH(['title:cool', 'documentness']).then(res => {
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 'a',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'documentness', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'cool', SCORE: '1.00' }
          ],
          _score: 1.39
        },
        {
          _id: 'b',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'documentness', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'cool', SCORE: '1.00' }
          ],
          _score: 1.39
        }
      ],
      RESULT_LENGTH: 2
    })
  })
})

test('can _SEARCH by numeric value (and return DOCUMENT)', t => {
  t.plan(1)
  global[indexName]
    .SEARCH([500], {
      DOCUMENTS: true
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: 'b',
            _match: [{ FIELD: 'importantnumber', VALUE: 500, SCORE: 500 }],
            _score: 693.15,
            _doc: {
              _id: 'b',
              title: 'quite a cool document',
              body: {
                text: 'this document is really cool bananas',
                metadata: 'coolness documentness'
              },
              importantNumber: 500
            }
          }
        ],
        RESULT_LENGTH: 1
      })
    })
})

test('can _OR by numeric value and _SORT by numeric value', t => {
  t.plan(1)
  global[indexName]
    ._OR([500, 200])
    .then(resultSet =>
      global[indexName]._SORT(resultSet, {
        FIELD: '_match.importantnumber',
        TYPE: 'NUMERIC',
        DIRECTION: 'ASCENDING'
      })
    )
    .then(res => {
      t.deepEqual(res, [
        {
          _id: 'b',
          _match: [{ FIELD: 'importantnumber', VALUE: 500, SCORE: 500 }]
        },
        {
          _id: 'c',
          _match: [{ FIELD: 'importantnumber', VALUE: 200, SCORE: 200 }]
        }
      ])
    })
})

// _OR-ing
test('can search by numeric value and _OR with one term on any field', t => {
  t.plan(1)
  global[indexName]
    ._OR([200, { FIELD: 'importantnumber', VALUE: 5000 }])
    .then(res =>
      t.deepEqual(res, [
        {
          _id: 'c',
          _match: [{ FIELD: 'importantnumber', VALUE: 200, SCORE: 200 }]
        },
        {
          _id: 'a',
          _match: [{ FIELD: 'importantnumber', VALUE: 5000, SCORE: 5000 }]
        }
      ])
    )
})

test('can _GET', t => {
  t.plan(1)
  global[indexName]._GET('body.text:cool').then(res =>
    t.deepEqual(res, [
      {
        _id: 'a',
        _match: [{ FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' }]
      },
      {
        _id: 'b',
        _match: [{ FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' }]
      }
    ])
  )
})

test('can _GET with no field specified', t => {
  t.plan(1)
  global[indexName]._GET('cool').then(res => {
    t.deepEqual(res, [
      {
        _id: 'a',
        _match: [
          { FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' },
          { FIELD: 'title', VALUE: 'cool', SCORE: '1.00' }
        ]
      },
      {
        _id: 'b',
        _match: [
          { FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' },
          { FIELD: 'title', VALUE: 'cool', SCORE: '1.00' }
        ]
      }
    ])
  })
})

test('can _AND', t => {
  t.plan(1)
  global[indexName]
    ._AND(['body.text:really', 'body.metadata:coolness'])
    .then(res => {
      t.deepEqual(res, [
        {
          _id: 'a',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
            { FIELD: 'body.text', VALUE: 'really', SCORE: '0.33' }
          ]
        },
        {
          _id: 'b',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
            { FIELD: 'body.text', VALUE: 'really', SCORE: '1.00' }
          ]
        }
      ])
    })
})

test('can _AND with embedded _OR', t => {
  t.plan(1)
  global[indexName]
    ._AND([
      global[indexName]._OR(['title:quite', 'body.text:different']),
      'body.metadata:coolness'
    ])
    .then(res => {
      t.deepEqual(res, [
        {
          _id: 'a',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'quite', SCORE: '1.00' }
          ]
        },
        {
          _id: 'b',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'quite', SCORE: '1.00' }
          ]
        },
        {
          _id: 'c',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
            { FIELD: 'body.text', VALUE: 'different', SCORE: '1.00' }
          ]
        }
      ])
    })
})

test('can _AND with embedded _OR and embedded _AND', t => {
  t.plan(1)
  global[indexName]
    ._AND([
      global[indexName]._OR([
        'title:quite',
        global[indexName]._AND(['body.text:totally', 'body.text:different'])
      ]),
      'body.metadata:coolness'
    ])
    .then(res => {
      t.deepEqual(res, [
        {
          _id: 'a',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'quite', SCORE: '1.00' }
          ]
        },
        {
          _id: 'b',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'quite', SCORE: '1.00' }
          ]
        },
        {
          _id: 'c',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
            { FIELD: 'body.text', VALUE: 'different', SCORE: '1.00' },
            { FIELD: 'body.text', VALUE: 'totally', SCORE: '1.00' }
          ]
        }
      ])
    })
})

test('can _NOT', t => {
  t.plan(1)
  global[indexName]._NOT('cool', 'bananas').then(res => {
    t.deepEqual(res, [
      {
        _id: 'a',
        _match: [
          { FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' },
          { FIELD: 'title', VALUE: 'cool', SCORE: '1.00' }
        ]
      }
    ])
  })
})

test('can _OR', t => {
  t.plan(1)
  global[indexName]
    ._OR(['body.text:bananas', 'body.text:different'])
    .then(res => {
      t.deepEqual(res, [
        {
          _id: 'b',
          _match: [{ FIELD: 'body.text', VALUE: 'bananas', SCORE: '1.00' }]
        },
        {
          _id: 'c',
          _match: [{ FIELD: 'body.text', VALUE: 'different', SCORE: '1.00' }]
        }
      ])
    })
})

test('_AND with embedded _OR', t => {
  t.plan(1)
  global[indexName]
    ._AND([
      'bananas',
      global[indexName]._OR(['body.text:cool', 'body.text:coolness'])
    ])
    .then(res => {
      t.deepEqual(res, [
        {
          _id: 'b',
          _match: [
            { FIELD: 'body.text', VALUE: 'bananas', SCORE: '1.00' },
            { FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' }
          ]
        }
      ])
    })
})

test('AND with embedded OR (JSON API)', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      AND: ['bananas']
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: 'b',
            _match: [{ FIELD: 'body.text', VALUE: 'bananas', SCORE: '1.00' }]
          }
        ],
        RESULT_LENGTH: 1
      })
    })
})

test('AND with embedded OR (JSON API)', t => {
  t.plan(1)
  global[indexName]
    .QUERY({
      AND: ['bananas', { OR: ['body.text:cool', 'body.text:coolness'] }]
    })
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: 'b',
            _match: [
              { FIELD: 'body.text', VALUE: 'bananas', SCORE: '1.00' },
              { FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' }
            ]
          }
        ],
        RESULT_LENGTH: 1
      })
    })
})

// This is not currently supported, but should it be? ->
// test('DOCUMENT (JSON API)', t => {
//   t.plan(1)
//   global[indexName]
//     .QUERY({
//       DOCUMENTS: ['b', 'a']
//     })
//     .then(res => {
//       t.deepEqual(res, {
//         RESULT: [
//           {
//             _id: 'b',
//             title: 'quite a cool document',
//             body: {
//               text: 'this document is really cool bananas',
//               metadata: 'coolness documentness'
//             },
//             importantNumber: 500
//           },
//           {
//             _id: 'a',
//             title: 'quite a cool document',
//             body: {
//               text: 'this document is really cool cool cool',
//               metadata: 'coolness documentness'
//             },
//             importantNumber: 5000
//           }
//         ],
//         RESULT_LENGTH: 2
//       })
//     })
// })

// TODO: I think DOCUMENT should behave differently here
// this should be a SEARCH should it not?
test('QUERY with a string and then connect documents', t => {
  t.plan(1)
  global[indexName].QUERY('bananas', { DOCUMENTS: true }).then(res => {
    t.deepEqual(res, {
      RESULT: [
        {
          _id: 'b',
          _match: [{ FIELD: 'body.text', VALUE: 'bananas', SCORE: '1.00' }],
          _doc: {
            _id: 'b',
            title: 'quite a cool document',
            body: {
              text: 'this document is really cool bananas',
              metadata: 'coolness documentness'
            },
            importantNumber: 500
          }
        }
      ],
      RESULT_LENGTH: 1
    })
  })
})

test('_AND with embedded _OR', t => {
  t.plan(1)
  global[indexName]
    ._AND([
      global[indexName]._OR(['bananas', 'different']),
      global[indexName]._OR(['cool', 'coolness'])
    ])
    .then(res => {
      t.deepEqual(res, [
        {
          _id: 'b',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
            { FIELD: 'body.text', VALUE: 'bananas', SCORE: '1.00' },
            { FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'cool', SCORE: '1.00' }
          ]
        },
        {
          _id: 'c',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
            { FIELD: 'body.text', VALUE: 'different', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'different', SCORE: '1.00' }
          ]
        }
      ])
    })
})

test('can _GET range with one value', t => {
  t.plan(1)
  global[indexName]
    ._GET({
      VALUE: {
        GTE: 'cool',
        LTE: 'cool'
      }
    })
    .then(res =>
      t.deepEqual(res, [
        {
          _id: 'a',
          _match: [
            { FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'cool', SCORE: '1.00' }
          ]
        },
        {
          _id: 'b',
          _match: [
            { FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'cool', SCORE: '1.00' }
          ]
        }
      ])
    )
})

test('can _GET range with a range of values', t => {
  t.plan(1)
  global[indexName]
    ._GET({
      VALUE: {
        GTE: 'cool',
        LTE: 'coolness'
      }
    })
    .then(res =>
      t.deepEqual(res, [
        {
          _id: 'a',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
            { FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'cool', SCORE: '1.00' }
          ]
        },
        {
          _id: 'b',
          _match: [
            { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
            { FIELD: 'body.text', VALUE: 'cool', SCORE: '1.00' },
            { FIELD: 'title', VALUE: 'cool', SCORE: '1.00' }
          ]
        },
        {
          _id: 'c',
          _match: [{ FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' }]
        }
      ])
    )
})

// TODO: FIX. This test seems to be giving inconsistent results between browser and node
test('_SEARCH with embedded _OR', t => {
  t.plan(1)
  global[indexName]
    .SEARCH([{ OR: ['bananas', 'different'] }, 'coolness'])
    .then(res => {
      t.deepEqual(res, {
        RESULT: [
          {
            _id: 'c',
            _match: [
              { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
              { FIELD: 'body.text', VALUE: 'different', SCORE: '1.00' },
              { FIELD: 'title', VALUE: 'different', SCORE: '1.00' }
            ],
            _score: 2.08
          },
          {
            _id: 'b',
            _match: [
              { FIELD: 'body.metadata', VALUE: 'coolness', SCORE: '1.00' },
              { FIELD: 'body.text', VALUE: 'bananas', SCORE: '1.00' }
            ],
            _score: 1.39
          }
        ],
        RESULT_LENGTH: 2
      })
    })
})

test('DICTIONARY with specified field', t => {
  t.plan(1)
  global[indexName].DICTIONARY('body.text').then(res => {
    global[indexName].DICTIONARY({ FIELD: ['body.text'] }).then(res => {
      t.deepEqual(res, [
        'bananas',
        'cool',
        'different',
        'document',
        'is',
        'really',
        'something',
        'this',
        'totally'
      ])
    })
  })
})

test('DICTIONARY with specified field (JSON API)', t => {
  t.plan(1)
  global[indexName].DICTIONARY({ FIELD: ['body.text'] }).then(res => {
    t.deepEqual(res, [
      'bananas',
      'cool',
      'different',
      'document',
      'is',
      'really',
      'something',
      'this',
      'totally'
    ])
  })
})

test('DICTIONARY with gte lte', t => {
  t.plan(1)
  global[indexName]
    .DICTIONARY({
      FIELD: ['body.text'],
      VALUE: {
        GTE: 'd',
        LTE: 'r'
      }
    })
    .then(res => {
      t.deepEqual(res, ['different', 'document', 'is', 'really'])
    })
})

test('DICTIONARY without specified field', t => {
  t.plan(1)
  global[indexName].DICTIONARY().then(res => {
    t.deepEqual(res, [
      200,
      500,
      5000,
      'a',
      'bananas',
      'cool',
      'coolness',
      'different',
      'document',
      'documentness',
      'is',
      'quite',
      'really',
      'something',
      'this',
      'totally'
    ])
  })
})

test('DICTIONARY without specified field', t => {
  t.plan(1)
  global[indexName].DICTIONARY().then(res => {
    t.deepEqual(res, [
      200,
      500,
      5000,
      'a',
      'bananas',
      'cool',
      'coolness',
      'different',
      'document',
      'documentness',
      'is',
      'quite',
      'really',
      'something',
      'this',
      'totally'
    ])
  })
})

test('DOCUMENT_COUNT is 3', t => {
  t.plan(1)
  global[indexName].DOCUMENT_COUNT().then(res => {
    t.equal(res, 3)
  })
})
