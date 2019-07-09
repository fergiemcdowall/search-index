/* global si */
import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'

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

test('create a search index', t => {
  t.plan(1)
  si({
    name: sandbox + 'simple-test'
  }, (err, idx) => {
    global.idx = idx
    t.error(err)
  })
})

test('can add some worldbank data', t => {
  t.plan(1)
  idx.PUT(data).then(() => {
    t.pass('ok')
  })
})

// should be able to get non-tokenised (readable) version of object out of index
test('can search', t => {
  t.plan(1)
  idx.SEARCH(
    'body.text:cool', // use colon? eg "body.text:cool"
    'body.text:really',
    'body.text:bananas'
  ).then(res => {
    t.looseEqual(res, [
      {
        '_id': 'b',
        // how about "match"
        'match': [
          'body.text.cool:0.17',
          'body.text.really:0.17',
          'body.text.bananas:0.17'
        ],
        'score': 0.71,
        'obj': data[1]
      }
    ])
  })
})

test('can search in any field', t => {
  t.plan(1)
  idx.SEARCH(
    'cool',
    'really',
    'bananas'
  ).then(res => {
    t.looseEqual(res, [
      { _id: 'b',
        match: [
          'body.text.cool:0.17',
          'title.cool:0.25',
          'body.text.really:0.17',
          'body.text.bananas:0.17'
        ],
        score: 1.05,
        obj: data[1]
      }
    ])
  })
})

test('can do 0-hit', t => {
  t.plan(1)
  idx.SEARCH(
    'cool',
    'really',
    'sdasdadsasd',
    'bananas'
  ).then(res => {
    t.looseEqual(res, [])
  })
})

test('can do a mixture of fielded search and any-field search', t => {
  t.plan(1)
  idx.SEARCH(
    'title:cool',
    'body.metadata:documentness'
  ).then(res => {
    t.looseEqual(res, [
      {
        '_id': 'a',
        'match': [
          'title.cool:0.25',
          'body.metadata.documentness:0.50'
        ],
        'score': 0.52,
        'obj': data[0]
      },
      {
        '_id': 'b',
        'match': [
          'title.cool:0.25',
          'body.metadata.documentness:0.50'
        ],
        'score': 0.52,
        'obj': data[1]
      }
    ])
  })
})

test('can search by numeric value', t => {
  t.plan(1)
  idx.AND(
    'importantNumber:*'
  ).then(
    resultSet => idx.SCORENUMERIC({
      resultSet: resultSet,
      fieldName: 'importantNumber',
      sort: (a, b) => b.score - a.score,
      offset: 0,
      limit: 10
    })
  ).then(idx.DOCUMENTS)
   .then(res => {
     t.looseEqual(res, [
       { _id: 'a',
         match: [ 'importantNumber.5000:5000' ],
         score: 5000,
         obj: data[0]
       },
       { _id: 'b',
         match: [ 'importantNumber.500:500' ],
         score: 500,
         obj: data[1]
       },
       { _id: 'c',
         match: [ 'importantNumber.200:200' ],
         score: 200,
         obj: data[2]
       }
     ])
   })
})

// OR-ing
test('can search by numeric value and OR', t => {
  t.plan(1)
  idx.OR(
    'importantNumber:200',
    'importantNumber:5000'
  ).then(res => t.looseEqual(res, [
    {
      _id: 'c',
      match: [ 'importantNumber.200:200' ]
    },
    {
      _id: 'a',
      match: [ 'importantNumber.5000:5000' ]
    }
  ]))
})

// OR-ing
test('can search by numeric value and OR with one term on any field', t => {
  t.plan(1)
  idx.OR(
    '200',
    'importantNumber:5000'
  ).then(res => t.looseEqual(res, [
    {
      _id: 'c',
      match: [ 'importantNumber.200:200' ]
    },
    {
      _id: 'a',
      match: [ 'importantNumber.5000:5000' ]
    }
  ]))
})

test('can GET', t => {
  t.plan(1)
  idx.GET(
    'body.text:cool'
  ).then(res => t.looseEqual(res, [
    {
      _id: 'b', match: [ 'body.text.cool:0.17' ]
    },
    {
      _id: 'a', match: [ 'body.text.cool:0.60' ]
    }
  ]))
})

test('can GET with no field specified', t => {
  t.plan(1)
  idx.GET(
    'cool'
  ).then(res => {
    t.looseEqual(res, [
      {
        '_id': 'b',
        'match': [
          'body.text.cool:0.17',
          'title.cool:0.25'
        ]
      },
      {
        '_id': 'a',
        'match': [
          'body.text.cool:0.60',
          'title.cool:0.25'
        ]
      }
    ])
  })
})

test('can AND', t => {
  t.plan(1)
  idx.AND(
    'title:quite',
    'body.metadata:coolness'
  ).then(res => {
    t.looseEqual(res, [
      {
        '_id': 'a',
        'match': [
          'title.quite:0.25',
          'body.metadata.coolness:0.50'
        ]
      },
      {
        '_id': 'b',
        'match': [
          'title.quite:0.25',
          'body.metadata.coolness:0.50'
        ]
      }
    ])
  })
})

test('can AND with embedded OR', t => {
  t.plan(1)
  idx.AND(
    idx.OR('title:quite', 'body.text:different'),
    'body.metadata:coolness'
  ).then(res => {
    t.looseEqual(res, [
      {
        '_id': 'a',
        'match': [
          'title.quite:0.25',
          'body.metadata.coolness:0.50'
        ]
      },
      {
        '_id': 'b',
        'match': [
          'title.quite:0.25',
          'body.metadata.coolness:0.50'
        ]
      },
      {
        '_id': 'c',
        'match': [
          'body.text.different:0.33',
          'body.metadata.coolness:0.50'
        ]
      }
    ])
  })
})

test('can AND with embedded OR and embedded AND', t => {
  t.plan(1)
  idx.AND(
    idx.OR(
      'title:quite',
      idx.AND(
        'body.text:totally',
        'body.text:different'
      )
    ),
    'body.metadata:coolness'
  ).then(res => {
    t.looseEqual(res, [
      {
        '_id': 'a',
        'match': [
          'title.quite:0.25',
          'body.metadata.coolness:0.50'
        ]
      },
      {
        '_id': 'b',
        'match': [
          'title.quite:0.25',
          'body.metadata.coolness:0.50'
        ]
      },
      {
        '_id': 'c',
        'match': [
          'body.text.totally:0.33',
          'body.text.different:0.33',
          'body.metadata.coolness:0.50'
        ]
      }
    ])
  })
})

test('can NOT', t => {
  t.plan(1)
  idx.NOT(
    'cool',
    'bananas'
  ).then(res => {
    t.looseEqual(res, [
      {
        '_id': 'a',
        'match': [
          'body.text.cool:0.60',
          'title.cool:0.25'
        ]
      }
    ])
  })
})

test('can OR', t => {
  t.plan(1)
  idx.OR('body.text:bananas', 'body.text:different')
   .then(res => {
     t.looseEqual(res, [
       {
         _id: 'b',
         match: [
           'body.text.bananas:0.17'
         ]
       }, {
         _id: 'c',
         match: [
           'body.text.different:0.33'
         ]
       }
     ])
   })
})

test('AND with embedded OR', t => {
  t.plan(1)
  idx.AND(
    'bananas',
    idx.OR('body.text:cool', 'body.text:coolness')
  ).then(res => {
    t.looseEqual(res, [
      { _id: 'b', match: [ 'body.text.bananas:0.17', 'body.text.cool:0.17' ] }
    ])
  })
})

test('AND with embedded OR (JSON API)', t => {
  t.plan(1)
  idx.read({
    AND:['bananas']
  }).then(res => {
    t.looseEqual(res, [
      { _id: 'b', match: [ 'body.text.bananas:0.17' ] }
    ])
  })
})

test('AND with embedded OR (JSON API)', t => {
  t.plan(1)
  idx.read({
    AND:['bananas', {OR:['body.text:cool', 'body.text:coolness']}]
  }).then(res => {
    t.looseEqual(res, [
      { _id: 'b', match: [ 'body.text.bananas:0.17', 'body.text.cool:0.17' ] }
    ])
  })
})


test('AND with embedded OR (THENable JSON API)', t => {
  t.plan(1)
  idx.read('bananas', { DOCUMENTS: true }).then(res => {
    t.looseEqual(res, [
      { _id: 'b', match: [ 'body.text.bananas:0.17' ], obj: {
        _id: 'b', title: 'quite a cool document', body: {
          text: 'this document is really cool bananas', metadata: 'coolness documentness'
        }, importantNumber: 500 }
      }
    ])
  })
})



test('AND with embedded OR', t => {
  t.plan(1)
  idx.AND(
    idx.OR('bananas', 'different'),
    idx.OR('cool', 'coolness')
  ).then(res => {
    t.looseEqual(res, [
      { _id: 'b',
        match: [
          'body.text.bananas:0.17',
          'body.text.cool:0.17',
          'title.cool:0.25',
          'body.metadata.coolness:0.50' ] },
      { _id: 'c',
        match: [
          'body.text.different:0.33',
          'title.different:0.50',
          'body.metadata.coolness:0.50' ] }
    ])
  })
})


// test('debug', t => {
//   t.plan(1)
//   idx.INDEX.STORE.createReadStream()
//     .on('data', console.log)
//     .on('close', () => { t.pass('close') })
// })

test('SEARCH with embedded OR', t => {
  t.plan(1)
  idx.SEARCH(
    idx.OR('bananas', 'different'),
    'coolness'
  ).then(res => {
    t.looseEqual(res, [
      {
        '_id': 'c',
        'match': [
          'body.text.different:0.33',
          'title.different:0.50',
          'body.metadata.coolness:0.50'
        ],
        'score': 0.92,
        'obj': data[2]
      },
      {
        '_id': 'b',
        'match': [
          'body.text.bananas:0.17',
          'body.metadata.coolness:0.50'
        ],
        'score': 0.46,
        'obj': data[1]
      }
    ])
  })
})

test('DICTIONARY with specified field', t => {
  t.plan(1)
  idx.DICTIONARY('body.text').then(res => {
    t.looseEqual(res, [
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
  idx.DICTIONARY({
    gte: 'body.text.d',
    lte: 'body.text.r'
  }).then(res => {
    t.looseEqual(res, [
      'different',
      'document',
      'is',
      'really',
    ])
  })
})


test('DICTIONARY without specified field', t => {
  t.plan(1)
  idx.DICTIONARY().then(res => {
    //    console.log(JSON.stringify(res, null, 2))
    t.looseEqual(res, [
      '200',
      '500',
      '5000',
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

  const { DICTIONARY } = idx
  
  DICTIONARY().then(res => {
    //    console.log(JSON.stringify(res, null, 2))
    t.looseEqual(res, [
      '200',
      '500',
      '5000',
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
