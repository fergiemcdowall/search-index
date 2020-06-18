import si from '../../dist/search-index.esm.js'
import test from 'tape'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'DELETE-TEST'

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
  si({ name: indexName }).then(db => {
    global[indexName] = db    
    t.pass('ok')
  })
})

test('can add some worldbank data', t => {
  t.plan(1)
  global[indexName].PUT(data).then(() => {
    t.pass('ok')
  })
})

test('can DELETE', t => {
  t.plan(1)
  global[indexName].DELETE([ 'b' ]).then((res) => t.looseEqual(res, [    
    { _id: 'b', operation: 'DELETE', status: 'OK' } 
  ]))
})

test('verify DELETE', t => {
  const expectedIndexStructure = [
    { key: 'body.metadata:coolness#1.00', value: [ 'a', 'c' ] },
    { key: 'body.metadata:documentness#1.00', value: [ 'a', 'c' ] },
    { key: 'body.text:cool#1.00', value: [ 'a' ] },
    { key: 'body.text:different#1.00', value: [ 'c' ] },
    { key: 'body.text:document#0.33', value: [ 'a' ] },
    { key: 'body.text:is#0.33', value: [ 'a' ] },
    { key: 'body.text:really#0.33', value: [ 'a' ] },
    { key: 'body.text:something#1.00', value: [ 'c' ] },
    { key: 'body.text:this#0.33', value: [ 'a' ] },
    { key: 'body.text:totally#1.00', value: [ 'c' ] },
    { key: 'importantNumber:200#1.00', value: [ 'c' ] },
    { key: 'importantNumber:5000#1.00', value: [ 'a' ] },
    { key: 'title:a#1.00', value: [ 'a' ] },
    { key: 'title:cool#1.00', value: [ 'a' ] },
    { key: 'title:different#1.00', value: [ 'c' ] },
    { key: 'title:document#1.00', value: [ 'a' ] },
    { key: 'title:quite#1.00', value: [ 'a' ] },
    { key: 'title:something#1.00', value: [ 'c' ] },
    { key: '￮DOCUMENT_COUNT￮', value: 3 },
    { key: '￮DOC_RAW￮a￮', value: { _id: 'a', title: 'quite a cool document', body: { text: 'this document is really cool cool cool', metadata: 'coolness documentness' }, importantNumber: 5000 } },
    { key: '￮DOC_RAW￮c￮', value: { _id: 'c', title: 'something different', body: { text: 'something totally different', metadata: 'coolness documentness' }, importantNumber: 200 } },
    { key: '￮DOC￮a￮', value: { _id: 'a', title: [ 'a#1.00', 'cool#1.00', 'document#1.00', 'quite#1.00' ], body: { text: [ 'cool#1.00', 'document#0.33', 'is#0.33', 'really#0.33', 'this#0.33' ], metadata: [ 'coolness#1.00', 'documentness#1.00' ] }, importantNumber: [ '5000#1.00' ] } },
    { key: '￮DOC￮c￮', value: { _id: 'c', title: [ 'different#1.00', 'something#1.00' ], body: { text: [ 'different#1.00', 'something#1.00', 'totally#1.00' ], metadata: [ 'coolness#1.00', 'documentness#1.00' ] }, importantNumber: [ '200#1.00' ] } },
    { key: '￮FIELD￮body.metadata￮', value: 'body.metadata' },
    { key: '￮FIELD￮body.text￮', value: 'body.text' },
    { key: '￮FIELD￮importantNumber￮', value: 'importantNumber' },
    { key: '￮FIELD￮title￮', value: 'title' }
  ]
  t.plan(expectedIndexStructure.length)
  global[indexName].INDEX.STORE.createReadStream()
   .on('data', d => t.looseEquals(
     d, expectedIndexStructure.shift())
   )
})


test('verify DELETE using DOCUMENTS', t => {
  t.plan(1)
  global[indexName].DOCUMENTS([
    {_id:'a'},
    {_id:'b'},
    {_id:'c'}
  ]).then(docs => {
    t.looseEqual(docs, [
      {
        _id: 'a', _doc: {
          _id: 'a', title: 'quite a cool document', body: {
            text: 'this document is really cool cool cool', metadata: 'coolness documentness'
          },
          importantNumber: 5000 }
      }, {
        _id: 'b', _doc: null
      }, {
        _id: 'c', _doc: {
          _id: 'c', title: 'something different', body: {
            text: 'something totally different', metadata: 'coolness documentness'
          },
          importantNumber: 200
        }
      }
    ])
  })
})

test('can DELETE with json', t => {
  t.plan(1)
  global[indexName].UPDATE({
    DELETE: [ 'c' ]
  }).then((res) => t.looseEqual(res, [    
    { _id: 'c', operation: 'DELETE', status: 'OK' } 
  ]))
})

test('verify DELETE', t => {
  const expectedIndexStructure = [
    { key: 'body.metadata:coolness#1.00', value: [ 'a' ] },
    { key: 'body.metadata:documentness#1.00', value: [ 'a' ] },
    { key: 'body.text:cool#1.00', value: [ 'a' ] },
    { key: 'body.text:document#0.33', value: [ 'a' ] },
    { key: 'body.text:is#0.33', value: [ 'a' ] },
    { key: 'body.text:really#0.33', value: [ 'a' ] },
    { key: 'body.text:this#0.33', value: [ 'a' ] },
    { key: 'importantNumber:5000#1.00', value: [ 'a' ] },
    { key: 'title:a#1.00', value: [ 'a' ] },
    { key: 'title:cool#1.00', value: [ 'a' ] },
    { key: 'title:document#1.00', value: [ 'a' ] },
    { key: 'title:quite#1.00', value: [ 'a' ] },
    { key: '￮DOCUMENT_COUNT￮', value: 3 },
    { key: '￮DOC_RAW￮a￮', value: { _id: 'a', title: 'quite a cool document', body: { text: 'this document is really cool cool cool', metadata: 'coolness documentness' }, importantNumber: 5000 } },
    { key: '￮DOC￮a￮', value: { _id: 'a', title: [ 'a#1.00', 'cool#1.00', 'document#1.00', 'quite#1.00' ], body: { text: [ 'cool#1.00', 'document#0.33', 'is#0.33', 'really#0.33', 'this#0.33' ], metadata: [ 'coolness#1.00', 'documentness#1.00' ] }, importantNumber: [ '5000#1.00' ] } },
    { key: '￮FIELD￮body.metadata￮', value: 'body.metadata' },
    { key: '￮FIELD￮body.text￮', value: 'body.text' },
    { key: '￮FIELD￮importantNumber￮', value: 'importantNumber' },
    { key: '￮FIELD￮title￮', value: 'title' }
  ]
  t.plan(expectedIndexStructure.length)
  global[indexName].INDEX.STORE.createReadStream()
   .on('data', d => t.looseEquals(
     d, expectedIndexStructure.shift())
   )

})


test('DELETE with non-existent id', t => {
  t.plan(1)
  global[indexName].UPDATE({
    DELETE: [ 'd' ]
  }).then((res) => t.looseEqual(res, [    
    { _id: 'd', operation: 'DELETE', status: 'OK' } 
  ]))
})
