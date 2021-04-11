const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'FLUSH'

const data = [
  {
    _id: 'a',
    title: 'quite a cool document',
    body: {
      text: 'this document is really cool cool cool',
      metadata: 'coolness documentness'
    },
    importantNumber: 5000
  }
]

test('create a search index', t => {
  t.plan(1)
  si({
    name: indexName,
    storeVectors: true
  }).then(db => {
    global[indexName] = db
    t.pass('ok')
  })
})

test('can add some data', t => {
  t.plan(1)
  global[indexName].PUT(data).then(() => {
    t.pass('ok')
  })
})

test('verify index structure', t => {
  const expectedIndexStructure = [
    { key: 'body.metadata:coolness#1.00', value: ['a'] },
    { key: 'body.metadata:documentness#1.00', value: ['a'] },
    { key: 'body.text:cool#1.00', value: ['a'] },
    { key: 'body.text:document#0.33', value: ['a'] },
    { key: 'body.text:is#0.33', value: ['a'] },
    { key: 'body.text:really#0.33', value: ['a'] },
    { key: 'body.text:this#0.33', value: ['a'] },
    { key: 'importantnumber:5000#1.00', value: ['a'] },
    { key: 'title:a#1.00', value: ['a'] },
    { key: 'title:cool#1.00', value: ['a'] },
    { key: 'title:document#1.00', value: ['a'] },
    { key: 'title:quite#1.00', value: ['a'] },
    { key: '￮DOCUMENT_COUNT￮', value: 1 },
    {
      key: '￮DOC_RAW￮a￮',
      value: {
        _id: 'a',
        title: 'quite a cool document',
        body: {
          text: 'this document is really cool cool cool',
          metadata: 'coolness documentness'
        },
        importantNumber: 5000
      }
    },
    {
      key: '￮DOC￮a￮',
      value: {
        _id: 'a',
        title: ['a#1.00', 'cool#1.00', 'document#1.00', 'quite#1.00'],
        body: {
          text: [
            'cool#1.00', 'document#0.33', 'is#0.33', 'really#0.33', 'this#0.33'
          ],
          metadata: ['coolness#1.00', 'documentness#1.00']
        },
        importantNumber: ['5000#1.00']
      }
    },
    { key: '￮FIELD￮body.metadata￮', value: 'body.metadata' },
    { key: '￮FIELD￮body.text￮', value: 'body.text' },
    { key: '￮FIELD￮importantnumber￮', value: 'importantnumber' },
    { key: '￮FIELD￮title￮', value: 'title' }
  ]
  t.plan(expectedIndexStructure.length)
  global[indexName].INDEX.STORE.createReadStream({ lt: '￮￮' })
    .on('data', d => t.deepEquals(
      d, expectedIndexStructure.shift()
    ))
})

test('FLUSH index and verify', t => {
  t.plan(2)
  const expectedIndexStructure = [
    { key: '￮DOCUMENT_COUNT￮', value: 0 }
  ]
  global[indexName].FLUSH().then(
    () => global[indexName].INDEX.STORE.createReadStream({ lt: '￮￮' })
      .on('data', d => t.deepEquals(
        d, expectedIndexStructure.shift()
      ))
  ).then(() => t.pass('index appears empty'))
})
