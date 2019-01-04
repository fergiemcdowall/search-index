const test = require('tape')
const sandbox = 'test/sandbox/'
const si = require('../lib/main.js')

const indexName = sandbox + 'delete-test'

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
  global[indexName] = si({ name: indexName })
  t.pass('ok')
})

test('give lazy loading some time to complete', t => {
  t.plan(1)
  setTimeout(t.pass, 500)
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
    {
      'title': {
        'a': '0.25',
        'cool': '0.25',
        'document': '0.25',
        'quite': '0.25'
      },
      'body.text': {
        'bananas': '0.17',
        'cool': '0.17',
        'document': '0.17',
        'is': '0.17',
        'really': '0.17',
        'this': '0.17'
      },
      'body.metadata': {
        'coolness': '0.50',
        'documentness': '0.50'
      },
      'importantNumber': {
        '500': '500'
      },
      '_id': 'b',
      '!doc': {
        '_id': 'b',
        'title': 'quite a cool document',
        'body': {
          'text': 'this document is really cool bananas',
          'metadata': 'coolness documentness'
        },
        'importantNumber': 500
      }
    }
  ]))
})

test('verify DELETE', t => {
  t.plan(1)
  const indexStructure = []
  const expectedIndexStructure = [
    { 'key': 'body.metadata.coolness:0.50', 'value': ['a', 'c'] },
    { 'key': 'body.metadata.documentness:0.50', 'value': ['a', 'c'] },
    { 'key': 'body.text.cool:0.60', 'value': ['a'] },
    { 'key': 'body.text.different:0.33', 'value': ['c'] },
    { 'key': 'body.text.document:0.20', 'value': ['a'] },
    { 'key': 'body.text.is:0.20', 'value': ['a'] },
    { 'key': 'body.text.really:0.20', 'value': ['a'] },
    { 'key': 'body.text.something:0.33', 'value': ['c'] },
    { 'key': 'body.text.this:0.20', 'value': ['a'] },
    { 'key': 'body.text.totally:0.33', 'value': ['c'] },
    { 'key': 'importantNumber.200:200', 'value': ['c'] },
    { 'key': 'importantNumber.5000:5000', 'value': ['a'] },
    { 'key': 'title.a:0.25', 'value': ['a'] },
    { 'key': 'title.cool:0.25', 'value': ['a'] },
    { 'key': 'title.different:0.50', 'value': ['c'] },
    { 'key': 'title.document:0.25', 'value': ['a'] },
    { 'key': 'title.quite:0.25', 'value': ['a'] },
    { 'key': 'title.something:0.50', 'value': ['c'] },
    { 'key': '￮DOC￮a￮', 'value': { 'title': { 'a': '0.25', 'cool': '0.25', 'document': '0.25', 'quite': '0.25' }, 'body.text': { 'cool': '0.60', 'document': '0.20', 'is': '0.20', 'really': '0.20', 'this': '0.20' }, 'body.metadata': { 'coolness': '0.50', 'documentness': '0.50' }, 'importantNumber': { '5000': 5000 }, '_id': 'a', '!doc': { '_id': 'a', 'title': 'quite a cool document', 'body': { 'text': 'this document is really cool cool cool', 'metadata': 'coolness documentness' }, 'importantNumber': 5000 } } },
    { 'key': '￮DOC￮c￮', 'value': { 'title': { 'different': '0.50', 'something': '0.50' }, 'body.text': { 'different': '0.33', 'something': '0.33', 'totally': '0.33' }, 'body.metadata': { 'coolness': '0.50', 'documentness': '0.50' }, 'importantNumber': { '200': 200 }, '_id': 'c', '!doc': { '_id': 'c', 'title': 'something different', 'body': { 'text': 'something totally different', 'metadata': 'coolness documentness' }, 'importantNumber': 200 } } },
    { 'key': '￮FIELD￮body.metadata￮', 'value': true },
    { 'key': '￮FIELD￮body.text￮', 'value': true },
    { 'key': '￮FIELD￮importantNumber￮', 'value': true },
    { 'key': '￮FIELD￮title￮', 'value': true }
  ]
  global[indexName].INDEX.STORE.createReadStream()
    .on('data', d => indexStructure.push(d))
    .on('end', d => t.looseEquals(indexStructure, expectedIndexStructure))
})
