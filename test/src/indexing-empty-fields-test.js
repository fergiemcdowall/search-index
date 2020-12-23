const si = require('../../')
const test = require('tape')

const sandbox = 'test/sandbox/'
const dontIndexEmptyFields = sandbox + 'dontIndexEmptyFields'

const data = [{
  _id: '6',
  title: ' a volcano behind     waterfalls. [4472 × 5590] [OC]   '
}]

test('create a case sensitive search index', t => {
  t.plan(1)
  si({
    name: dontIndexEmptyFields
  }).then(db => {
    global[dontIndexEmptyFields] = db
    t.pass('ok')
  })
})

test('can add data to case sensitive index', t => {
  t.plan(1)
  global[dontIndexEmptyFields].PUT(data).then(t.pass)
})

test('should give no results for AND: [""]', t => {
  t.plan(1)
  global[dontIndexEmptyFields].QUERY({
    AND: ['']
  }).then(res => {
    t.deepEqual(res, {
      RESULT: [],
      RESULT_LENGTH: 0
    })
  })
})

test('index looks good', t => {
  const idx = [
    { key: 'title:4472#1.00', value: ['6'] },
    { key: 'title:5590#1.00', value: ['6'] },
    { key: 'title:a#1.00', value: ['6'] },
    { key: 'title:behind#1.00', value: ['6'] },
    { key: 'title:oc#1.00', value: ['6'] },
    { key: 'title:volcano#1.00', value: ['6'] },
    { key: 'title:waterfalls#1.00', value: ['6'] },
    { key: '￮DOCUMENT_COUNT￮', value: 1 },
    {
      key: '￮DOC_RAW￮6￮',
      value: {
        _id: '6',
        title: ' a volcano behind     waterfalls. [4472 × 5590] [OC]   '
      }
    },
    { key: '￮FIELD￮title￮', value: 'title' }
  ]
  t.plan(idx.length)
  global[dontIndexEmptyFields]
    .INDEX
    .STORE
    .createReadStream()
    .on('data', d => t.deepEquals(d, idx.shift()))
})
