import si from '../../dist/search-index.esm.js'
import test from 'tape'
import wbd from 'world-bank-dataset'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'BOOLEAN'

test('create a little world bank index', t => {
  t.plan(1)
  global[indexName] = si({ name: indexName })
  t.ok('loaded')
})

test('give lazy loading some time to complete', t => {
  t.plan(1)
  setTimeout(t.pass, 500)
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
  console.log(JSON.stringify(data, null, 2))
  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})


test('AND', t => {
  t.plan(1)
  global[indexName].AND(
    'sectorcode:BZ',
    'sectorcode:BC'
  ).then(res => {
    t.looseEqual(res, [
      {
        _id: '52b213b38594d8a2be17c789',
        match: [ 'sectorcode.BZ:0.33', 'sectorcode.BC:0.33' ]
      }
    ])
  })
})


test('AND', t => {
  t.plan(1)
  global[indexName].read({
    AND:[
      'sectorcode:BZ',
      'sectorcode:BC'
    ]}
  ).then(res => {
    t.looseEqual(res, [
      {
        _id: '52b213b38594d8a2be17c789',
        match: [ 'sectorcode.BZ:0.33', 'sectorcode.BC:0.33' ]
      }
    ])
  })
})


// test('SEARCH', t => {
//   t.plan(1)
//   global[indexName].SEARCH(
//     'sectorcode:BZ'
//   ).then(res => {
//     t.looseEqual(res, [
//       {
//         '_id': 'a',
//         'match': [
//           'title.quite:0.25',
//           'body.metadata.coolness:0.50'
//         ]
//       },
//       {
//         '_id': 'b',
//         'match': [
//           'title.quite:0.25',
//           'body.metadata.coolness:0.50'
//         ]
//       }
//     ])
//   })
// })


// test('JSON AND', t => {
//   t.plan(1)
//   global[indexName].read({
//     AND: [
//       'impagency.BM',
//       'impagency.BC'
//     ]
//   }).then(result => {
//     t.looseEqual(
//       result,
//       [
//         { gte: 'impagency.OF', lte: 'impagency.OF', _id: [
//           '52b213b38594d8a2be17c784', '52b213b38594d8a2be17c786',
//           '52b213b38594d8a2be17c789' ] }
//       ]
//     )
//   })
// })
