import si from '../../dist/search-index.esm.js'
import test from 'tape'
import wbd from 'world-bank-dataset'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'SEARCH'

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
