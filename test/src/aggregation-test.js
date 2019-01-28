import si from '../../dist/search-index.esm.js'
import test from 'tape'
import wbd from 'world-bank-dataset'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'WB'

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
  var dataSize = 10
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
  console.log(JSON.stringify(data, null, 2))
  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('can aggregate totalamt using underlying index', t => {
  t.plan(1)
  global[indexName].BUCKETFILTER(
    // global[indexName].INDEX.DISTINCT('totalamt').then(global[indexName].INDEX.EACH),
    global[indexName].INDEX.DISTINCT('totalamt')
                     .then(result => Promise.all(result.map(global[indexName].BUCKET))),
    global[indexName].SEARCH('board_approval_month:October')
  ).then(result => t.looseEqual(
    result,
    [ { match: 'totalamt.0:0',
        _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c787' ] },
      { match: 'totalamt.10000000:10000000',
        _id: [ '52b213b38594d8a2be17c785' ] },
      { match: 'totalamt.13100000:13100000',
        _id: [ '52b213b38594d8a2be17c784' ] },
      { match: 'totalamt.160000000:160000000',
        _id: [ '52b213b38594d8a2be17c788' ] },
      { match: 'totalamt.200000000:200000000',
        _id: [ '52b213b38594d8a2be17c789' ] },
      { match: 'totalamt.500000000:500000000',
        _id: [ '52b213b38594d8a2be17c786' ] } ]
  ))
})

test('can aggregate totalamt', t => {
  t.plan(1)
  global[indexName].DISTINCT('impagency').then(result => t.looseEqual(
    result,
    [
      'impagency.ADMINISTRATION',
      'impagency.AND',
      'impagency.COMMUNICATIONS',
      'impagency.DEPARTMANT,',
      'impagency.EDUCATION',
      'impagency.ENERGY',
      'impagency.FINANCE',
      'impagency.HIGHWAYS',
      'impagency.INDUSTRY',
      'impagency.INTENSIVE',
      'impagency.LABOR',
      'impagency.MINISTRY',
      'impagency.NATIONAL',
      'impagency.OF',
      'impagency.PMU',
      'impagency.PROJECT',
      'impagency.PUBLIC',
      'impagency.RAJASTHAN',
      'impagency.ROAD',
      'impagency.TRADE',
      'impagency.TRANSPORT',
      'impagency.WORKS'
    ]
  ))
})

test('can aggregate totalamt', t => {
  t.plan(1)
  global[indexName].DISTINCT('impagency')
   .then(result => Promise.all(result.map(global[indexName].BUCKET)))
   .then(result => {
     t.looseEqual(
       result,
       [
         { match: 'impagency.ADMINISTRATION',
           _id: [ '52b213b38594d8a2be17c787' ] },
         { match: 'impagency.AND',
           _id:
            [ '52b213b38594d8a2be17c782',
              '52b213b38594d8a2be17c784',
              '52b213b38594d8a2be17c786' ] },
         { match: 'impagency.COMMUNICATIONS',
           _id: [ '52b213b38594d8a2be17c782' ] },
         { match: 'impagency.DEPARTMANT,',
           _id: [ '52b213b38594d8a2be17c788' ] },
         { match: 'impagency.EDUCATION',
           _id: [ '52b213b38594d8a2be17c780' ] },
         { match: 'impagency.ENERGY',
           _id: [ '52b213b38594d8a2be17c787' ] },
         { match: 'impagency.FINANCE',
           _id: [ '52b213b38594d8a2be17c781', '52b213b38594d8a2be17c789' ] },
         { match: 'impagency.HIGHWAYS',
           _id: [ '52b213b38594d8a2be17c786' ] },
         { match: 'impagency.INDUSTRY',
           _id: [ '52b213b38594d8a2be17c784' ] },
         { match: 'impagency.INTENSIVE',
           _id: [ '52b213b38594d8a2be17c783' ] },
         { match: 'impagency.LABOR',
           _id: [ '52b213b38594d8a2be17c783' ] },
         { match: 'impagency.MINISTRY',
           _id:
            [ '52b213b38594d8a2be17c780',
              '52b213b38594d8a2be17c781',
              '52b213b38594d8a2be17c782',
              '52b213b38594d8a2be17c784',
              '52b213b38594d8a2be17c786',
              '52b213b38594d8a2be17c789' ] },
         { match: 'impagency.NATIONAL',
           _id: [ '52b213b38594d8a2be17c787' ] },
         { match: 'impagency.OF',
           _id:
            [ '52b213b38594d8a2be17c780',
              '52b213b38594d8a2be17c781',
              '52b213b38594d8a2be17c782',
              '52b213b38594d8a2be17c784',
              '52b213b38594d8a2be17c786',
              '52b213b38594d8a2be17c789' ] },
         { match: 'impagency.PMU', _id: [ '52b213b38594d8a2be17c783' ] },
         { match: 'impagency.PROJECT',
           _id: [ '52b213b38594d8a2be17c783' ] },
         { match: 'impagency.PUBLIC',
           _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788' ] },
         { match: 'impagency.RAJASTHAN',
           _id: [ '52b213b38594d8a2be17c788' ] },
         { match: 'impagency.ROAD', _id: [ '52b213b38594d8a2be17c786' ] },
         { match: 'impagency.TRADE',
           _id: [ '52b213b38594d8a2be17c784' ] },
         { match: 'impagency.TRANSPORT',
           _id: [ '52b213b38594d8a2be17c782', '52b213b38594d8a2be17c786' ] },
         { match: 'impagency.WORKS',
           _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788' ] }
       ]
     )
   })
})

test('can aggregate totalamt using underlying index', t => {
  t.plan(1)
  global[indexName].BUCKETFILTER(
    global[indexName].DISTINCT('impagency').then(result => Promise.all(result.map(global[indexName].BUCKET))),
    global[indexName].SEARCH('board_approval_month:October')
  ).then(result => {
    t.looseEqual(
      result,
      [ { match: 'impagency.ADMINISTRATION',
          _id: [ '52b213b38594d8a2be17c787' ] },
        { match: 'impagency.AND',
          _id: [ '52b213b38594d8a2be17c784', '52b213b38594d8a2be17c786' ] },
        { match: 'impagency.DEPARTMANT,',
          _id: [ '52b213b38594d8a2be17c788' ] },
        { match: 'impagency.ENERGY',
          _id: [ '52b213b38594d8a2be17c787' ] },
        { match: 'impagency.FINANCE',
          _id: [ '52b213b38594d8a2be17c789' ] },
        { match: 'impagency.HIGHWAYS',
          _id: [ '52b213b38594d8a2be17c786' ] },
        { match: 'impagency.INDUSTRY',
          _id: [ '52b213b38594d8a2be17c784' ] },
        { match: 'impagency.INTENSIVE',
          _id: [ '52b213b38594d8a2be17c783' ] },
        { match: 'impagency.LABOR',
          _id: [ '52b213b38594d8a2be17c783' ] },
        { match: 'impagency.MINISTRY',
          _id:
          [ '52b213b38594d8a2be17c784',
            '52b213b38594d8a2be17c786',
            '52b213b38594d8a2be17c789' ] },
        { match: 'impagency.NATIONAL',
          _id: [ '52b213b38594d8a2be17c787' ] },
        { match: 'impagency.OF',
          _id:
          [ '52b213b38594d8a2be17c784',
            '52b213b38594d8a2be17c786',
            '52b213b38594d8a2be17c789' ] },
        { match: 'impagency.PMU', _id: [ '52b213b38594d8a2be17c783' ] },
        { match: 'impagency.PROJECT',
          _id: [ '52b213b38594d8a2be17c783' ] },
        { match: 'impagency.PUBLIC',
          _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788' ] },
        { match: 'impagency.RAJASTHAN',
          _id: [ '52b213b38594d8a2be17c788' ] },
        { match: 'impagency.ROAD', _id: [ '52b213b38594d8a2be17c786' ] },
        { match: 'impagency.TRADE',
          _id: [ '52b213b38594d8a2be17c784' ] },
        { match: 'impagency.TRANSPORT',
          _id: [ '52b213b38594d8a2be17c786' ] },
        { match: 'impagency.WORKS',
          _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788' ] } ]
    )
  })
})
