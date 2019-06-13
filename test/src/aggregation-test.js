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
    [ { gte: 'totalamt.0:0',
        lte: 'totalamt.0:0',
        _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c787' ] },
      { gte: 'totalamt.10000000:10000000',
        lte: 'totalamt.10000000:10000000',
        _id: [ '52b213b38594d8a2be17c785' ] },
      { gte: 'totalamt.13100000:13100000',
        lte: 'totalamt.13100000:13100000',
        _id: [ '52b213b38594d8a2be17c784' ] },
      { gte: 'totalamt.160000000:160000000',
        lte: 'totalamt.160000000:160000000',
        _id: [ '52b213b38594d8a2be17c788' ] },
      { gte: 'totalamt.200000000:200000000',
        lte: 'totalamt.200000000:200000000',
        _id: [ '52b213b38594d8a2be17c789' ] },
      { gte: 'totalamt.500000000:500000000',
        lte: 'totalamt.500000000:500000000',
        _id: [ '52b213b38594d8a2be17c786' ] } ]
  ))
})

test('can aggregate totalamt using BUCKETFILTER (alternative invokation)', t => {
  t.plan(1)

  const b = global[indexName].DISTINCT('totalamt')
    .then(result => Promise.all(result.map(global[indexName].BUCKET)))
  const s = global[indexName].SEARCH('board_approval_month:October')
  
  global[indexName].BUCKETFILTER(b, s).then(result => t.looseEqual(
    result,
    [ { gte: 'totalamt.0',
        lte: 'totalamt.0',
        _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c787' ] },
      { gte: 'totalamt.10000000',
        lte: 'totalamt.10000000',
        _id: [ '52b213b38594d8a2be17c785' ] },
      { gte: 'totalamt.13100000',
        lte: 'totalamt.13100000',
        _id: [ '52b213b38594d8a2be17c784' ] },
      { gte: 'totalamt.160000000',
        lte: 'totalamt.160000000',
        _id: [ '52b213b38594d8a2be17c788' ] },
      { gte: 'totalamt.200000000',
        lte: 'totalamt.200000000',
        _id: [ '52b213b38594d8a2be17c789' ] },
      { gte: 'totalamt.500000000',
        lte: 'totalamt.500000000',
        _id: [ '52b213b38594d8a2be17c786' ] } ]
  ))
})


test('BUCKETing', t => {
  t.plan(1)
  global[indexName].BUCKET(
    {gte: 'totalamt.00', lte:'totalamt.25'}
  ).then(result => t.looseEqual(
    result,
    { gte: 'totalamt.00',
      lte: 'totalamt.25',
      _id: [ '52b213b38594d8a2be17c780', '52b213b38594d8a2be17c781',
             '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c784',
             '52b213b38594d8a2be17c785', '52b213b38594d8a2be17c787',
             '52b213b38594d8a2be17c788', '52b213b38594d8a2be17c789' ] }
  ))
})

test('BUCKETing', t => {
  t.plan(1)
  global[indexName].BUCKET(
    {gte: 'totalamt.26', lte:'totalamt.70'}
  ).then(result => t.looseEqual(
    result,
    { gte: 'totalamt.26',
      lte: 'totalamt.70',
      _id: [ '52b213b38594d8a2be17c782', '52b213b38594d8a2be17c786' ] }
  ))
})


test('can aggregate totalamt using BUCKETFILTER and custom buckets', t => {
  t.plan(1)  
  const b = Promise.all([
    'totalamt.0',
    'totalamt.10000000',
    'totalamt.200000000'
  ].map(global[indexName].BUCKET))
  const s = global[indexName].SEARCH('board_approval_month:October')  
  global[indexName].BUCKETFILTER(b, s).then(result => t.looseEqual(
    result,
    [ { gte: 'totalamt.0',
        lte: 'totalamt.0',
        _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c787' ] },
      { gte: 'totalamt.10000000',
        lte: 'totalamt.10000000',
        _id: [ '52b213b38594d8a2be17c785' ] },
      { gte: 'totalamt.200000000',
        lte: 'totalamt.200000000',
        _id: [ '52b213b38594d8a2be17c789' ] } ]
  ))
})

test('can aggregate totalamt using BUCKETFILTER and custom buckets', t => {
  t.plan(1)  
  const b = Promise.all([
    'totalamt.0',
    'totalamt.10000000',
    'totalamt.200000000'
  ].map(global[indexName].BUCKET))
  b.then(result => t.looseEqual(
    result,
    [ { gte: 'totalamt.0',
        lte: 'totalamt.0',
        _id: [ '52b213b38594d8a2be17c781', '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c787' ] },
      { gte: 'totalamt.10000000',
        lte: 'totalamt.10000000',
        _id: [ '52b213b38594d8a2be17c785' ] },
      { gte: 'totalamt.200000000',
        lte: 'totalamt.200000000',
        _id: [ '52b213b38594d8a2be17c789' ] } ]
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
         { gte: 'impagency.ADMINISTRATION',
           lte: 'impagency.ADMINISTRATION',
           _id: [ '52b213b38594d8a2be17c787' ] },
         { gte: 'impagency.AND',
           lte: 'impagency.AND',
           _id:
            [ '52b213b38594d8a2be17c782',
              '52b213b38594d8a2be17c784',
              '52b213b38594d8a2be17c786' ] },
         { gte: 'impagency.COMMUNICATIONS',
           lte: 'impagency.COMMUNICATIONS',
           _id: [ '52b213b38594d8a2be17c782' ] },
         { gte: 'impagency.DEPARTMANT,',
           lte: 'impagency.DEPARTMANT,',
           _id: [ '52b213b38594d8a2be17c788' ] },
         { gte: 'impagency.EDUCATION',
           lte: 'impagency.EDUCATION',
           _id: [ '52b213b38594d8a2be17c780' ] },
         { gte: 'impagency.ENERGY',
           lte: 'impagency.ENERGY',
           _id: [ '52b213b38594d8a2be17c787' ] },
         { gte: 'impagency.FINANCE',
           lte: 'impagency.FINANCE',
           _id: [ '52b213b38594d8a2be17c781', '52b213b38594d8a2be17c789' ] },
         { gte: 'impagency.HIGHWAYS',
           lte: 'impagency.HIGHWAYS',
           _id: [ '52b213b38594d8a2be17c786' ] },
         { gte: 'impagency.INDUSTRY',
           lte: 'impagency.INDUSTRY',
           _id: [ '52b213b38594d8a2be17c784' ] },
         { gte: 'impagency.INTENSIVE',
           lte: 'impagency.INTENSIVE',
           _id: [ '52b213b38594d8a2be17c783' ] },
         { gte: 'impagency.LABOR',
           lte: 'impagency.LABOR',
           _id: [ '52b213b38594d8a2be17c783' ] },
         { gte: 'impagency.MINISTRY',
           lte: 'impagency.MINISTRY',
           _id:
            [ '52b213b38594d8a2be17c780',
              '52b213b38594d8a2be17c781',
              '52b213b38594d8a2be17c782',
              '52b213b38594d8a2be17c784',
              '52b213b38594d8a2be17c786',
              '52b213b38594d8a2be17c789' ] },
         { gte: 'impagency.NATIONAL',
           lte: 'impagency.NATIONAL',
           _id: [ '52b213b38594d8a2be17c787' ] },
         { gte: 'impagency.OF',
           lte: 'impagency.OF',
           _id:
            [ '52b213b38594d8a2be17c780',
              '52b213b38594d8a2be17c781',
              '52b213b38594d8a2be17c782',
              '52b213b38594d8a2be17c784',
              '52b213b38594d8a2be17c786',
              '52b213b38594d8a2be17c789' ] },
         { gte: 'impagency.PMU',
           lte: 'impagency.PMU',
           _id: [ '52b213b38594d8a2be17c783' ] },
         { gte: 'impagency.PROJECT',
           lte: 'impagency.PROJECT',
           _id: [ '52b213b38594d8a2be17c783' ] },
         { gte: 'impagency.PUBLIC',
           lte: 'impagency.PUBLIC',
           _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788' ] },
         { gte: 'impagency.RAJASTHAN',
           lte: 'impagency.RAJASTHAN',
           _id: [ '52b213b38594d8a2be17c788' ] },
         { gte: 'impagency.ROAD',
           lte: 'impagency.ROAD',
           _id: [ '52b213b38594d8a2be17c786' ] },
         { gte: 'impagency.TRADE',
           lte: 'impagency.TRADE',
           _id: [ '52b213b38594d8a2be17c784' ] },
         { gte: 'impagency.TRANSPORT',
           lte: 'impagency.TRANSPORT',
           _id: [ '52b213b38594d8a2be17c782', '52b213b38594d8a2be17c786' ] },
         { gte: 'impagency.WORKS',
           lte: 'impagency.WORKS',
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
      [ { gte: 'impagency.ADMINISTRATION', lte:
  'impagency.ADMINISTRATION', _id: [ '52b213b38594d8a2be17c787' ] },
        { gte: 'impagency.AND', lte: 'impagency.AND', _id: [
          '52b213b38594d8a2be17c784', '52b213b38594d8a2be17c786' ] },
        { gte: 'impagency.DEPARTMANT,', lte: 'impagency.DEPARTMANT,',
          _id: [ '52b213b38594d8a2be17c788' ] },
        { gte: 'impagency.ENERGY', lte: 'impagency.ENERGY', _id: [
          '52b213b38594d8a2be17c787' ] },
        { gte: 'impagency.FINANCE', lte: 'impagency.FINANCE', _id: [
          '52b213b38594d8a2be17c789' ] },
        { gte: 'impagency.HIGHWAYS', lte: 'impagency.HIGHWAYS', _id: [
          '52b213b38594d8a2be17c786' ] },
        { gte: 'impagency.INDUSTRY', lte: 'impagency.INDUSTRY', _id: [
          '52b213b38594d8a2be17c784' ] },
        { gte: 'impagency.INTENSIVE', lte: 'impagency.INTENSIVE', _id:
  [ '52b213b38594d8a2be17c783' ] },
        { gte: 'impagency.LABOR', lte: 'impagency.LABOR', _id: [
          '52b213b38594d8a2be17c783' ] },
        { gte: 'impagency.MINISTRY', lte: 'impagency.MINISTRY', _id: [
          '52b213b38594d8a2be17c784', '52b213b38594d8a2be17c786',
          '52b213b38594d8a2be17c789' ] },
        { gte: 'impagency.NATIONAL', lte: 'impagency.NATIONAL', _id: [
          '52b213b38594d8a2be17c787' ] },
        { gte: 'impagency.OF', lte: 'impagency.OF', _id: [
          '52b213b38594d8a2be17c784', '52b213b38594d8a2be17c786',
          '52b213b38594d8a2be17c789' ] },
        { gte: 'impagency.PMU', lte: 'impagency.PMU', _id: [
          '52b213b38594d8a2be17c783' ] },
        { gte: 'impagency.PROJECT', lte: 'impagency.PROJECT', _id: [
          '52b213b38594d8a2be17c783' ] },
        { gte: 'impagency.PUBLIC', lte: 'impagency.PUBLIC', _id: [
          '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788' ] },
        { gte: 'impagency.RAJASTHAN', lte: 'impagency.RAJASTHAN', _id:
  [ '52b213b38594d8a2be17c788' ] },
        { gte: 'impagency.ROAD', lte: 'impagency.ROAD', _id: [
          '52b213b38594d8a2be17c786' ] },
        { gte: 'impagency.TRADE', lte: 'impagency.TRADE', _id: [
          '52b213b38594d8a2be17c784' ] },
        { gte: 'impagency.TRANSPORT', lte: 'impagency.TRANSPORT', _id:
  [ '52b213b38594d8a2be17c786' ] },
        { gte: 'impagency.WORKS', lte: 'impagency.WORKS', _id: [
          '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788' ] }
      ]
    )
  })
})


test('can aggregate totalamt using underlying index', t => {
  t.plan(1)
  global[indexName].BUCKET(
    'impagency.PMU'
  ).then(result => {
    t.looseEqual(
      result,
      {
        gte: 'impagency.PMU', lte: 'impagency.PMU',
        _id: ['52b213b38594d8a2be17c783']
      }
    )
  })
})


test('can aggregate totalamt using underlying index', t => {
  t.plan(1)
  global[indexName].BUCKET(
    { gte: 'impagency.P', lte: 'impagency.TXX' }
  ).then(result => {
    t.looseEqual(
      result,
      { gte: 'impagency.P',
        lte: 'impagency.TXX',
        _id: [ '52b213b38594d8a2be17c782',
               '52b213b38594d8a2be17c783',
               '52b213b38594d8a2be17c784',
               '52b213b38594d8a2be17c786',
               '52b213b38594d8a2be17c788' ] }
    )
  })
})


test('can aggregate totalamt using underlying index', t => {
  t.plan(1)
  Promise.all([
    global[indexName].BUCKET({ gte: 'totalamt.0', lte: 'totalamt.4999999999999' }),
    global[indexName].BUCKET({ gte: 'totalamt.5', lte: 'totalamt.9' })
  ]).then(result => {
    t.looseEqual(
      result,
      [ { gte: 'totalamt.0', lte: 'totalamt.4999999999999', _id: [
        '52b213b38594d8a2be17c780',
        '52b213b38594d8a2be17c781',
        '52b213b38594d8a2be17c783',
        '52b213b38594d8a2be17c784',
        '52b213b38594d8a2be17c785',
        '52b213b38594d8a2be17c787',
        '52b213b38594d8a2be17c788',
        '52b213b38594d8a2be17c789' ] },
        { gte: 'totalamt.5', lte: 'totalamt.9', _id: [
          '52b213b38594d8a2be17c782', '52b213b38594d8a2be17c786' ] } ]
    )
  })
})
