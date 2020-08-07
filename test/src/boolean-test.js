import si from '../../dist/search-index.esm.js'
import test from 'tape'
import wbd from 'world-bank-dataset'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'BOOLEAN'

test('create a search index', t => {
  t.plan(1)
  si({ name: indexName }).then(db => {
    global[indexName] = db    
    t.pass('ok')
  })
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
//  console.log(data)
  console.log(JSON.stringify(data, null, 2))
  t.plan(1)
  global[indexName]._PUT(data).then(result => {
    t.pass('ok')
  })
})

test('_AND', t => {
  t.plan(1)
  global[indexName]._AND(
    'sectorcode:bz',
    'sectorcode:bc'
  ).then(res => {
    t.deepEqual(res, [
      {
        _id: '52b213b38594d8a2be17c789',
        _match: [ 'sectorcode:bz#1.00', 'sectorcode:bc#1.00' ]
      }
    ])
  })
})


test('AND', t => {
  t.plan(1)
  global[indexName].GET({
    AND:[
      'sectorcode:bz',
      'sectorcode:bc'
    ]}
  ).then(res => {
    t.deepEqual(res, [
      {
        _id: '52b213b38594d8a2be17c789',
        _match: [ 'sectorcode:bz#1.00', 'sectorcode:bc#1.00' ]
      }
    ])
  })
})

test('OR', t => {
  t.plan(1)
  global[indexName].GET({
    OR:[
      'sectorcode:ti',
      'sectorcode:bz'
    ]}
  ).then(res => {
    t.deepEqual(res, [
      { _id: '52b213b38594d8a2be17c782', _match: [ 'sectorcode:ti#1.00' ] },
      { _id: '52b213b38594d8a2be17c786', _match: [ 'sectorcode:ti#1.00' ] },
      { _id: '52b213b38594d8a2be17c788', _match: [ 'sectorcode:ti#1.00' ] },
      { _id: '52b213b38594d8a2be17c781', _match: [ 'sectorcode:bz#1.00' ] },
      { _id: '52b213b38594d8a2be17c789', _match: [ 'sectorcode:bz#1.00' ] }
    ])
  })
})

test('OR', t => {
  t.plan(1)
  global[indexName].GET({
    OR:[
      'sectorcode:yz',
      'sectorcode:bc'
    ]}
  ).then(res => {
    t.deepEqual(res, [
      { _id: '52b213b38594d8a2be17c784', _match: [ 'sectorcode:yz#1.00' ] },
      { _id: '52b213b38594d8a2be17c789', _match: [ 'sectorcode:bc#1.00' ] }
    ])
  })
})

test('OR', t => {
  t.plan(1)
  global[indexName].GET({
    OR:[
      'sectorcode:yz',
      'sectorcode:bc'
    ]}
  ).then(res => {
    t.deepEqual(res, [
      { _id: '52b213b38594d8a2be17c784', _match: [ 'sectorcode:yz#1.00' ] },
      { _id: '52b213b38594d8a2be17c789', _match: [ 'sectorcode:bc#1.00' ] }
    ])
  })
})

test('OR', t => {
  t.plan(1)
  global[indexName].GET({
    OR:[
      'sectorcode:fh',
      'sectorcode:yw',
      'sectorcode:yz'
    ]}
  ).then(res => {
    t.deepEqual(res, [
      { _id: '52b213b38594d8a2be17c784', _match: [ 'sectorcode:fh#1.00', 'sectorcode:yw#1.00', 'sectorcode:yz#1.00' ] }
    ])
  })
})

test('can _NOT', t => {
  t.plan(1)
  global[indexName]._NOT(
    'sectorcode:ti',
    'board_approval_month:november'
  ).then(res => {
    t.deepEqual(res, [
      { _id: '52b213b38594d8a2be17c786', _match: [ 'sectorcode:ti#1.00' ] },
      { _id: '52b213b38594d8a2be17c788', _match: [ 'sectorcode:ti#1.00' ] }
    ])
  })
})

test('can NOT', t => {
  t.plan(1)
  global[indexName].GET(
    {
      NOT: {
        INCLUDE: 'sectorcode:ti',
        EXCLUDE: 'board_approval_month:november'
      }
    }
  ).then(res => {
    t.deepEqual(res, [
      { _id: '52b213b38594d8a2be17c786', _match: [ 'sectorcode:ti#1.00' ] },
      { _id: '52b213b38594d8a2be17c788', _match: [ 'sectorcode:ti#1.00' ] }
    ])
  })
})
