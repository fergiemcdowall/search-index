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
  console.log(JSON.stringify(data, null, 2))
  t.plan(1)
  global[indexName].PUT(data).then(result => {
    console.log('result is ->')
    console.log(result)
    t.pass('ok')
  })
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
        _match: [ 'sectorcode.BZ:0.33', 'sectorcode.BC:0.33' ]
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
        _match: [ 'sectorcode.BZ:0.33', 'sectorcode.BC:0.33' ]
      }
    ])
  })
})

test('OR', t => {
  t.plan(1)
  global[indexName].read({
    OR:[
      'sectorcode:TI',
      'sectorcode:BZ'
    ]}
  ).then(res => {
    t.looseEqual(res, [
      { _id: '52b213b38594d8a2be17c782', _match: [ 'sectorcode.TI:1.00' ] },
      { _id: '52b213b38594d8a2be17c786', _match: [ 'sectorcode.TI:1.00' ] },
      { _id: '52b213b38594d8a2be17c788', _match: [ 'sectorcode.TI:1.00' ] },
      { _id: '52b213b38594d8a2be17c789', _match: [ 'sectorcode.BZ:0.33' ] },
      { _id: '52b213b38594d8a2be17c781', _match: [ 'sectorcode.BZ:0.50' ] }
    ])
  })
})

test('OR', t => {
  t.plan(1)
  global[indexName].read({
    OR:[
      'sectorcode:YZ',
      'sectorcode:BC'
    ]}
  ).then(res => {
    t.looseEqual(res, [
      { _id: '52b213b38594d8a2be17c784', _match: [ 'sectorcode.YZ:0.33' ] },
      { _id: '52b213b38594d8a2be17c789', _match: [ 'sectorcode.BC:0.33' ] }
    ])
  })
})

test('OR', t => {
  t.plan(1)
  global[indexName].read({
    OR:[
      'sectorcode:YZ',
      'sectorcode:BC'
    ]}
  ).then(res => {
    t.looseEqual(res, [
      { _id: '52b213b38594d8a2be17c784', _match: [ 'sectorcode.YZ:0.33' ] },
      { _id: '52b213b38594d8a2be17c789', _match: [ 'sectorcode.BC:0.33' ] }
    ])
  })
})

test('OR', t => {
  t.plan(1)
  global[indexName].read({
    OR:[
      'sectorcode:FH',
      'sectorcode:YW',
      'sectorcode:YZ'
    ]}
  ).then(res => {
    t.looseEqual(res, [
      { _id: '52b213b38594d8a2be17c784', _match: [ 'sectorcode.FH:0.33', 'sectorcode.YW:0.33', 'sectorcode.YZ:0.33' ] }
    ])
  })
})

test('can NOT', t => {
  t.plan(1)
  global[indexName].NOT(
    'sectorcode:TI',
    'board_approval_month:November'
  ).then(res => {
    t.looseEqual(res, [
      { _id: '52b213b38594d8a2be17c786', _match: [ 'sectorcode.TI:1.00' ] },
      { _id: '52b213b38594d8a2be17c788', _match: [ 'sectorcode.TI:1.00' ] }
    ])
  })
})

test('can NOT', t => {
  t.plan(1)
  global[indexName].read(
    {
      NOT: {
        include: 'sectorcode:TI',
        exclude: 'board_approval_month:November'
      }
    }
  ).then(res => {
    t.looseEqual(res, [
      { _id: '52b213b38594d8a2be17c786', _match: [ 'sectorcode.TI:1.00' ] },
      { _id: '52b213b38594d8a2be17c788', _match: [ 'sectorcode.TI:1.00' ] }
    ])
  })
})
