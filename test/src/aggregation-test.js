import si from '../../dist/search-index.esm.js'
import test from 'tape'
import wbd from 'world-bank-dataset'

const sandbox = 'test/sandbox/'
const indexName = sandbox + 'WB'


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
      sectorcode: item.sectorcode.split(','),
      board_approval_month: item.board_approval_month,
      impagency: item.impagency,
      majorsector_percent: item.majorsector_percent,
      mjsector_namecode: item.mjsector_namecode,
      sector_namecode: item.sector_namecode,
      totalamt: item.totalamt
    }
  })
  /* console.log(JSON.stringify(data.map(item => ({
   *   _id: item._id,
   *   totalamt: item.totalamt,
   *   board_approval_month: item.board_approval_month,
   * })), null, 2))*/
  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('can aggregate totalamt using underlying index', t => {
  const { BUCKET, BUCKETFILTER, DISTINCT, SEARCH } = global[indexName]
  t.plan(1)
  BUCKETFILTER(
    DISTINCT({
      field: 'totalamt'
    }).then(dict => dict.map(BUCKET)),
    SEARCH('board_approval_month:october')
  ).then(result => t.looseEqual(
    result,
    [
      { field: 'totalamt', value: { gte: '0', lte: '0' }, _id: [
        '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c787'
      ] },
      { field: 'totalamt', value: { gte: '10000000', lte: '10000000' }, _id: [
        '52b213b38594d8a2be17c785'
      ] },
      { field: 'totalamt', value: { gte: '130000000', lte: '130000000' }, _id: [] },
      { field: 'totalamt', value: { gte: '13100000', lte: '13100000' }, _id: [
        '52b213b38594d8a2be17c784'
      ] },
      { field: 'totalamt', value: { gte: '160000000', lte: '160000000' }, _id: [
        '52b213b38594d8a2be17c788'
      ] },
      { field: 'totalamt', value: { gte: '200000000', lte: '200000000' }, _id: [
        '52b213b38594d8a2be17c789'
      ] },
      { field: 'totalamt', value: { gte: '500000000', lte: '500000000' }, _id: [
        '52b213b38594d8a2be17c786'
      ] },
      { field: 'totalamt', value: { gte: '6060000', lte: '6060000' }, _id: [] }      
    ]
  ))
})

test('can aggregate totalamt using BUCKETFILTER (alternative invokation)', t => {
  t.plan(1)
  const { BUCKET, BUCKETFILTER, DISTINCT, SEARCH } = global[indexName]

  const b = DISTINCT({
    field: 'totalamt'
  }).then(dict => dict.map(BUCKET))
  const s = SEARCH('board_approval_month:october')
  
  BUCKETFILTER(b, s).then(result => t.looseEqual(
    result,
    [
      { field: 'totalamt', value: { gte: '0', lte: '0' }, _id: [
        '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c787'
      ] },
      { field: 'totalamt', value: { gte: '10000000', lte: '10000000' }, _id: [
        '52b213b38594d8a2be17c785'
      ] },
      { field: 'totalamt', value: { gte: '130000000', lte: '130000000' }, _id: [] },
      { field: 'totalamt', value: { gte: '13100000', lte: '13100000' }, _id: [
        '52b213b38594d8a2be17c784'
      ] },
      { field: 'totalamt', value: { gte: '160000000', lte: '160000000' }, _id: [
        '52b213b38594d8a2be17c788'
      ] },
      { field: 'totalamt', value: { gte: '200000000', lte: '200000000' }, _id: [
        '52b213b38594d8a2be17c789'
      ] },
      { field: 'totalamt', value: { gte: '500000000', lte: '500000000' }, _id: [
        '52b213b38594d8a2be17c786'
      ] },
      { field: 'totalamt', value: { gte: '6060000', lte: '6060000' }, _id: [] }
    ]
  ))
})

test('BUCKETing', t => {
  t.plan(1)
  global[indexName].BUCKET({
    field: 'totalamt',
    value: {
      gte: '0',
      lte: '25'
    }
  }).then(result => t.looseEqual(
    result,
    {
      field: 'totalamt', value: { gte: '0', lte: '25' },
      _id: [ '52b213b38594d8a2be17c780', '52b213b38594d8a2be17c781',
             '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c784',
             '52b213b38594d8a2be17c785', '52b213b38594d8a2be17c787',
             '52b213b38594d8a2be17c788', '52b213b38594d8a2be17c789' ] }
  ))
})


test('BUCKETing', t => {
  t.plan(1)
  global[indexName].BUCKET({
    field: 'totalamt',
    value: {
      gte: '00',
      lte: '25'
    }
  }).then(result => t.looseEqual(
    result,
    {
      field: 'totalamt', value: { gte: '00', lte: '25' },
      _id: [ '52b213b38594d8a2be17c780',
             '52b213b38594d8a2be17c784',
             '52b213b38594d8a2be17c785',
             '52b213b38594d8a2be17c788',
             '52b213b38594d8a2be17c789' ] }
  ))
})


test('BUCKETing', t => {
  t.plan(1)
  global[indexName].BUCKET({
    field: 'totalamt',
    value: {
      gte: '26',
      lte: '70'
    }
  }).then(result => t.looseEqual(
    result,
    {
      field: 'totalamt',
      value: { gte: '26', lte: '70' },
      _id: [ '52b213b38594d8a2be17c782', '52b213b38594d8a2be17c786' ]
    }
  ))
})



test('can aggregate totalamt using BUCKETFILTER and custom buckets', t => {
  t.plan(1)  
  const b = Promise.all([
    {
      field: 'totalamt',
      value: { gte: '0', lte: '0' }
    },
    {
      field: 'totalamt',
      value: { gte: '10000000', lte: '10000000' }
    },
    {
      field: 'totalamt',
      value: { gte: '200000000', lte: '200000000' }
    }
  ].map(global[indexName].BUCKET))
  const s = global[indexName].SEARCH('board_approval_month:october')  
  global[indexName].BUCKETFILTER(b, s).then(result => t.looseEqual(
    result,
    [
      { field: 'totalamt', value: { gte: '0', lte: '0' },
        _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c787' ] },
      { field: 'totalamt', value: { gte: '10000000', lte: '10000000' },
        _id: [ '52b213b38594d8a2be17c785' ] },
      { field: 'totalamt', value: { gte: '200000000', lte: '200000000' },
        _id: [ '52b213b38594d8a2be17c789' ] }
    ]
  ))
})

test('make some BUCKETs', t => {
  t.plan(1)  
  const b = Promise.all([
    {
      field: 'totalamt',
      value: { gte: '0', lte: '0' }
    },
    {
      field: 'totalamt',
      value: { gte: '10000000', lte: '10000000' }
    },
    {
      field: 'totalamt',
      value: { gte: '200000000', lte: '200000000' }
    }
  ].map(global[indexName].BUCKET))
  b.then(result => t.looseEqual(
    result,
    [
      {
        field: 'totalamt', value: { gte: '0', lte: '0' },
        _id: [ '52b213b38594d8a2be17c781', '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c787' ]
      },
      {
        field: 'totalamt', value: { gte: '10000000', lte: '10000000' },
        _id: [ '52b213b38594d8a2be17c785' ]
      },
      {
        field: 'totalamt', value: { gte: '200000000', lte: '200000000' },
        _id: [ '52b213b38594d8a2be17c789' ]
      }
    ]
  ))
})


test('can aggregate totalamt', t => {
  t.plan(1)
  global[indexName].DISTINCT({
    field: 'impagency'
  }).then(result => t.looseEqual(
    result, [
      { field: 'impagency', value: 'administration' },
      { field: 'impagency', value: 'and' },
      { field: 'impagency', value: 'communications' },
      { field: 'impagency', value: 'departmant' },
      { field: 'impagency', value: 'education' },
      { field: 'impagency', value: 'energy' },
      { field: 'impagency', value: 'finance' },
      { field: 'impagency', value: 'highways' },
      { field: 'impagency', value: 'industry' },
      { field: 'impagency', value: 'intensive' },
      { field: 'impagency', value: 'labor' },
      { field: 'impagency', value: 'ministry' },
      { field: 'impagency', value: 'national' },
      { field: 'impagency', value: 'of' },
      { field: 'impagency', value: 'pmu' },
      { field: 'impagency', value: 'project' },
      { field: 'impagency', value: 'public' },
      { field: 'impagency', value: 'rajasthan' },
      { field: 'impagency', value: 'road' },
      { field: 'impagency', value: 'trade' },
      { field: 'impagency', value: 'transport' },
      { field: 'impagency', value: 'works' }
    ]
  ))
})


test('can aggregate totalamt JSON DISTINCT', t => {
  t.plan(1)
  global[indexName].QUERY({
    DISTINCT: {
      field: 'impagency'
    }
  }).then(result => t.looseEqual(
    result, [
      { field: 'impagency', value: 'administration' },
      { field: 'impagency', value: 'and' },
      { field: 'impagency', value: 'communications' },
      { field: 'impagency', value: 'departmant' },
      { field: 'impagency', value: 'education' },
      { field: 'impagency', value: 'energy' },
      { field: 'impagency', value: 'finance' },
      { field: 'impagency', value: 'highways' },
      { field: 'impagency', value: 'industry' },
      { field: 'impagency', value: 'intensive' },
      { field: 'impagency', value: 'labor' },
      { field: 'impagency', value: 'ministry' },
      { field: 'impagency', value: 'national' },
      { field: 'impagency', value: 'of' },
      { field: 'impagency', value: 'pmu' },
      { field: 'impagency', value: 'project' },
      { field: 'impagency', value: 'public' },
      { field: 'impagency', value: 'rajasthan' },
      { field: 'impagency', value: 'road' },
      { field: 'impagency', value: 'trade' },
      { field: 'impagency', value: 'transport' },
      { field: 'impagency', value: 'works' }
    ]
  ))
})


test('can aggregate totalamt', t => {
  t.plan(1)
  global[indexName].DISTINCT({
    field: 'impagency'
  })
   .then(result => Promise.all(result.map(global[indexName].BUCKET)))
   .then(result => {
     t.looseEqual(
       result,
       [
         { field: 'impagency', value: { gte: 'administration', lte: 'administration' }, _id: [ '52b213b38594d8a2be17c787' ] },
         { field: 'impagency', value: { gte: 'and', lte: 'and' }, _id: [ '52b213b38594d8a2be17c782', '52b213b38594d8a2be17c784', '52b213b38594d8a2be17c786' ] },
         { field: 'impagency', value: { gte: 'communications', lte: 'communications' }, _id: [ '52b213b38594d8a2be17c782' ] },
         { field: 'impagency', value: { gte: 'departmant', lte: 'departmant' }, _id: [ '52b213b38594d8a2be17c788' ] },
         { field: 'impagency', value: { gte: 'education', lte: 'education' }, _id: [ '52b213b38594d8a2be17c780' ] },
         { field: 'impagency', value: { gte: 'energy', lte: 'energy' }, _id: [ '52b213b38594d8a2be17c787' ] },
         { field: 'impagency', value: { gte: 'finance', lte: 'finance' }, _id: [ '52b213b38594d8a2be17c781', '52b213b38594d8a2be17c789' ] },
         { field: 'impagency', value: { gte: 'highways', lte: 'highways' }, _id: [ '52b213b38594d8a2be17c786' ] },
         { field: 'impagency', value: { gte: 'industry', lte: 'industry' }, _id: [ '52b213b38594d8a2be17c784' ] },
         { field: 'impagency', value: { gte: 'intensive', lte: 'intensive' }, _id: [ '52b213b38594d8a2be17c783' ] },
         { field: 'impagency', value: { gte: 'labor', lte: 'labor' }, _id: [ '52b213b38594d8a2be17c783' ] },
         { field: 'impagency', value: { gte: 'ministry', lte: 'ministry' }, _id: [ '52b213b38594d8a2be17c780', '52b213b38594d8a2be17c781', '52b213b38594d8a2be17c782', '52b213b38594d8a2be17c784', '52b213b38594d8a2be17c786', '52b213b38594d8a2be17c789' ] },
         { field: 'impagency', value: { gte: 'national', lte: 'national' }, _id: [ '52b213b38594d8a2be17c787' ] },
         { field: 'impagency', value: { gte: 'of', lte: 'of' }, _id: [ '52b213b38594d8a2be17c780', '52b213b38594d8a2be17c781', '52b213b38594d8a2be17c782', '52b213b38594d8a2be17c784', '52b213b38594d8a2be17c786', '52b213b38594d8a2be17c789' ] },
         { field: 'impagency', value: { gte: 'pmu', lte: 'pmu' }, _id: [ '52b213b38594d8a2be17c783' ] },
         { field: 'impagency', value: { gte: 'project', lte: 'project' }, _id: [ '52b213b38594d8a2be17c783' ] },
         { field: 'impagency', value: { gte: 'public', lte: 'public' }, _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788' ] },
         { field: 'impagency', value: { gte: 'rajasthan', lte: 'rajasthan' }, _id: [ '52b213b38594d8a2be17c788' ] },
         { field: 'impagency', value: { gte: 'road', lte: 'road' }, _id: [ '52b213b38594d8a2be17c786' ] },
         { field: 'impagency', value: { gte: 'trade', lte: 'trade' }, _id: [ '52b213b38594d8a2be17c784' ] },
         { field: 'impagency', value: { gte: 'transport', lte: 'transport' }, _id: [ '52b213b38594d8a2be17c782', '52b213b38594d8a2be17c786' ] },
         { field: 'impagency', value: { gte: 'works', lte: 'works' }, _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788' ] }
       ]
     )
   })
})


test('can aggregate totalamt using underlying index', t => {
  t.plan(1)
  global[indexName].BUCKETFILTER(
    global[indexName].DISTINCT({
      field: 'impagency'
    }).then(result => Promise.all(result.map(global[indexName].BUCKET))),
    global[indexName].SEARCH('board_approval_month:october')
  ).then(result => {
    t.looseEqual(
      result,
      [
        { field: 'impagency', value: { gte: 'administration', lte: 'administration' }, _id: [ '52b213b38594d8a2be17c787' ] },
         { field: 'impagency', value: { gte: 'and', lte: 'and' }, _id: [ '52b213b38594d8a2be17c784', '52b213b38594d8a2be17c786' ] },
         { field: 'impagency', value: { gte: 'communications', lte: 'communications' }, _id: [] },
         { field: 'impagency', value: { gte: 'departmant', lte: 'departmant' }, _id: [ '52b213b38594d8a2be17c788' ] },
         { field: 'impagency', value: { gte: 'education', lte: 'education' }, _id: [] },
         { field: 'impagency', value: { gte: 'energy', lte: 'energy' }, _id: [ '52b213b38594d8a2be17c787' ] },
         { field: 'impagency', value: { gte: 'finance', lte: 'finance' }, _id: [ '52b213b38594d8a2be17c789' ] },
         { field: 'impagency', value: { gte: 'highways', lte: 'highways' }, _id: [ '52b213b38594d8a2be17c786' ] },
         { field: 'impagency', value: { gte: 'industry', lte: 'industry' }, _id: [ '52b213b38594d8a2be17c784' ] },
         { field: 'impagency', value: { gte: 'intensive', lte: 'intensive' }, _id: [ '52b213b38594d8a2be17c783' ] },
         { field: 'impagency', value: { gte: 'labor', lte: 'labor' }, _id: [ '52b213b38594d8a2be17c783' ] },
         { field: 'impagency', value: { gte: 'ministry', lte: 'ministry' }, _id: [ '52b213b38594d8a2be17c784', '52b213b38594d8a2be17c786', '52b213b38594d8a2be17c789' ] },
         { field: 'impagency', value: { gte: 'national', lte: 'national' }, _id: [ '52b213b38594d8a2be17c787' ] },
         { field: 'impagency', value: { gte: 'of', lte: 'of' }, _id: [ '52b213b38594d8a2be17c784', '52b213b38594d8a2be17c786', '52b213b38594d8a2be17c789' ] },
         { field: 'impagency', value: { gte: 'pmu', lte: 'pmu' }, _id: [ '52b213b38594d8a2be17c783' ] },
         { field: 'impagency', value: { gte: 'project', lte: 'project' }, _id: [ '52b213b38594d8a2be17c783' ] },
         { field: 'impagency', value: { gte: 'public', lte: 'public' }, _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788' ] },
         { field: 'impagency', value: { gte: 'rajasthan', lte: 'rajasthan' }, _id: [ '52b213b38594d8a2be17c788' ] },
         { field: 'impagency', value: { gte: 'road', lte: 'road' }, _id: [ '52b213b38594d8a2be17c786' ] },
         { field: 'impagency', value: { gte: 'trade', lte: 'trade' }, _id: [ '52b213b38594d8a2be17c784' ] },
         { field: 'impagency', value: { gte: 'transport', lte: 'transport' }, _id: [ '52b213b38594d8a2be17c786' ] },
         { field: 'impagency', value: { gte: 'works', lte: 'works' }, _id: [ '52b213b38594d8a2be17c783', '52b213b38594d8a2be17c788' ] }
      ]
    )
  })
})

test('JSON BUCKET', t => {
  t.plan(1)
  global[indexName].QUERY({
    BUCKET: {
      field: 'impagency',
      value: 'of'
    }
  }).then(result => {
    t.looseEqual(
      result,
      {
        field: 'impagency', value: { gte: 'of', lte: 'of' },
        _id: [
          '52b213b38594d8a2be17c780',
          '52b213b38594d8a2be17c781',
          '52b213b38594d8a2be17c782',
          '52b213b38594d8a2be17c784',
          '52b213b38594d8a2be17c786',
          '52b213b38594d8a2be17c789'
        ]
      }
    )
  })
})

test('JSON BUCKETFILTER', t => {
  t.plan(1)
  global[indexName].QUERY({
    BUCKETFILTER: {
      BUCKETS: [
        {
          field: 'impagency',
          value: 'of'
        }
      ],
      FILTER: {
        SEARCH: [ 'board_approval_month:october' ]
      }
    }
  }).then(result => {
    t.looseEqual(
      result,
      [
        {
          field: 'impagency', value: { gte: 'of', lte: 'of' },
          _id: [
            '52b213b38594d8a2be17c784',
            '52b213b38594d8a2be17c786',
            '52b213b38594d8a2be17c789'
          ]
        }
      ]
    )
  })
})
  

test('can aggregate totalamt using underlying index', t => {
  t.plan(1)
  global[indexName].BUCKET({
    field: 'impagency',
    value: 'pmu'
  }).then(result => {
    t.looseEqual(
      result,
      {
        field: 'impagency', value: { gte: 'pmu', lte: 'pmu' },
        _id: ['52b213b38594d8a2be17c783']
      }
    )
  })
})


test('can aggregate totalamt using underlying index (JSON BUCKET)', t => {
  t.plan(1)
  global[indexName].QUERY({
    BUCKET: {
      field: 'impagency',
      value: 'pmu'
    }
  }).then(result => {
    t.looseEqual(
      result,
      {
        field: 'impagency', value: { gte: 'pmu', lte: 'pmu' },
        _id: ['52b213b38594d8a2be17c783']
      }
    )
  })
})



test('can aggregate totalamt using underlying index', t => {
  t.plan(1)
  global[indexName].BUCKET({
    field: 'impagency',
    value: {
      gte: 'p',
      lte: 'txx'
    }
  }).then(result => {
    t.looseEqual(
      result,
      {
        field: 'impagency', value: { gte: 'p', lte: 'txx' },
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
    global[indexName].BUCKET({
      field: 'totalamt',
      value: {
        gte: '0', lte: '4999999999999'
      }
    }),
    global[indexName].BUCKET({
      field: 'totalamt',
      value: {
        gte: '5', lte: '9'
      }
    })
  ]).then(result => {
    t.looseEqual(
      result,
      [
        {
          field: 'totalamt', value: { gte: '0', lte: '4999999999999' },
          _id: [
            '52b213b38594d8a2be17c780',
            '52b213b38594d8a2be17c781',
            '52b213b38594d8a2be17c783',
            '52b213b38594d8a2be17c784',
            '52b213b38594d8a2be17c785',
            '52b213b38594d8a2be17c787',
            '52b213b38594d8a2be17c788',
            '52b213b38594d8a2be17c789'
          ]
        },
        {
          field: 'totalamt', value: { gte: '5', lte: '9' },
          _id: [
            '52b213b38594d8a2be17c782', '52b213b38594d8a2be17c786'
          ]
        }
      ]
    )
  })
})
