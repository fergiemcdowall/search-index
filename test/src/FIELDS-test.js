import test from 'tape'

const { SearchIndex } = await import(
  '../../src/' + process.env.SI_TEST_ENTRYPOINT
)

const global = {}
const sandbox = 'test/sandbox/'
const indexName = sandbox + 'FIELDS'

test('create a search index', async t => {
  t.plan(1)
  try {
    global[indexName] = await new SearchIndex({ name: indexName })
    t.ok(global[indexName])
  } catch (e) {
    t.error(e)
  }
})

test('can add data', t => {
  const data = [
    {
      _id: 0,
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'Volvo'
    },
    {
      _id: 1,
      make: 'BMW',
      manufacturer: 'Volvo',
      brand: 'Volvo',
      wheels: {
        tyres: 'goodyear',
        rims: 'alloy'
      }
    },
    {
      _id: 2,
      make: 'Tesla',
      manufacturer: 'Tesla',
      brand: 'Volvo',
      colours: [
        {
          roof: {
            color: ['red', 'yellow'],
            type: 'gloss'
          }
        },
        {
          bonnet: {
            color: ['green', 'yellow'],
            type: 'matt'
          }
        }
      ]
    },
    {
      _id: 3,
      make: 'Tesla',
      manufacturer: 'Volvo',
      brand: 'BMW'
    }
  ]
  t.plan(1)
  global[indexName].PUT(data).then(t.pass)
})

test('simple FIELDS', t => {
  t.plan(1)
  global[indexName].FIELDS().then(res => {
    t.deepEqual(res, [
      'brand',
      'colours.bonnet.color',
      'colours.bonnet.type',
      'colours.roof.color',
      'colours.roof.type',
      'make',
      'manufacturer',
      'wheels.rims',
      'wheels.tyres'
    ])
  })
})

// leave this one out for now
// test('simple FIELDS (JSON)', t => {
//   t.plan(1)
//   global[indexName].QUERY({
//     FIELDS: true
//   }).then(res => {
//     t.deepEqual(res, [
//       'brand',
//       'colours.bonnet.color',
//       'colours.bonnet.type',
//       'colours.roof.color',
//       'colours.roof.type',
//       'make',
//       'manufacturer',
//       'wheels.rims',
//       'wheels.tyres'
//     ])
//   })
// })

test('simple FIELDS (JSON)', t => {
  t.plan(1)
  global[indexName].FIELDS().then(res => {
    t.deepEqual(res, [
      'brand',
      'colours.bonnet.color',
      'colours.bonnet.type',
      'colours.roof.color',
      'colours.roof.type',
      'make',
      'manufacturer',
      'wheels.rims',
      'wheels.tyres'
    ])
  })
})
