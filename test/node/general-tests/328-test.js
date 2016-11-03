const Readable = require('stream').Readable
const SearchIndex = require('../../../')
const logLevel = process.env.NODE_ENV || 'error'
const sandboxPath = 'test/sandbox'
const should = require('should')

const s = new Readable({ objectMode: true })
s.push({
  "id":"offering:21",
  "originalId":"21",
  "sortedId":"000000000021",
  "body":[
    "And Filter Operator",
    ""
  ],
  "userId":"b0946ffa20357194688e54b1cf143c9c",
  "productSpecification":"000000000017",
  "categoriesId":[
    "1",
    "5"
  ],
  "categoriesName":[
    "WireCloud Component",
    "WireCloud Operator"
  ],
  "href":"http://store.lab.fiware.org/DSProductCatalog/api/catalogManagement/v2/catalog/19/productOffering/21:(0.1)",
  "name":"And Filter Operator",
  "lifecycleStatus":"Launched",
  "isBundle":false,
  "catalog":"19"
})
s.push(null)

describe('bug 328', function() {

  var si

  it('should index the doc', function (done) {
    var i = 0
    SearchIndex({
      indexPath: sandboxPath + '/328-test',
      logLevel: logLevel
    }, function (err, thisSI) {
      if (err) false.should.eql(true)
      si = thisSI
      s.pipe(si.defaultPipeline())
        .pipe(si.add())
        .on('data', function (data) {
          // nowt
        })
        .on('end', function () {
          true.should.be.exactly(true)
          return done()
        })
    })
  })

  it('simple * search, sorted by ID', function (done) {
    var results = []
    si.search({
      query: [{
        AND: {'*': ['*']}
      }]
    }).on('data', function (data) {
      results.push(data)
    }).on('end', function () {
      results.map(function (item) {
        return item.document.id
      }).should.eql(
        [ 'offering:21' ])
      return done()
    })
  })

  it('search for categoriesId: 5, lifecycleStatus: launched, sorted by ID', function (done) {
    var results = []
    si.search({
      query: [{
        AND: {
          categoriesId: ['5'],
          lifecycleStatus: ['launched']
        }
      }]
    }).on('data', function (data) {
      results.push(data)
    }).on('end', function () {
      results.map(function (item) {
        return item.document.id
      }).should.eql(
        [ 'offering:21' ]
      )
      return done()
    })
  })


})
