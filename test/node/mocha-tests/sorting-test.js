/* global describe */
/* global it */

const Readable = require('stream').Readable
const SearchIndex = require('../../../')
const logLevel = process.env.NODE_ENV || 'error'
const sandboxPath = 'test/sandbox'
const should = require('should')
const sw = require('stopword')

var si

const batch = [
  {
    id: '1',
    name: 'Apple Watch',
    description: 'Receive and respond to notiﬁcations in an instant. Watch this amazing watch',
    price: 20002,
    age: 346
  },
  {
    id: '2',
    name: 'Victorinox Swiss Army',
    description: 'You have the power to keep time moving with this Airboss automatic watch',
    price: 99,
    age: 33342
  },
  {
    id: '3',
    name: "Versace Men's Swiss",
    description: "Versace Men's Swiss Chronograph Mystique Sport Two-Tone Ion-Plated Stainless Steel Bracelet Watch",
    price: 4716,
    age: 8293
  },
  {
    id: '4',
    name: "CHARRIOL Men's Swiss Alexandre",
    description: 'With CHARRIOLs signature twisted cables, the Alexander C timepiece collection is a must-have piece for lovers of the famed brand.',
    price: 2132,
    age: 33342
  },
  {
    id: '5',
    name: "Ferragamo Men's Swiss 1898",
    description: 'The 1898 timepiece collection from Ferragamo offers timeless luxury.',
    price: 99999,
    age: 33342
  },
  {
    id: '6',
    name: 'Bulova AccuSwiss',
    description: 'The Percheron Treble timepiece from Bulova AccuSwiss sets the bar high with sculpted cases showcasing sporty appeal. A Manchester United® special edition.',
    price: 1313,
    age: 33342
  },
  {
    id: '7',
    name: 'TW Steel',
    description: 'A standout timepiece that boasts a rich heritage and high-speed design. This CEO Tech watch from TW Steel sets the standard for elite.',
    price: 33333,
    age: 33342
  },
  {
    id: '8',
    name: 'Invicta Bolt Zeus ',
    description: "Invicta offers an upscale timepiece that's as full of substance as it is style. From the Bolt Zeus collection.",
    price: 8767,
    age: 33342
  },
  {
    id: '9',
    name: 'Victorinox Night Vision ',
    description: "Never get left in the dark with Victorinox Swiss Army's Night Vision watch. First at Macy's! BOOM BOOM BOOM",
    price: 1000,
    age: 33342
  },
  {
    id: '10',
    name: 'Armani Swiss Moon Phase',
    description: 'Endlessly sophisticated in materials and design, this Emporio Armani Swiss watch features high-end timekeeping with moon phase movement and calendar tracking.',
    price: 30000,
    age: 33342
  }
]

const s = new Readable({ objectMode: true })
batch.forEach(function (item) {
  s.push(item)
})
s.push(null)

describe('sorting: ', function () {
  it('should do some simple indexing', function (done) {
    SearchIndex({
      indexPath: sandboxPath + '/sorting-test',
      logLevel: logLevel,
      stopwords: sw.en
    }, function (err, thisSI) {
      should(err).not.ok
      si = thisSI
      s.pipe(si.defaultPipeline({
        fieldOptions: {
          price: {
            sortable: true
          },
          name: {
            sortable: true,
            separator: '%'  // index field as one token
          }
        }
      }))
        .pipe(si.add())
        .on('data', function (data) {

        })
        .on('end', function () {
          true.should.be.exactly(true)
          return done()
        })
    })
  })

  it('simple search, sorted by ID', function (done) {
    var results = [ '9', '8', '7', '6', '5', '4', '3', '2', '10', '1' ]
    si.search({
      query: [{
        AND: {'*': ['*']}
      }]
    }).on('data', function (data) {
      data.id.should.eql(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('simple search, sorted by relevance', function (done) {
    var results = [ '7', '3', '2', '10', '1', '9' ]
    si.search({
      query: [{
        AND: {'*': ['watch']}
      }]
    }).on('data', function (data) {
      data.id.should.eql(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('simple search, sorted by price', function (done) {
    var i = 0
    var results = [ '2', '3', '7', '10', '1', '9' ]
    var prices = [ '99', '4716', '33333', '30000', '20002', '1000' ]
    si.search({
      query: {
        AND: {'*': ['watch']}
      },
      sort: {
        field: 'price',
        direction: 'desc'
      }
    }).on('data', function (data) {
      i++
      data.id.should.eql(results.shift())
      data.score.should.eql(prices.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      prices.length.should.be.exactly(0)
      i.should.be.exactly(6)
      return done()
    })
  })

  it('simple search, sorted by name', function (done) {
    var i = 0
    var results = [ '1', '10', '7', '3', '9', '2' ]
    var names = [
      'Apple Watch',
      'Armani Swiss Moon Phase',
      'TW Steel',
      "Versace Men's Swiss",
      'Victorinox Night Vision ',
      'Victorinox Swiss Army'
    ]
    si.search({
      query: {
        AND: {'*': ['watch']}
      },
      sort: {
        field: 'name',
        direction: 'asc'
      }
    }).on('data', function (data) {
      i++
      data.id.should.eql(results.shift())
      data.document.name.should.eql(names.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      names.length.should.be.exactly(0)
      i.should.be.exactly(6)
      return done()
    })
  })

  it('simple search, two tokens, sorted by price', function (done) {
    var i = 0
    var results = [ '3', '10', '9' ]
    var prices = [ '4716', '30000', '1000' ]
    si.search({
      query: {
        AND: {'*': [ 'watch', 'swiss' ]}
      },
      sort: {
        field: 'price',
        direction: 'desc'
      }
    }).on('data', function (data) {
      i++
      data.id.should.eql(results.shift())
      data.score.should.eql(prices.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      prices.length.should.be.exactly(0)
      i.should.be.exactly(3)
      return done()
    })
  })

  it('simple search, two tokens, sorted by price pagesize = 2', function (done) {
    var i = 0
    var results = [ '3', '10' ]
    var prices = [ '4716', '30000' ]
    si.search({
      query: {
        AND: {'*': ['watch', 'swiss']}
      },
      sort: {
        field: 'price',
        direction: 'desc'
      },
      pageSize: 2
    }).on('data', function (data) {
      i++
      data.id.should.eql(results.shift())
      data.score.should.eql(prices.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      prices.length.should.be.exactly(0)
      i.should.be.exactly(2)
      return done()
    })
  })

  it('simple search, two tokens, sorted by price pagesize = 2, offset = 1', function (done) {
    var i = 0
    var results = [ '10', '9' ]
    var prices = [ '30000', '1000' ]
    si.search({
      query: {
        AND: {'*': ['watch', 'swiss']}
      },
      sort: {
        field: 'price',
        direction: 'desc'
      },
      offset: 1,
      pageSize: 2
    }).on('data', function (data) {
      i++
      data.id.should.eql(results.shift())
      data.score.should.eql(prices.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      prices.length.should.be.exactly(0)
      i.should.be.exactly(2)
      return done()
    })
  })

  it('simple search, two tokens, sorted by price pagesize = 2, offset = 1, sort asc', function (done) {
    var i = 0
    var results = [ '10', '3' ]
    var prices = [ '30000', '4716' ]
    si.search({
      query: {
        AND: {
          '*': [ 'watch', 'swiss' ]
        }
      },
      sort: {
        field: 'price',
        direction: 'asc'
      },
      offset: 1,
      pageSize: 2
    }).on('data', function (data) {
      i++
      data.id.should.eql(results.shift())
      data.score.should.eql(prices.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      prices.length.should.be.exactly(0)
      i.should.be.exactly(2)
      return done()
    })
  })

  it('calculate total hits', function (done) {
    si.totalHits('swiss watch', function (err, totalHits) {
      totalHits.should.be.exactly(3)
      return done()
    })
  })

  it('calculate total hits', function (done) {
    si.totalHits({
      query: {
        AND: {
          '*': [ 'watch', 'swiss' ]
        }
      }
    }, function (err, totalHits) {
      totalHits.should.be.exactly(3)
      return done()
    })
  })

})
