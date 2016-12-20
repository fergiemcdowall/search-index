/* global describe */
/* global it */

const JSONStream = require('JSONStream')
const Readable = require('stream').Readable
const SearchIndex = require('../../../')
const logLevel = process.env.NODE_ENV || 'error'
const sandbox = process.env.SANDBOX || 'test/sandbox'
const should = require('should')

var si

const batch = [
  {
    id: '1',
    name: 'Apple Watch',
    description: 'Receive and respond to notiﬁcations in an instant.',
    price: '20002',
    age: '346'
  },
  {
    id: '2',
    name: 'Victorinox Swiss Army',
    description: 'You have the power to keep time moving with this Airboss automatic watch.',
    price: '99',
    age: '33342'
  },
  {
    id: '3',
    name: "Versace Men's Swiss",
    description: "Versace Men's Swiss Chronograph Mystique Sport Two-Tone Ion-Plated Stainless Steel Bracelet Watch",
    price: '4716',
    age: '8293'
  },
  {
    id: '4',
    name: "CHARRIOL Men's Swiss Alexandre",
    description: 'With CHARRIOLs signature twisted cables, the Alexander C timepiece collection is a must-have piece for lovers of the famed brand.',
    price: '2132',
    age: '33342'
  },
  {
    id: '5',
    name: "Ferragamo Men's Swiss 1898",
    description: 'The 1898 timepiece collection from Ferragamo offers timeless luxury.',
    price: '99999',
    age: '33342'
  },
  {
    id: '6',
    name: 'Bulova AccuSwiss',
    description: 'The Percheron Treble timepiece from Bulova AccuSwiss sets the bar high with sculpted cases showcasing sporty appeal. A Manchester United® special edition.',
    price: '1313',
    age: '33342'
  },
  {
    id: '7',
    name: 'TW Steel',
    description: 'A standout timepiece that boasts a rich heritage and high-speed design. This CEO Tech watch from TW Steel sets the standard for elite. Armani',
    price: '33333',
    age: '33342'
  },
  {
    id: '8',
    name: 'Invicta Bolt Zeus ',
    description: "Invicta offers an upscale timepiece that's as full of substance as it is style. From the Bolt Zeus collection.",
    price: '8767',
    age: '33342'
  },
  {
    id: '9',
    name: 'Victorinox Night Vision ',
    description: "Never get left in the dark with Victorinox Swiss Army's Night Vision watch. First at Macy's!",
    price: '1000',
    age: '33342'
  },
  {
    id: '10',
    name: 'Armani Swiss Moon Phase',
    description: 'Endlessly sophisticated in materials and design, this Emporio Armani Swiss watch features high-end timekeeping with moon phase movement and calendar tracking.',
    price: '30000',
    age: '33342'
  }
]

var s = new Readable({ objectMode: true })
batch.forEach(function (item) {
  s.push(item)
})
s.push(null)

describe('scanning: ', function () {
  it('initialize a search index', function (done) {
    SearchIndex({
      indexPath: sandbox + '/si-scan',
      logLevel: logLevel
    }, function (err, thisSi) {
      ;(err === null).should.be.exactly(true)
      si = thisSi
      s.pipe(si.defaultPipeline())
        .pipe(si.add())
        .on('data', function (data) {

        })
        .on('end', function () {
          true.should.be.exactly(true)
          return done()
        })
    })
  })

  it('do a simple scan', function (done) {
    var results = []
    si.scan({
      query: {
        AND: {'*': ['swiss', 'watch']}
      }
    }).on('data', function (doc) {
      results.push(doc.id)
    }).on('end', function () {
      results.should.eql([ '10', '2', '3', '9' ])
      done()
    })
  })

  it('do a simple scan with one word', function (done) {
    var results = []
    si.scan({
      query: {
        AND: {'*': ['watch']}
      }
    }).on('data', function (doc) {
      results.push(doc.id)
    }).on('end', function () {
      results.should.eql([ '1', '10', '2', '3', '7', '9' ])
      done()
    })
  })

  it('do a simple scan with one word on a given field', function (done) {
    var results = []
    si.scan({
      query: {
        AND: {'name': ['swiss']}
      }
    }).on('data', function (doc) {
      results.push(doc.id)
    }).on('end', function () {
      results.should.eql([ '10', '2', '3', '4', '5' ])
      done()
    })
  })

  // // TODO: make filters work

  it('do a simple scan with one word on a given field and filter', function (done) {
    var results = []
    si.scan({
      query: {
        AND: {
          name: ['swiss'],
          price: [{gte: '30000', lte: '9'}]
        }
      }
    }).on('data', function (doc) {
      results.push(doc.id)
    }).on('end', function () {
      results.should.eql([ '10', '3' ])
      done()
    })
  })
})
