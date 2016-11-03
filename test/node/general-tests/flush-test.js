const JSONStream = require('JSONStream')
const Readable = require('stream').Readable
const SearchIndex = require('../../../')
const logLevel = process.env.NODE_ENV || 'error'
const sandboxPath = 'test/sandbox'
const should = require('should')
const sw = require('stopword')

should

const s = new Readable({ objectMode: true })

var si

s.push({
  id: '1',
  name: 'Apple Watch',
  description: 'Receive and respond to notiﬁcations in an instant.',
  price: '20002',
  age: '346'
})
s.push({
  id: '2',
  name: 'Victorinox Swiss Army',
  description: 'You have the power to keep time moving with this Airboss automatic watch.',
  price: '99',
  age: '33342'
})
s.push({
  id: '3',
  name: "Versace Men's Swiss",
  description: "Versace Men's Swiss Chronograph Mystique Sport Two-Tone Ion-Plated Stainless Steel Bracelet Watch",
  price: '4716',
  age: '8293'
})
s.push({
  id: '4',
  name: "CHARRIOL Men's Swiss Alexandre",
  description: 'With CHARRIOLs signature twisted cables, the Alexander C timepiece collection is a must-have piece for lovers of the famed brand.',
  price: '2132',
  age: '33342'
})
s.push({
  id: '5',
  name: "Ferragamo Men's Swiss 1898",
  description: 'The 1898 timepiece collection from Ferragamo offers timeless luxury.',
  price: '99999',
  age: '33342'
})
s.push({
  id: '6',
  name: 'Bulova AccuSwiss',
  description: 'The Percheron Treble timepiece from Bulova AccuSwiss sets the bar high with sculpted cases showcasing sporty appeal. A Manchester United® special edition.',
  price: '1313',
  age: '33342'
})
s.push({
  id: '7',
  name: 'TW Steel',
  description: 'A standout timepiece that boasts a rich heritage and high-speed design. This CEO Tech watch from TW Steel sets the standard for elite. Armani',
  price: '33333',
  age: '33342'
})
s.push({
  id: '8',
  name: 'Invicta Bolt Zeus ',
  description: "Invicta offers an upscale timepiece that's as full of substance as it is style. From the Bolt Zeus collection.",
  price: '8767',
  age: '33342'
})
s.push({
  id: '9',
  name: 'Victorinox Night Vision ',
  description: "Never get left in the dark with Victorinox Swiss Army's Night Vision watch. First at Macy's!",
  price: '1000',
  age: '33342'
})
s.push({
  id: '10',
  name: 'Armani Swiss Moon Phase',
  description: 'Endlessly sophisticated in materials and design, this Emporio Armani Swiss watch features high-end timekeeping with moon phase movement and calendar tracking.',
  price: '30000',
  age: '33342'
})
s.push(null)

describe('boosting', function () {
  it('should do some simple indexing', function (done) {
    var i = 0
    SearchIndex({
      indexPath: sandboxPath + '/flush-test',
      logLevel: logLevel,
      stopwords: sw.en
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
        [ '9', '8', '7', '6', '5', '4', '3', '2', '10', '1' ])
      return done()
    })
  })

  it('flush index', function (done) {
    si.flush(function (err) {
      if (err) true.should.be.exactly(false)
      return done()
    })
  })

  it('index should be flushed', function (done) {
    var i = 0
    si.search({
      query: [{
        AND: {'*': ['*']}
      }]
    }).on('data', function (data) {
      i++
    }).on('end', function () {
      i.should.be.exactly(0)
      return done()
    })
  })

})
