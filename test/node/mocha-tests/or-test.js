/* global describe */
/* global it */

const SearchIndex = require('../../../')
const logLevel = process.env.NODE_ENV || 'error'
const s = require('stream').Readable({ objectMode: true})
const sandboxPath = 'test/sandbox'
const should = require('should')
const stopwords = require('stopword').en

should

var si

s.push({
  id: '1',
  name: 'Apple Watch',
  description: 'Receive and respond to notiﬁcations in an instant. Watch this amazing watch',
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

describe('OR-ing: ', function () {
  it('should do some simple indexing', function (done) {
    SearchIndex({
      indexPath: sandboxPath + '/or-test',
      logLevel: logLevel,
      stopwords: stopwords
    }, function (err, thisSI) {
      ;(!err).should.be.exactly(true)
      si = thisSI
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

  it('simple * search, sorted by ID', function (done) {
    var results = [ '9', '8', '7', '6', '5', '4', '3', '2', '10', '1' ]
    si.search({
      query: [{
        AND: { '*': [ '*' ] }
      }]
    }).on('data', function (data) {
      data.document.id.should.be.exactly(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('simple search, sorted by ID', function (done) {
    var results = [ '7', '10' ]
    si.search({
      query: [{
        AND: { '*': [ 'armani', 'watch' ] }
      }]
    }).on('data', function (data) {
      data.document.id.should.be.exactly(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('search for Armarni AND TW', function (done) {
    var results = [ '7', '10' ]
    si.search({
      query: [{
        AND: { '*': [ 'armani', 'watch' ] }
      }, {
        AND: { '*': [ 'tw', 'watch' ] }
      }]
    }).on('data', function (data) {
      data.id.should.be.exactly(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('search for Armarni AND Watch OR Victorinox AND swiss OR TW AND watch', function (done) {
    var results = [ '7', '9', '2', '10' ]
    si.search({
      query: [{
        AND: { '*': [ 'armani', 'watch' ] }
      }, {
        AND: { '*': [ 'victorinox', 'swiss' ] }
      }, {
        AND: {
          description: [ 'tw', 'watch' ],
          name: [ 'tw' ]
        }
      }]
    }).on('data', function (data) {
      data.id.should.be.exactly(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('search for watch NOT armani', function (done) {
    var results = [ '9', '3', '2', '1' ]
    si.search({
      query: [{
        AND: { '*': [ 'watch' ] },
        NOT: { '*': [ 'armani' ] }
      }]
    }).on('data', function (data) {
      data.id.should.be.exactly(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('search for watch NOT apple in name field', function (done) {
    var results = [ '9', '7', '3', '2', '10' ]
    si.search({
      query: [{
        AND: { '*': [ 'watch' ] },
        NOT: { 'name': [ 'apple' ] }
      }]
    }).on('data', function (data) {
      data.id.should.be.exactly(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('search for watch NOT apple in title field, but then add "apple watch" back in through an OR condition', function (done) {
    var results = [ '1', '9', '7', '3', '2', '10' ]
    si.search({
      query: [{
        AND: { '*': [ 'watch' ] },
        NOT: { 'name': [ 'apple' ] }
      }, {
        AND: { '*': [ 'apple', 'watch' ] }
      }]
    }).on('data', function (data) {
      data.id.should.be.exactly(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('search for armani NOT TW', function (done) {
    var results = [ '10' ]
    si.search({
      query: [{
        AND: { '*': [ 'armani' ] },
        NOT: { '*': [ 'tw' ] }
      }]
    }).on('data', function (data) {
      data.id.should.be.exactly(results.shift())
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })
})
