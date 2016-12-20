/* global describe */
/* global it */

const JSONStream = require('JSONStream')
const Readable = require('stream').Readable
const SearchIndex = require('../../../')
const logLevel = process.env.NODE_ENV || 'error'
const sandboxPath = 'test/sandbox'
const should = require('should')

var si, si2

should

var getStream = function () {
  const s = new Readable({ objectMode: true })
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
  return s
}

describe('storing fields: ', function () {
  it('should do some simple indexing and not store "description field"', function (done) {
    var i = 0
    SearchIndex({
      indexPath: sandboxPath + '/fieldstostore-test',
      logLevel: logLevel
    }, function (err, thisSI) {
      if (err) false.should.eql(true)
      si = thisSI
      getStream()
        .pipe(si.defaultPipeline({
          fieldOptions: {
            description: {
              storeable: false
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

  it('documents returned by search should not have description fields', function (done) {
    var results = [
      { id: '9', name: 'Victorinox Night Vision ', price: '1000', age: '33342' },
      { id: '8', name: 'Invicta Bolt Zeus ', price: '8767', age: '33342' },
      { id: '7', name: 'TW Steel', price: '33333', age: '33342' },
      { id: '6', name: 'Bulova AccuSwiss', price: '1313', age: '33342' },
      { id: '5', name: "Ferragamo Men's Swiss 1898", price: '99999', age: '33342' },
      { id: '4', name: "CHARRIOL Men's Swiss Alexandre", price: '2132', age: '33342' },
      { id: '3', name: "Versace Men's Swiss", price: '4716', age: '8293' },
      { id: '2', name: 'Victorinox Swiss Army', price: '99', age: '33342' },
      { id: '10', name: 'Armani Swiss Moon Phase', price: '30000', age: '33342' },
      { id: '1', name: 'Apple Watch', price: '20002', age: '346' }
    ]
    si.search({
      query: [{
        AND: {'*': ['*']}
      }]
    }).on('data', function (data) {
      results.shift().should.eql(data.document)
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })

  it('should do some simple indexing and ONLY store "name field"', function (done) {
    var i = 0
    SearchIndex({
      indexPath: sandboxPath + '/fieldstostore2-test',
      logLevel: logLevel,
      storeable: false
    }, function (err, thisSI) {
      if (err) false.should.eql(true)
      si2 = thisSI
      getStream()
        .pipe(si2.defaultPipeline({
          fieldOptions: {
            name: {
              storeable: true
            }
          }
        }))
        .pipe(si2.add())
        .on('data', function (data) {
        })
        .on('end', function () {
          true.should.be.exactly(true)
          return done()
        })
    })
  })

  it('documents returned by search should not have description fields', function (done) {
    var results = [
      { name: 'Victorinox Night Vision ', id: '9' },
      { name: 'Invicta Bolt Zeus ', id: '8' },
      { name: 'TW Steel', id: '7' },
      { name: 'Bulova AccuSwiss', id: '6' },
      { name: "Ferragamo Men's Swiss 1898", id: '5' },
      { name: "CHARRIOL Men's Swiss Alexandre", id: '4' },
      { name: "Versace Men's Swiss", id: '3' },
      { name: 'Victorinox Swiss Army', id: '2' },
      { name: 'Armani Swiss Moon Phase', id: '10' },
      { name: 'Apple Watch', id: '1' }
    ]
    si2.search({
      query: [{
        AND: {'*': ['*']}
      }]
    }).on('data', function (data) {
      results.shift().should.eql(data.document)
    }).on('end', function () {
      results.length.should.be.exactly(0)
      return done()
    })
  })
})
