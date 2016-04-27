/* global it */
/* global describe */

const sandboxPath = 'test/sandbox'
const SearchIndex = require('../../../')
const should = require('should')
const _ = require('lodash')
var si, si2

const batch = [
  {
    id: '1',
    name: 'Apple Watch',
    description: 'Receive and respond to notiﬁcations in an instant. Watch this amazing watch',
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
    name: 'Versace Men\'s Swiss',
    description: 'Versace Men\'s Swiss Chronograph Mystique Sport Two-Tone Ion-Plated Stainless Steel Bracelet Watch',
    price: '4716',
    age: '8293'
  },
  {
    id: '4',
    name: 'CHARRIOL Men\'s Swiss Alexandre',
    description: 'With CHARRIOLs signature twisted cables, the Alexander C timepiece collection is a must-have piece for lovers of the famed brand.',
    price: '2132',
    age: '33342'
  },
  {
    id: '5',
    name: 'Ferragamo Men\'s Swiss 1898',
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
    description: 'Invicta offers an upscale timepiece that\'s as full of substance as it is style. From the Bolt Zeus collection.',
    price: '8767',
    age: '33342'
  },
  {
    id: '9',
    name: 'Victorinox Night Vision ',
    description: 'Never get left in the dark with Victorinox Swiss Army\'s Night Vision watch. First at Macy\'s!',
    price: '1000',
    age: '33342'
  },
  {
    id: '10',
    name: 'Armani Swiss Moon Phase',
    description: 'Endlessly sophisticated in materials and design, this Emporio Armani Swiss watch features high-end timekeeping with moon phase movement and calendar tracking.',
    price: '30000',
    age: '33342'
  },
]


it('should do some simple indexing and not store "description field"', function (done) {
  SearchIndex({
    indexPath: sandboxPath + '/fieldstostore-test',
    logLevel: 'warn'
  }, function(err, thisSI){
    si = thisSI
    si.add(batch, {
      fieldOptions: [{
        fieldName: 'description',
        store: false
      }]
    }, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })
})

it('documents returned by search should not have description fields', function (done) {
  var q = {}
  q.query = {
    AND: {'*': ['*']}
  }
  si.search(q, function (err, searchResults) {
    should.exist(searchResults)
    ;(err === null).should.be.exactly(true)
    searchResults.hits.length.should.be.exactly(10)
    searchResults.totalHits.should.be.exactly(10)
    searchResults.hits.map(function (hit) {
      return hit.document
    }).should.eql([
      { id: '9', name: 'Victorinox Night Vision ', price: '1000', age: '33342' },
      { id: '8', name: 'Invicta Bolt Zeus ', price: '8767', age: '33342' },
      { id: '7', name: 'TW Steel', price: '33333', age: '33342' },
      { id: '6', name: 'Bulova AccuSwiss', price: '1313', age: '33342' },
      { id: '5', name: 'Ferragamo Men\'s Swiss 1898', price: '99999', age: '33342' },
      { id: '4', name: 'CHARRIOL Men\'s Swiss Alexandre', price: '2132', age: '33342' },
      { id: '3', name: 'Versace Men\'s Swiss', price: '4716', age: '8293' },
      { id: '2', name: 'Victorinox Swiss Army', price: '99', age: '33342' },
      { id: '10', name: 'Armani Swiss Moon Phase', price: '30000', age: '33342' },
      { id: '1', name: 'Apple Watch', price: '20002', age: '346' }
    ])
    done()
  })
})

it('should do some simple indexing and ONLY store "description field"', function (done) {
  SearchIndex({
    indexPath: sandboxPath + '/fieldstostore-test',
    logLevel: 'warn',
    store: false
  }, function(err, thisSI){
    si2 = thisSI
    si2.add(batch, {
      fieldOptions: [{
        fieldName: 'name',
        store: true
      }]
    }, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })
})

it('documents returned by search should not have description fields', function (done) {
  var q = {}
  q.query = {
    AND: {'*': ['*']}
  }
  si2.search(q, function (err, searchResults) {
    should.exist(searchResults)
    ;(err === null).should.be.exactly(true)
    searchResults.hits.length.should.be.exactly(10)
    searchResults.totalHits.should.be.exactly(10)
    searchResults.hits.map(function(hit) {
      return hit.document
    }).should.eql([
      { name: 'Victorinox Night Vision ' },
      { name: 'Invicta Bolt Zeus ' },
      { name: 'TW Steel' },
      { name: 'Bulova AccuSwiss' },
      { name: 'Ferragamo Men\'s Swiss 1898' },
      { name: 'CHARRIOL Men\'s Swiss Alexandre' },
      { name: 'Versace Men\'s Swiss' },
      { name: 'Victorinox Swiss Army' },
      { name: 'Armani Swiss Moon Phase' },
      { name: 'Apple Watch' }
    ])
    done()
  })
})

