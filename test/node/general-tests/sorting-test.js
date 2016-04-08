/* global it */
/* global describe */

const sandboxPath = 'test/sandbox'
const SearchIndex = require('../../../')
const should = require('should')
const _ = require('lodash')
var si

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
    description: 'A standout timepiece that boasts a rich heritage and high-speed design. This CEO Tech watch from TW Steel sets the standard for elite.',
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


it('should do some simple indexing', function (done) {
  SearchIndex({
    indexPath: sandboxPath + '/sorting-test',
    logLevel: 'warn'
  }, function(err, thisSI){
    si = thisSI
    si.add(batch, {}, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })
})


it('simple search, sorted by ID', function (done) {
  si.search({
    query: {
      AND: {'*': ['*']}
    }
  }, function (err, results) {
    (err === null).should.be.exactly(true)
    should.exist(results)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '9', '8', '7', '6', '5', '4', '3', '2', '10', '1' ])
    done()
  })
})


it('simple search, sorted by relevance', function (done) {
  si.search({
    query: {
      AND: {'*': ['watch']}
    }
  }, function (err, results) {
    ;(err === null).should.be.exactly(true)
    should.exist(results)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '1', '9', '7', '3', '2', '10' ])
    done()
  })
})

it('simple search, sorted by price', function (done) {
  si.search({
    query: {
      AND: {'*': ['watch']}
    },
    sort:['price', 'desc']
  }, function (err, results) {
    (err === null).should.be.exactly(true)
    should.exist(results)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '7', '10', '1', '3', '9', '2' ])
    results.hits.map(function (item) { return item.document.price }).should.eql(
      [ '33333', '30000', '20002', '4716', '1000', '99' ])
    done()
  })
})

it('simple search, two tokens, sorted by price', function (done) {
  si.search({
    query: {
      AND: {'*': ['watch', 'swiss'] }
    },
    sort:['price', 'desc']
  }, function (err, results) {
    (err === null).should.be.exactly(true)
    should.exist(results)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '10', '3', '9', '2' ])
    results.hits.map(function (item) { return item.document.price }).should.eql(
      [ '30000', '4716', '1000', '99' ] )
    done()
  })
})

it('simple search, two tokens, sorted by price pagesize = 2', function (done) {
  si.search({
    query: {
      AND: {'*': ['watch', 'swiss']}
    },
    pageSize: 2,
    sort:['price', 'desc']
  }, function (err, results) {
    (err === null).should.be.exactly(true)
    should.exist(results)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '10', '3' ])
    results.hits.map(function (item) { return item.document.price }).should.eql(
      [ '30000', '4716' ] )
    done()
  })
})


it('simple search, two tokens, sorted by price pagesize = 2, offset = 1', function (done) {
  si.search({
    query: {
      AND: {'*': ['watch', 'swiss']}
    },
    offset: 1,
    pageSize: 2,
    sort:['price', 'desc']
  }, function (err, results) {
    ;(err === null).should.be.exactly(true)
    should.exist(results)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '3', '9' ])
    results.hits.map(function (item) { return item.document.price }).should.eql(
      [ '4716', '1000' ] )
    done()
  })
})

it('simple search, two tokens, sorted by price pagesize = 2, offset = 1, sort asc', function (done) {
  si.search({
    query: {
      AND: {'*': ['watch', 'swiss']}
    },
    offset: 1,
    pageSize: 2,
    sort:['price', 'asc']
  }, function (err, results) {
    (err === null).should.be.exactly(true)
    should.exist(results)
//    console.log(JSON.stringify(results.hits, null, 2))
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '9', '3' ])
    results.hits.map(function (item) { return item.document.price }).should.eql(
      [ '1000', '4716' ] )
    done()
  })
})
