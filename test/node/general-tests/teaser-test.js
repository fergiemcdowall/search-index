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


it('should do some simple indexing', function (done) {
  SearchIndex({
    indexPath: sandboxPath + '/teaser-test',
    logLevel: 'warn'
  }, function(err, thisSI){
    si = thisSI
    si.add(batch, {}, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })
})


it('set simple one word teaser on * field', function (done) {
  si.search({
    query: {
      AND: [{'*': ['watch']}]
    },
    teaser: 'description'
  }, function (err, results) {
    ;(err === null).should.be.exactly(true)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '1', '9', '7', '3', '2', '10' ]
    )
    results.hits[0].document.teaser.should.equal('Receive and respond to notiﬁcations in an instant. <b>watch</b> this amazing <b>watch</b>')
    done()
  })
})


it('set simple two word teaser on * field', function (done) {
  si.search({
    query: {
      AND: [{'*': ['swiss', 'watch']}]
    },
    teaser: 'description'
  }, function (err, results) {
    ;(err === null).should.be.exactly(true)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '3', '10', '9', '2' ]
    )
    results.hits[0].document.teaser.should.equal('Versace Men\'s <b>swiss</b> Chronograph Mystique Sport Two-Tone Ion-Plated Stainless Steel Bracelet <b>watch</b>')
    done()
  })
})

it('set simple two word teaser on * field', function (done) {
  si.search({
    query: {
      AND: [{'*': ['swiss', 'watch']}]
    },
    teaser: 'description'
  }, function (err, results) {
    ;(err === null).should.be.exactly(true)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '3', '10', '9', '2' ]
    )
    results.hits[0].document.teaser.should.equal('Versace Men\'s <b>swiss</b> Chronograph Mystique Sport Two-Tone Ion-Plated Stainless Steel Bracelet <b>watch</b>')
    done()
  })
})

it('set simple two word teaser on * field', function (done) {
  si.search({
    query: {
      AND: [{'*': ['swiss', 'watch']}]
    },
    teaser: 'description'
  }, function (err, results) {
    ;(err === null).should.be.exactly(true)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '3', '10', '9', '2' ]
    )
    results.hits[0].document.teaser.should.equal('Versace Men\'s <b>swiss</b> Chronograph Mystique Sport Two-Tone Ion-Plated Stainless Steel Bracelet <b>watch</b>')
    done()
  })
})

it('set simple two word teaser on * field', function (done) {
  si.search({
    query: {
      AND: [{'*': ['swiss', 'watch']}]
    },
    teaser: 'description',
    teaserHighlighter: function (string) {
      return '<booom>' + string + '</booom>'
    }
  }, function (err, results) {
    ;(err === null).should.be.exactly(true)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '3', '10', '9', '2' ]
    )
    results.hits[0].document.teaser.should.equal('Versace Men\'s <booom>swiss</booom> Chronograph Mystique Sport Two-Tone Ion-Plated Stainless Steel Bracelet <booom>watch</booom>')
    done()
  })
})
