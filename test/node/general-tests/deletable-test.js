const sandboxPath = 'test/sandbox'
const SearchIndex = require('../../../')
const should = require('should')
const _ = require('lodash')
const levelup = require('levelup')

var db
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


it('should do some simple indexing with deletable set to "false"', function (done) {
  levelup(sandboxPath + '/deletable-test', {
    valueEncoding: 'json'
  }, function (err, thisDb) {
    db = thisDb
    SearchIndex({
      indexes: db,
      deletable: false,
      logLevel: 'warn'
    }, function(err, thisSI){
      si = thisSI
      si.add(batch, {}, function (err) {
        ;(err === null).should.be.exactly(true)
        done()
      })
    })
  })
})


it('should not contain a delete key for the first doc in the batch', function (done) {
  var id = batch[0].id
  db.get('DELETE-DOCUMENT￮' + id, function(err, value) {
    ;(typeof value).should.be.exactly('undefined')
    err.toString().should.be.exactly(
      'NotFoundError: Key not found in database [DELETE-DOCUMENT￮1]'
    )
    done()
  })
})

it('should be able to search as normal', function (done) {
  si.search({
    query: {
      AND: [{'*': ['watch']}]
    }
  }, function (err, results) {
    results.hits.map(function (hit) {
      return hit.id
    }).should.be.eql([ '1', '9', '7', '3', '2', '10' ])
    done()
  })
})

it('deleting should return a nice error', function (done) {
  si.del('1', function (err) {
    err.toString().should.be.exactly(
      'Error: this index is non-deleteable, and some of the documents you are deleting have IDs that are already present in the index. Either reinitialize index with "deletable: true" or alter the IDs of the new documents'
    )
    done()
  })
})


it('adding should return a nice error if doc with same ID is in the index', function (done) {
  si.add([{
    id: '1',
    body: 'this doc should not be indexed since a doc with id=1 exists and deletable is false'
  }], {}, function (err) {
    err.toString().should.be.exactly(
      'Error: this index is non-deleteable, and some of the documents you are deleting have IDs that are already present in the index. Either reinitialize index with "deletable: true" or alter the IDs of the new documents'
    )
    done()
  })
})

