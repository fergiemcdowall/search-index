const Readable = require('stream').Readable
const SearchIndex = require('../../../')
const logLevel = process.env.LOG_LEVEL || 'warn'
const sandbox = process.env.SANDBOX || 'test/sandbox'
const test = require('tape')

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

const s = new Readable({ objectMode: true })
batch.forEach(function (item) {
  s.push(item)
})
s.push(null)

test('initialize a search index', function (t) {
  t.plan(2)
  SearchIndex({
    indexPath: sandbox + '/si-classifier',
    fieldedSearch: false,
    nGramLength: {gte: 1, lte: 3},
    logLevel: logLevel
  }, function (err, indexer) {
    t.error(err)
    s.pipe(indexer.feed({ objectMode: true }))
      .on('finish', function () {
        indexer.close(function (err) {
          t.error(err)
        })
      })
  })
})

test('initialize a searcher', function (t) {
  t.plan(1)
  SearchIndex({
    indexPath: sandbox + '/si-classifier',
    nGramLength: {gte: 1, lte: 3}
  }, function (err, thisSi) {
    t.error(err)
    si = thisSi
  })
})

test('classify', function (t) {
  t.plan(11)
  var expectedResults = [
    { token: 'this', documents: [ '10', '2', '7' ] },
    { token: 'is', documents: [ '4', '8' ] },
    { token: 'a', documents: [ '4', '6', '7' ] },
    { token: 'tw', documents: [ '7' ] },
    { token: 'swiss', documents: [ '10', '2', '3', '4', '5', '9' ] },
    { token: 'and', documents: [ '1', '10', '7' ] },
    { token: 'a', documents: [ '4', '6', '7' ] },
    { token: 'watch', documents: [ '1', '10', '2', '3', '7', '9' ] },
    { token: 'from', documents: [ '5', '6', '7', '8' ] },
    { token: 'tw', documents: [ '7' ] }
  ]
  var s = new Readable()
  'This is a really tw interesting sentence about swiss watches and also a watch from tw wooo'
    .split(' ')
    .forEach(function (item) {
      s.push(item)
    })
  s.push(null)
  s.pipe(si.classify())
    .on('data', function (data) {
      t.looseEquals(data, expectedResults.shift())
    })
    .on('error', function (err) {
      t.error(err)
    })
    .on('end', function () {
      t.equals(expectedResults.length, 0)
    })
})

test('classify maxNGramLength is 3', function (t) {
  var expectedResults = [
    { token: 'this', documents: [ '10', '2', '7' ] },
    { token: 'is', documents: [ '4', '8' ] },
    { token: 'is a', documents: [ '4' ] },
    { token: 'a', documents: [ '4', '6', '7' ] },
    { token: 'swiss', documents: [ '10', '2', '3', '4', '5', '9' ] },
    { token: 'and', documents: [ '1', '10', '7' ] },
    { token: 'a', documents: [ '4', '6', '7' ] },
    { token: 'watch', documents: [ '1', '10', '2', '3', '7', '9' ] },
    { token: 'watch from', documents: [ '7' ] },
    { token: 'watch from tw', documents: [ '7' ] },
    { token: 'from', documents: [ '5', '6', '7', '8' ] },
    { token: 'from tw', documents: [ '7' ] },
    { token: 'tw', documents: [ '7' ] }
  ]
  var s = new Readable()
  'This is a really interesting sentence about swiss watches and also a watch from tw wooo'
    .split(' ')
    .forEach(function (item) {
      s.push(item)
    })
  s.push(null)
  t.plan(14)
  s.pipe(si.classify({
    maxNGramLength: 3
  }))
    .on('data', function (data) {
      t.looseEquals(data, expectedResults.shift())
    })
    .on('error', function (err) {
      t.error(err)
    })
    .on('end', function () {
      t.equals(expectedResults.length, 0)
    })
})
