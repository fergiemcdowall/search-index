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
    manufacturer: ['apple'],
    color: ['white', 'gold', 'black'],
    price: '20002',
    age: '346'
  },
  {
    id: '2',
    name: 'Victorinox Swiss Army',
    description: 'You have the power to keep time moving with this Airboss automatic watch.',
    manufacturer: ['victorinox'],
    color: ['white', 'gold', 'black'],
    price: '99',
    age: '33342'
  },
  {
    id: '3',
    name: 'Versace Men\'s Swiss',
    description: 'Versace Men\'s Swiss Chronograph Mystique Sport Two-Tone Ion-Plated Stainless Steel Bracelet Watch',
    manufacturer: ['versace'],
    color: ['white', 'gold', 'black'],
    price: '4716',
    age: '8293'
  },
  {
    id: '4',
    name: 'CHARRIOL Men\'s Swiss Alexandre',
    description: 'With CHARRIOLs signature twisted cables, the Alexander C timepiece collection is a must-have piece for lovers of the famed brand.',
    manufacturer: ['charriol'],
    color: ['pink', 'gold', 'black'],
    price: '2132',
    age: '33342'
  },
  {
    id: '5',
    name: 'Ferragamo Men\'s Swiss 1898',
    description: 'The 1898 timepiece collection from Ferragamo offers timeless luxury.',
    manufacturer: ['ferragamo'],
    color: ['pink', 'gold', 'black'],
    price: '99999',
    age: '33342'
  },
  {
    id: '6',
    name: 'Bulova AccuSwiss',
    description: 'The Percheron Treble timepiece from Bulova AccuSwiss sets the bar high with sculpted cases showcasing sporty appeal. A Manchester United® special edition.',
    manufacturer: ['bulova'],
    color: ['pink', 'gold', 'black'],
    price: '1313',
    age: '33342'
  },
  {
    id: '7',
    name: 'TW Steel',
    description: 'A standout timepiece that boasts a rich heritage and high-speed design. This CEO Tech watch from TW Steel sets the standard for elite. Armani',
    manufacturer: ['tw', 'armani'],
    color: ['pink'],
    price: '33333',
    age: '33342'
  },
  {
    id: '8',
    name: 'Invicta Bolt Zeus ',
    description: 'Invicta offers an upscale timepiece that\'s as full of substance as it is style. From the Bolt Zeus collection.',
    manufacturer: ['invicta'],
    color: ['red', 'blue'],
    price: '8767',
    age: '33342'
  },
  {
    id: '9',
    name: 'Victorinox Night Vision ',
    description: 'Never get left in the dark with Victorinox Swiss Army\'s Night Vision watch. First at Macy\'s!',
    manufacturer: ['victorinox'],
    color: ['red', 'black'],
    price: '1000',
    age: '33342'
  },
  {
    id: '10',
    name: 'Armani Swiss Moon Phase',
    description: 'Endlessly sophisticated in materials and design, this Emporio Armani Swiss watch features high-end timekeeping with moon phase movement and calendar tracking.',
    manufacturer: ['armani'],
    color: ['gold'],
    price: '30000',
    age: '33342'
  },
]

it('should do some simple indexing with filters', function (done) {
  SearchIndex({
    indexPath: sandboxPath + '/facet-test',
    logLevel: 'warn',
    fieldOptions: [
      {fieldName: 'manufacturer', filter: true},
      {fieldName: 'color', filter: true},
      {fieldName: 'price', filter: true}
    ]
  }, function(err, thisSI) {
    si = thisSI
    si.add(batch, {}, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })
})


it('return all docs, and show manufacturer and color categories, filter on color: black', function (done) {
  si.search({
    query: [
      {
        AND: {'*': ['swiss', 'watch']},
      },
      {
        AND: {'*': ['apple']},
      }
    ],
    categories: [{
      name: 'manufacturer'
    }, {
      name: 'color'
    }],
    filter: [{
      field: 'color',
      gte: 'black',
      lte: 'black'
    }]
  }, function (err, results) {
    ;(err === null).should.be.exactly(true)
    should.exist(results)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '1', '3', '9', '2' ]
    )
    // console.log(JSON.stringify(results.categories, null, 2))

    // results.categories.should.eql('sds')

    done()
  })
})



it('should be able to do simple filtering on manufacturer=armani', function (done) {
  var q = {}
  q.query = {
    AND: {'*': ['*']}
  }
  q.filter = [
    {
      field: 'manufacturer',
      gte: 'armani',
      lte: 'armani'
    }
  ]
  q.pageSize = 100
  si.search(q, function (err, results) {
    should.exist(results)
    ;(err === null).should.be.exactly(true)
    results.hits.length.should.be.exactly(2)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '7', '10' ])
    done()
  })
})

it('should be able to do simple filtering on manufacturer=armani, ferragamo', function (done) {
  var q = {}
  q.query = {
    AND: {'*': ['*']}
  }
  q.filter = [
    {
      field: 'manufacturer',
      gte: 'armani',
      lte: 'armani'
    },
    {
      field: 'manufacturer',
      gte: 'tw',
      lte: 'tw'
    }
  ]
  q.pageSize = 100
  si.search(q, function (err, results) {
    should.exist(results)
    ;(err === null).should.be.exactly(true)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '7' ])
    results.hits.length.should.be.exactly(1)
    done()
  })
})


it('should be able to do simple filtering on a range of prices and manufacturer', function (done) {
  var q = {}
  q.query = {
    AND: {'*': ['*']}
  }
  q.filter = [
    {
      field: 'price',
      gte: '32',
      lte: '4'
    },
    {
      field: 'manufacturer',
      gte: 'armani',
      lte: 'armani'
    }
  ]
  q.pageSize = 100
  si.search(q, function (err, results) {
    should.exist(results)
    ;(err === null).should.be.exactly(true)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '7' ] )
    results.hits.length.should.be.exactly(1)
    done()
  })
})


// TODO: instead of facets, maybe use 'categories' and 'buckets' instead?

it('should be able to search in indexed data with faceting', function (done) {
  var q = {}
  q.query = [
    {
      AND: {'*': ['*','watch']}
    }
  ]
  q.categories = [
    {
      name: 'manufacturer'
    }
  ]
  si.search(q, function (err, results) {
    // console.log(JSON.stringify(results.categories, null, 2))
    should.exist(results)
    ;(err === null).should.be.exactly(true)
    results.hits.length.should.be.exactly(6)
    results.totalHits.should.be.exactly(6)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '1', '9', '7', '3', '2', '10' ])
    results.categories.length.should.be.exactly(1)
    results.categories[0].key.should.be.exactly('manufacturer')
    results.categories.should.eql([ 
      { 
        'key': 'manufacturer',
        'value': [
          { 
            'key': 'armani',
            'value': 2
          },
          { 
            'key': 'victorinox',
            'value': 2
          },
          { 
            'key': 'apple',
            'value': 1
          },
          { 
            'key': 'tw',
            'value': 1
          },
          { 
            'key': 'versace',
            'value': 1
          }
        ]
      }
    ])
    done()
  })
})


it('should be able to search in indexed data with faceting, using a different sort', function (done) {
  var q = {}
  q.query = [
    {
      AND: {'*': ['*','watch']}
    }
  ]
  q.categories = [
    {
      name: 'manufacturer',
      sort: 'keyDesc'
    }
  ]
  si.search(q, function (err, results) {
    // console.log(JSON.stringify(results.categories, null, 2))
    should.exist(results)
    ;(err === null).should.be.exactly(true)
    results.hits.length.should.be.exactly(6)
    results.totalHits.should.be.exactly(6)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '1', '9', '7', '3', '2', '10' ])
    results.categories.length.should.be.exactly(1)
    results.categories[0].key.should.be.exactly('manufacturer')
    results.categories.should.eql([ 
      { 
        'key': 'manufacturer',
        'value': [
          {
            'key': 'victorinox',
            'value': 2
          },
          {
            'key': 'versace',
            'value': 1
          },
          {
            'key': 'tw',
            'value': 1
          },
          {
            'key': 'armani',
            'value': 2
          },
          {
            'key': 'apple',
            'value': 1
          } 
        ] 
      }
    ])
    done()
  })
})


it('should be able to search in indexed data with faceting, using a different sort, and limiting the facet length', function (done) {
  var q = {}
  q.query = [
    {
      AND: {'*': ['*','watch']}
    }
  ]
  q.categories = [
    {
      name: 'manufacturer',
      sort: 'keyDesc',
      limit: 3
    }
  ]
  si.search(q, function (err, results) {
    should.exist(results)
    ;(err === null).should.be.exactly(true)
    results.hits.length.should.be.exactly(6)
    results.totalHits.should.be.exactly(6)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '1', '9', '7', '3', '2', '10' ])
    results.categories.length.should.be.exactly(1)
    results.categories[0].key.should.be.exactly('manufacturer')
    results.categories.should.eql([{
      'key': 'manufacturer',
      'value': [
        {
          'key': 'victorinox',
          'value': 2
        },
        {
          'key': 'versace',
          'value': 1
        },
        {
          'key': 'tw',
          'value': 1
        } 
      ] 
    }])
    done()
  })
})


it('search for Armarni AND Watch OR Victorinox AND swiss OR TW AND watch and return categories', function (done) {
  si.search({
    query: [
      {
        AND: {'*': ['armani', 'watch']}
      },
      {
        AND: {'*': ['victorinox', 'swiss']}
      },
      {
        AND: {'*': ['tw', 'watch']}
      }
    ],
    categories: [{
      name: 'manufacturer'
    }]
  }, function (err, results) {
    ;(err === null).should.be.exactly(true)
    should.exist(results)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '7', '9', '10', '2'])
    results.categories.should.eql([
      {
        "key": "manufacturer",
        "value": [
          {
            "key": "armani",
            "value": 2
          },
          {
            "key": "victorinox",
            "value": 2
          },
          {
            "key": "tw",
            "value": 1
          } 
        ] 
      } 
    ])
    done()
  })
})

it('search for "swiss" NOT "watch" and return categories for manufacturer', function (done) {
  si.search({
    query: [
      {
        AND: {'*': ['swiss']},
        NOT: {'*': ['watch']}
      }
    ],
    categories: [{
      name: 'manufacturer'
    }]
  }, function (err, results) {
    ;(err === null).should.be.exactly(true)
    should.exist(results)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '5', '4' ]
    )
    results.categories.should.eql([
      {
        "key": "manufacturer",
        "value": [
          {
            "key": "charriol",
            "value": 1
          },
          {
            "key": "ferragamo",
            "value": 1
          }
        ] 
      } 
    ])
    done()
  })
})

it('return all docs, and show manufacturer and color categories', function (done) {
  si.search({
    query: [
      {
        AND: {'*': ['*']},
      }
    ],
    categories: [{
      name: 'manufacturer'
    }, {
      name: 'color'
    }]
  }, function (err, results) {
    ;(err === null).should.be.exactly(true)
    // console.log(JSON.stringify(results.categories, null, 2))
    should.exist(results)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '9', '8', '7', '6', '5', '4', '3', '2', '10', '1' ]
    )
    results.categories.should.eql([
      {
        "key": "manufacturer",
        "value": [
          {
            "key": "armani",
            "value": 2
          },
          { 
            "key": "victorinox",
            "value": 2
          },
          { 
            "key": "apple",
            "value": 1
          },
          { 
            "key": "bulova",
            "value": 1
          },
          { 
            "key": "charriol",
            "value": 1
          },
          { 
            "key": "ferragamo",
            "value": 1
          },
          { 
            "key": "invicta",
            "value": 1
          },
          { 
            "key": "tw",
            "value": 1
          },
          { 
            "key": "versace",
            "value": 1
          }
        ]
      },
      {
        "key": "color",
        "value": [
          {
            "key": "black",
            "value": 7
          },
          {
            "key": "gold",
            "value": 7
          },
          {
            "key": "pink",
            "value": 4
          },
          {
            "key": "white",
            "value": 3
          },
          {
            "key": "red",
            "value": 2
          },
          {
            "key": "blue",
            "value": 1
          } 
        ] 
      }
    ])
    done()
  })
})


it('return all items and bucket on price', function (done) {
  si.search({
    query: [
      {
        AND: {'*': ['*']},
      }
    ],
    buckets: [{
      field: 'price',
      gte: '2',
      lte: '3'
    }]
  }, function (err, results) {
    ;(err === null).should.be.exactly(true)
    should.exist(results)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '9', '8', '7', '6', '5', '4', '3', '2', '10', '1' ]
    )
    results.buckets.should.eql([
      {
        "field": "price",
        "gte": "2",
        "lte": "3",
        "count": 4
      } 
    ])
    done()
  })
})


it('return all items and bucket on price', function (done) {
  si.search({
    query: [
      {
        AND: {'*': ['watch']},
      },
      {
        AND: {'*': ['swiss']},
      }
    ],
    buckets: [{
      field: 'price',
      gte: '1',
      lte: '5'
    }]
  }, function (err, results) {
    ;(err === null).should.be.exactly(true)
    // console.log(JSON.stringify(results.buckets, null, 2))
    should.exist(results)
    results.hits.map(function (item) { return item.id }).should.eql(
      [ '3', '10', '1', '9', '2', '7', '5', '4' ]
    )
    results.buckets.should.eql([
      {
        "field": "price",
        "gte": "1",
        "lte": "5",
        "count": 6
      } 
    ])
    done()
  })
})
