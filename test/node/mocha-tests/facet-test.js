/* global it */
/* global describe */

const JSONStream = require('JSONStream')
const Readable = require('stream').Readable
const SearchIndex = require('../../../')
const logLevel = process.env.NODE_ENV || 'error'
const sandboxPath = 'test/sandbox'
const should = require('should')

var si

should

const s = new Readable()
s.push(JSON.stringify({
  id: '1',
  name: 'Apple Watch',
  description: 'Receive and respond to notiﬁcations in an instant. Watch this amazing watch',
  manufacturer: 'apple',
  color: 'white gold black',
  price: '20002',
  age: '346'
}))
s.push(JSON.stringify({
  id: '2',
  name: 'Victorinox Swiss Army',
  description: 'You have the power to keep time moving with this Airboss automatic watch.',
  manufacturer: 'victorinox',
  color: ['white', 'gold', 'black'],
  price: '99',
  age: '33342'
}))
s.push(JSON.stringify({
  id: '3',
  name: "Versace Men's Swiss",
  description: "Versace Men's Swiss Chronograph Mystique Sport Two-Tone Ion-Plated Stainless Steel Bracelet Watch",
  manufacturer: 'versace',
  color: ['white', 'gold', 'black'],
  price: '4716',
  age: '8293'
}))
s.push(JSON.stringify({
  id: '4',
  name: "CHARRIOL Men's Swiss Alexandre",
  description: 'With CHARRIOLs signature twisted cables, the Alexander C timepiece collection is a must-have piece for lovers of the famed brand.',
  manufacturer: ['charriol'],
  color: ['pink', 'gold', 'black'],
  price: '2132',
  age: '33342'
}))
s.push(JSON.stringify({
  id: '5',
  name: "Ferragamo Men's Swiss 1898",
  description: 'The 1898 timepiece collection from Ferragamo offers timeless luxury.',
  manufacturer: ['ferragamo'],
  color: ['pink', 'gold', 'black'],
  price: '99999',
  age: '33342'
}))
s.push(JSON.stringify({
  id: '6',
  name: 'Bulova AccuSwiss',
  description: 'The Percheron Treble timepiece from Bulova AccuSwiss sets the bar high with sculpted cases showcasing sporty appeal. A Manchester United® special edition.',
  manufacturer: ['bulova'],
  color: ['pink', 'gold', 'black'],
  price: '1313',
  age: '33342'
}))
s.push(JSON.stringify({
  id: '7',
  name: 'TW Steel',
  description: 'A standout timepiece that boasts a rich heritage and high-speed design. This CEO Tech watch from TW Steel sets the standard for elite. Armani',
  manufacturer: ['tw', 'armani'],
  color: ['pink'],
  price: '33333',
  age: '33342'
}))
s.push(JSON.stringify({
  id: '8',
  name: 'Invicta Bolt Zeus ',
  description: "Invicta offers an upscale timepiece that's as full of substance as it is style. From the Bolt Zeus collection.",
  manufacturer: ['invicta'],
  color: ['red', 'blue'],
  price: '8767',
  age: '33342'
}))
s.push(JSON.stringify({
  id: '9',
  name: 'Victorinox Night Vision ',
  description: "Never get left in the dark with Victorinox Swiss Army's Night Vision watch. First at Macy's!",
  manufacturer: ['victorinox'],
  color: ['red'],
  price: '1000',
  age: '33342'
}))
s.push(JSON.stringify({
  id: '10',
  name: 'Armani Swiss Moon Phase',
  description: 'Endlessly sophisticated in materials and design, this Emporio Armani Swiss watch features high-end timekeeping with moon phase movement and calendar tracking.',
  manufacturer: ['armani'],
  color: ['gold'],
  price: '30000',
  age: '33342'
}))
s.push(null)

describe('categories: ', function () {
  it('should do some simple indexing with filters', function (done) {
    SearchIndex({
      indexPath: sandboxPath + '/facet-test',
      logLevel: logLevel
    }, function (err, thisSI) {
      if (err) false.should.eql(true)
      si = thisSI
      s.pipe(JSONStream.parse())
        .pipe(si.defaultPipeline())
        .pipe(si.add())
        .on('data', function (data) {})
        .on('end', function () {
          return done()
        })
    })
  })

  it('return all docs, and show manufacturer category', function (done) {
    var result = [
      {
        'key': '*',
        'value': 4
      },
      {
        'key': 'armani',
        'value': 1
      },
      {
        'key': 'versace',
        'value': 1
      },
      {
        'key': 'victorinox',
        'value': 2
      }
    ]
    si.categorize({
      query: [
        {
          AND: {'*': ['swiss', 'watch']}
        }
      ],
      category: {
        field: 'manufacturer'
      }
    }).on('data', function (data) {
      data.should.eql(result.shift())
    }).on('end', function () {
      return done()
    })
  })

  it('return all docs, and show manufacturer category by set', function (done) {
    var result = [
      {
        'key': '*',
        'value': [ '10', '2', '3', '9' ]
      },
      {
        'key': 'armani',
        'value': [ '10' ]
      },
      {
        'key': 'versace',
        'value': [ '3' ]
      },
      {
        'key': 'victorinox',
        'value': [ '2', '9' ]
      }
    ]
    si.categorize({
      query: [
        {
          AND: {'*': ['swiss', 'watch']}
        }
      ],
      category: {
        field: 'manufacturer',
        set: true
      }
    }).on('data', function (data) {
      data.should.eql(result.shift())
    }).on('end', function () {
      return done()
    })
  })

  it('return all docs, and show manufacturer category, filter on color: black', function (done) {
    var result = [
      { key: '*', value: 3 },
      { key: 'apple', value: 1 },
      { key: 'versace', value: 1 },
      { key: 'victorinox', value: 1 }
    ]
    si.categorize({
      query: [
        {
          AND: {
            '*': ['swiss', 'watch'],
            'color': ['black']
          }
        },
        {
          AND: {
            '*': ['apple'],
            'color': ['black']
          }
        }
      ],
      category: {
        field: 'manufacturer'
      }
    }).on('data', function (data) {
      data.should.eql(result.shift())
    }).on('end', function () {
      result.length.should.be.exactly(0)
      return done()
    })
  })

  it('return all docs, and show color category, filter on color: black', function (done) {
    var result = [
      { key: '*', value: 6 },
      { key: 'black', value: 6, filter: true },
      { key: 'gold', value: 6 },
      { key: 'pink', value: 3 },
      { key: 'white', value: 3 }
    ]
    si.categorize({
      query: [
        {
          AND: {
            '*': ['*'],
            'color': ['black']
          }
        }
      ],
      category: {
        field: 'color'
      }
    }).on('data', function (data) {
      data.should.eql(result.shift())
    }).on('end', function () {
      result.length.should.be.exactly(0)
      return done()
    })
  })

  it('return all docs, and show color category, filter on color: black, pink', function (done) {
    var result = [
      { key: '*', value: 3 },
      { key: 'black', value: 3, filter: true },
      { key: 'gold', value: 3 },
      { key: 'pink', value: 3, filter: true }
    ]
    si.categorize({
      query: [
        {
          AND: {
            '*': ['*'],
            'color': ['black', 'pink']
          }
        }
      ],
      category: {
        field: 'color'
      }
    }).on('data', function (data) {
      data.should.eql(result.shift())
    }).on('end', function () {
      result.length.should.be.exactly(0)
      return done()
    })
  })


  it('should be able to do simple filtering on price', function (done) {
    var result = [
      { key: '*', value: 3 },
      { key: 'armani', value: 1 },
      { key: 'versace', value: 1 },
      { key: 'victorinox', value: 1 }
    ]
    si.categorize({
      query: [
        {
          AND: {
            '*': ['swiss', 'watch'],
            'price': [{
              gte: '1000',
              lte: '8'
            }]
          }
        }
      ],
      category: {
        field: 'manufacturer'
      }
    }).on('data', function (data) {
      // console.log(data)
      data.should.eql(result.shift())
    }).on('end', function () {
      result.length.should.be.exactly(0)
      return done()
    })
  })

  it('search for Armarni AND Watch OR Victorinox AND swiss OR TW AND watch and return categories', function (done) {
    var result = [
      { key: '*', value: 4 },
      { key: 'armani', value: 2 },
      { key: 'tw', value: 1 },
      { key: 'victorinox', value: 2 }
    ]
    si.categorize({
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
      category: {
        field: 'manufacturer'
      }
    }).on('data', function (data) {
      // console.log(data)
      data.should.eql(result.shift())
    }).on('end', function () {
      result.length.should.be.exactly(0)
      return done()
    })
  })

  it('search for "swiss" NOT "watch" and return categories for manufacturer', function (done) {
    var result = [
      { key: '*', value: ['4', '5'] },
      { key: 'charriol', value: [ '4' ] },
      { key: 'ferragamo', value: [ '5' ] }
    ]
    si.categorize({
      query: [
        {
          AND: {'*': ['swiss']},
          NOT: {'*': ['watch']}
        }
      ],
      category: {
        field: 'manufacturer',
        set: true
      }
    }).on('data', function (data) {
      // console.log(data)
      data.should.eql(result.shift())
    }).on('end', function () {
      result.length.should.be.exactly(0)
      return done()
    })
  })

  it('return all docs, show color category', function (done) {
    var result = [
      { key: '*', value: 10 },
      { key: 'black', value: 6 },
      { key: 'blue', value: 1 },
      { key: 'gold', value: 7 },
      { key: 'pink', value: 4 },
      { key: 'red', value: 2 },
      { key: 'white', value: 3 }
    ]
    si.categorize({
      query: [
        {
          AND: {'*': ['*']}
        }
      ],
      category: {
        field: 'color'
      }
    }).on('data', function (data) {
      data.should.eql(result.shift())
    }).on('end', function () {
      result.length.should.be.exactly(0)
      return done()
    })
  })

  it('return all docs, show color category, limit to 4', function (done) {
    var result = [
      { key: '*', value: 10 },
      { key: 'black', value: 6 },
      { key: 'blue', value: 1 },
      { key: 'gold', value: 7 }
    ]
    si.categorize({
      query: [
        {
          AND: {'*': ['*']}
        }
      ],
      category: {
        field: 'color'
      },
      pageSize: 4
    }).on('data', function (data) {
      data.should.eql(result.shift())
    }).on('end', function () {
      result.length.should.be.exactly(0)
      return done()
    })
  })

  it('return all docs, show color category, offset 1, limit to 4', function (done) {
    var result = [
      { key: 'black', value: 6 },
      { key: 'blue', value: 1 },
      { key: 'gold', value: 7 },
      { key: 'pink', value: 4 }
    ]
    si.categorize({
      query: [
        {
          AND: {'*': ['*']}
        }
      ],
      category: {
        field: 'color'
      },
      offset: 1,
      pageSize: 4
    }).on('data', function (data) {
      data.should.eql(result.shift())
    }).on('end', function () {
      result.length.should.be.exactly(0)
      return done()
    })
  })


  it('bucket on price', function (done) {
    var result = [
      {
        'field': 'price',
        'gte': '2',
        'lte': '3',
        'set': false,
        'value': 4
      }
    ]
    si.buckets({
      query: [
        {
          AND: {'*': ['*']}
        }
      ],
      buckets: [{
        field: 'price',
        gte: '2',
        lte: '3',
        set: false
      }]
    }).on('data', function (data) {
      data.should.eql(result.shift())
    }).on('end', function () {
      result.length.should.be.exactly(0)
      return done()
    })
  })

  it('return all items and bucket on price', function (done) {
    var result = [
      {
        'field': 'price',
        'gte': '1',
        'lte': '5',
        'set': false,
        'value': 6
      }
    ]
    si.buckets({
      query: [
        {
          AND: {'*': ['watch']}
        },
        {
          AND: {'*': ['swiss']}
        }
      ],
      buckets: [{
        field: 'price',
        gte: '1',
        lte: '5',
        set: false
      }]
    }).on('data', function (data) {
      data.should.eql(result.shift())
    }).on('end', function () {
      result.length.should.be.exactly(0)
      return done()
    })
  })
})
