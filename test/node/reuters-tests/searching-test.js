/* global it */
/* global describe */

const should = require('should')
const searchindex = require('../../../')

describe('Searching Reuters: ', function () {

  var si

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: 'test/sandbox/si-reuters',
      logLevel: 'error'
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si = thisSi
      done()
    })
  })


  it('should search on all fields and get results', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['usa']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.above(1)
      searchResults.hits.length.should.be.exactly(100)
      searchResults.hits[0].id.should.be.exactly('510')
      searchResults.hits[1].id.should.be.exactly('287')
      searchResults.hits[2].id.should.be.exactly('998')
      searchResults.hits[3].id.should.be.exactly('997')
      done()
    })
  })

  it('should search on all fields and get no results for a valid, yet absent keyword', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['usaasdadadlkjadj']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(0)
      done()
    })
  })

  it('should be able to handle multiword searches', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['reuter', '1987']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(100)
      searchResults.totalHits.should.be.exactly(922)
      done()
    })
  })

  it('should be able to return all results by doing a wildcard (*) search', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['*']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(100)
      searchResults.totalHits.should.be.exactly(1000)
      done()
    })
  })

  it('should be able to handle multi word searches where some words are not present in index', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['reuter', 'yorkxxxxxxx']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(0)
      // TODO: make this return a full resultset
      //        searchResults.totalHits.should.be.exactly(0)
      done()
    })
  })

  it('should be able to offset', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['japan']}
    }
    q.offset = 5
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(51)
      searchResults.hits.length.should.be.above(1)
      searchResults.hits[0].id.should.be.exactly('351')
      done()
    })
  })

  it('should be able to set page size (limit results)', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['japan']}
    }
    q.pageSize = 5
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(5)
      done()
    })
  })

  it('should be able to page (set offset and page size)', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['japan']}
    }
    q.offset = 5
    q.pageSize = 5
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(5)
      searchResults.hits[0].id.should.be.exactly('351')
      done()
    })
  })

  it('should be able to search in indexed data with faceting', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['usa']}
    }
    // q.facets = {places: {}}
    q.categories = [{
      name: 'places'
    }]
    si.search(q, function (err, searchResults) {

      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(100)
      searchResults.hits[0].id.should.be.exactly('510')
      searchResults.hits[1].id.should.be.exactly('287')
      searchResults.hits[2].id.should.be.exactly('998')
      searchResults.hits[3].id.should.be.exactly('997')

      searchResults.categories[0].value.slice(0, 10).should.eql(
        [ 
          {
            "key": "usa",
            "value": 546
          },
          {
            "key": "japan",
            "value": 16
          },
          {
            "key": "uk",
            "value": 14
          },
          {
            "key": "brazil",
            "value": 9
          },
          {
            "key": "taiwan",
            "value": 5
          },
          {
            "key": "australia",
            "value": 4
          },
          {
            "key": "china",
            "value": 4
          },
          {
            "key": "ussr",
            "value": 4
          },
          {
            "key": "argentina",
            "value": 3
          },
          {
            "key": "canada",
            "value": 3
          } 
        ]
      )

      done()
    })
  })

  it('should be able to filter search results', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['usa']}
    }
    q.categories = [{
      name: 'places'
    }]
    q.filter = [{
      field: 'places',
      gte: 'japan',
      lte: 'japan'
    }]
    // q.facets = {places: {}}
    // q.filter = {places: [['japan', 'japan']]}

    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(16)
      searchResults.hits[0].id.should.be.exactly('287')
      done()
    })
  })

  it('should be able to search on all fields', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['reagan']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(20)
      searchResults.hits[0].id.should.be.exactly('386')
      searchResults.hits[1].id.should.be.exactly('804')
      searchResults.hits[2].id.should.be.exactly('231')
      searchResults.hits[3].id.should.be.exactly('964')
      done()
    })
  })

  it('should be able to search on one field', function (done) {
    var q = {}
    q.query = {
      AND: {title: ['reagan']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(10)
      searchResults.hits[0].id.should.be.exactly('964')
      done()
    })
  })

  it('should be able to search on one field for two terms', function (done) {
    var q = {}
    q.query = {
      AND: {title: ['reagan', 'baker']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(4)
      searchResults.hits[0].id.should.be.exactly('804')
      searchResults.hits[1].id.should.be.exactly('796')
      searchResults.hits[2].id.should.be.exactly('790')
      searchResults.hits[3].id.should.be.exactly('386')
      done()
    })
  })

  it('should be able to search on on two fields for seperate terms', function (done) {
    var q = {}
    q.query = {
      AND: [{
        title: ['reagan']
      }, {
        body: ['intelligence']
      }]
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(4)
      searchResults.hits[0].id.should.be.exactly('386')
      searchResults.hits[1].id.should.be.exactly('869')
      searchResults.hits[2].id.should.be.exactly('801')
      searchResults.hits[3].id.should.be.exactly('28')
      done()
    })
  })

  it('should be able to search on on two fields for multiple terms', function (done) {
    var q = {}
    q.query = {
      AND:[
        {title: ['reagan']},
        {body: ['intelligence', 'agency', 'contra']}
      ]
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(2)
      searchResults.hits[0].id.should.be.exactly('386')
      searchResults.hits[1].id.should.be.exactly('869')
      done()
    })
  })

  it('should be able to generate teasers', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['advertising']}
    }
    q.teaser = 'title'
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(3)
      searchResults.hits[0].document.teaser.should.be
        .exactly('GREY <span class="sc-em">advertising</span> <GREY> FORMS NEW DIVISION')
      done()
    })
  })

  it('should be able to display information about the index', function (done) {
    // TODO: there should probably be an error object in this function
    si.tellMeAboutMySearchIndex(function (err, info) {
      ;(err === null).should.be.exactly(true)
      should.exist(info)
      info.totalDocs.should.be.exactly(1000)
      done()
    })
  })

  it('should be able to filter on a chosen facetrange and drill down on two values in multiple filters', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['reuter']}
    }
    q.categories = [
      {name: 'topics'},
      {name: 'places'},
      {name: 'organisations'},
    ]
    q.filter = [
      {
        field: 'places',
        gte: 'usa',
        lte: 'usa'
      },
      {
        field: 'places',
        gte: 'japan',
        lte: 'japan'
      }
    ]
    si.search(q, function (err, searchResults) {
      // console.log(JSON.stringify(searchResults, null, 2))
      // console.log(JSON.stringify(searchResults.hits.map(function(hit) {return hit.document.topics}), null, 2))
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(16)
      searchResults.totalHits.should.be.exactly(16)

      searchResults.categories.should.eql(
        [
          {
            "key": "topics",
            "value": [
              {
                "key": "trade",
                "value": 3
              },
              {
                "key": "dlr",
                "value": 1
              },
              {
                "key": "money-fx",
                "value": 1
              },
              {
                "key": "reserves",
                "value": 1
              },
              { 
                "key": "yen",
                "value": 1
              }
            ]
          },
          { 
            "key": "places",
            "value": [
              { 
                "key": "japan",
                "value": 16,
                "active": true
              },
              { 
                "key": "usa",
                "value": 16,
                "active": true
              },
              { 
                "key": "uk",
                "value": 4
              },
              { 
                "key": "brazil",
                "value": 2
              },
              {
                "key": "france",
                "value": 2
              },
              {
                "key": "west-germany", 
                "value": 2
              },
              {
                "key": "hong-kong",
                "value": 1
              },
              {
                "key": "italy",
                "value": 1
              },
              {
                "key": "switzerland",  
                "value": 1
              },
              {
                "key": "taiwan",
                "value": 1
              }
            ]
          },
          {
            "key": "organisations",
            "value": []
          }
        ]
      )

      searchResults.hits.map(function(hit) {return hit.id}).should.eql(
        [
          "991",
          "894",
          "893",
          "888",
          "872",
          "759",
          "753",
          "676",
          "419",
          "323",
          "287",
          "223",
          "914",
          "208",
          "333",
          "342"]        
      )
      done()
    })
  })

  it('should be able to display information about the index', function (done) {
    // TODO: there should probably be an error object in this function
    si.tellMeAboutMySearchIndex(function (err, info) {
      ;(err === null).should.be.exactly(true)
      should.exist(info)
      info.totalDocs.should.be.exactly(1000)
      done()
    })
  })

  it('should be able to filter on a chosen facetrange and drill down on two values in multiple filters', function (done) {
    var q = {}
    q.query = {
      AND:{'*': ['reuter']}
    }
    q.categories = [
      {name: 'topics'},
      {name: 'places'},
      {name: 'organisations'},
    ]
    q.filter = [
      {
        field: 'topics',
        gte: 'earn',
        lte: 'earn'
      },
      {
        field: 'topics',
        gte: 'alum',
        lte: 'alum'
      }
    ]
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(2)
      searchResults.totalHits.should.be.exactly(2)
      searchResults.hits[0].id.should.be.exactly('938')
      searchResults.hits[1].id.should.be.exactly('921')
      si.close(function(err){
        done()
      })
    })
  })

})
