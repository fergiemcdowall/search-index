/* global it */
/* global describe */

const should = require('should')
const searchindex = require('../../../')

describe('Searching Reuters and Checking Faceting: ', function () {

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: 'test/sandbox/si-reuters',
      logLevel: 'error'
    }, function (err, thisSi) {
      ;(err === null).should.be.exactly(true)
      si = thisSi
      done()
    })
  })


  it('should be able to search in indexed data with faceting', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['usa']}
    }
    q.categories = [
      {
        name: 'places'
      }
    ]
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(100)
      results.hits.map(function(item){return item.id}).slice(0, 10).should.eql(
        [ 
          "510",
          "287",
          "998",
          "997",
          "996",
          "995",
          "994",
          "993",
          "992",
          "991", 
        ])
      results.totalHits.should.be.exactly(546)
      // console.log(JSON.stringify(results.categories, null, 2))
      results.categories.should.eql([
        {
          "key": "places",
          "value": [
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
            },
            {
              "key": "france",
              "value": 3
            },
            {
              "key": "hong-kong",
              "value": 3
            },
            {
              "key": "south-korea",  
              "value": 3
            },
            {
              "key": "west-germany", 
              "value": 3
            },
            {
              "key": "indonesia",
              "value": 2
            },
            {
              "key": "iran",
              "value": 2
            },
            {
              "key": "spain",
              "value": 2
            },
            {
              "key": "switzerland",  
              "value": 2
            },
            {
              "key": "algeria",
              "value": 1
            },
            {
              "key": "austria",
              "value": 1
            },
            {
              "key": "bahrain",
              "value": 1
            },
            {
              "key": "colombia",
              "value": 1
            },
            {
              "key": "el-salvador",  
              "value": 1
            },
            {
              "key": "guam",
              "value": 1
            },
            {
              "key": "honduras",
              "value": 1
            },
            {
              "key": "italy",
              "value": 1
            },
            {
              "key": "lebanon",
              "value": 1
            },
            {
              "key": "liechtenstein",
              "value": 1
            },
            {
              "key": "panama",
              "value": 1
            },
            {
              "key": "philippines",  
              "value": 1
            },
            {
              "key": "poland",
              "value": 1
            },
            {
              "key": "portugal",
              "value": 1
            },
            {
              "key": "singapore",
              "value": 1
            },
            {
              "key": "sweden",
              "value": 1
            },
            {
              "key": "uae",
              "value": 1
            },
            {
              "key": "uruguay",
              "value": 1
            },
            {
              "key": "us-virgin-islands",
              "value": 1
            },
            {
              "key": "venezuela",
              "value": 1
            },
            {
              "key": "yemen-arab-republic",
              "value": 1
            }
          ]
        }
      ])
      done()
    })
  })



  it('should be able to sort facets by value ascending', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['reuter']}
    }
    // q.facets = {places: {sort: 'valueAsc'}}
    q.categories = [
      {
        name: 'places',
        sort: 'valueAsc'
      }
    ]
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(100)
      results.totalHits.should.be.exactly(922)
      // console.log(JSON.stringify(results.categories, null, 2))
      results.categories.should.eql(
        [ 
          {
            "key": "places",
            "value": [
              {
                "key": "algeria",
                "value": 1
              },
              {
                "key": "austria",
                "value": 1
              },
              {
                "key": "bhutan",
                "value": 1
              },
              {
                "key": "bolivia",
                "value": 1
              },
              {
                "key": "congo",
                "value": 1
              },
              {
                "key": "cuba",
                "value": 1
              },
              {
                "key": "ecuador",
                "value": 1
              },
              {
                "key": "el-salvador",
                "value": 1
              },
              {
                "key": "finland",
                "value": 1
              },
              {
                "key": "greece",
                "value": 1
              },
              {
                "key": "guam",
                "value": 1
              },
              {
                "key": "honduras",
                "value": 1
              },
              {
                "key": "hungary",
                "value": 1
              },
              {
                "key": "ireland",
                "value": 1
              },
              {
                "key": "libya",
                "value": 1
              },
              {
                "key": "liechtenstein",
                "value": 1
              },
              {
                "key": "mexico",
                "value": 1
              },
              {
                "key": "nepal",
                "value": 1
              },
              {
                "key": "oman",
                "value": 1
              },
              {
                "key": "panama",
                "value": 1
              },
              {
                "key": "poland",
                "value": 1
              },
              {
                "key": "portugal",
                "value": 1
              },
              {
                "key": "sri-lanka",
                "value": 1
              },
              {
                "key": "tanzania",
                "value": 1
              },
              {
                "key": "uruguay",
                "value": 1
              },
              {
                "key": "us-virgin-islands",
                "value": 1
              },
              {
                "key": "yemen-demo-republic",
                "value": 1
              },
              {
                "key": "zambia",
                "value": 1
              },
              {
                "key": "lebanon",
                "value": 2
              },
              {
                "key": "nigeria",
                "value": 2
              },
              {
                "key": "syria",
                "value": 2
              },
              {
                "key": "turkey",
                "value": 2
              },
              {
                "key": "venezuela",
                "value": 2
              },
              {
                "key": "yemen-arab-republic",
                "value": 2
              },
              {
                "key": "zaire",
                "value": 2
              },
              {
                "key": "bangladesh",   
                "value": 3
              },
              {
                "key": "colombia",
                "value": 3
              },
              {
                "key": "qatar",
                "value": 3
              },
              {
                "key": "south-africa", 
                "value": 3
              },
              {
                "key": "egypt",
                "value": 4
              },
              {
                "key": "kuwait",
                "value": 4
              },
              {
                "key": "malaysia",
                "value": 4
              },
              {
                "key": "pakistan",
                "value": 4
              },
              {
                "key": "sweden",
                "value": 4
              },
              {
                "key": "uae",
                "value": 4
              },
              {
                "key": "argentina",
                "value": 5
              },
              {
                "key": "iraq",
                "value": 5
              },
              {
                "key": "singapore",
                "value": 5
              },
              {
                "key": "spain",
                "value": 5
              },
              {
                "key": "thailand",
                "value": 5
              }
            ]
          }
        ])
      done()
    })
  })

  it('should be able to sort facets by key ascending', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['reuter']}
    }
    q.categories = [{
      name: 'places',
      sort: 'keyAsc'
    }]
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(100)
      results.totalHits.should.be.exactly(922)
      // console.log(JSON.stringify(results.categories, null, 2))
      results.categories.should.eql(
        [
          {
            "key": "places",
            "value": [
              {
                "key": "algeria",
                "value": 1
              },
              {
                "key": "argentina",
                "value": 5
              },
              {
                "key": "australia",
                "value": 17
              },
              {
                "key": "austria",
                "value": 1
              },
              {
                "key": "bahrain",
                "value": 7
              },
              {
                "key": "bangladesh",   
                "value": 3
              },
              {
                "key": "belgium",
                "value": 6
              },
              {
                "key": "bhutan",
                "value": 1
              },
              {
                "key": "bolivia",
                "value": 1
              },
              {
                "key": "brazil",
                "value": 21
              },
              {
                "key": "canada",
                "value": 42
              },
              {
                "key": "china",
                "value": 16
              },
              {
                "key": "colombia",
                "value": 3
              },
              {
                "key": "congo",
                "value": 1
              },
              {
                "key": "cuba",
                "value": 1
              },
              {
                "key": "ecuador",
                "value": 1
              },
              {
                "key": "egypt",
                "value": 4
              },
              {
                "key": "el-salvador",  
                "value": 1
              },
              {
                "key": "finland",
                "value": 1
              },
              {
                "key": "france",
                "value": 18
              },
              {
                "key": "greece",
                "value": 1
              },
              {
                "key": "guam",
                "value": 1
              },
              {
                "key": "honduras",
                "value": 1
              },
              {
                "key": "hong-kong",
                "value": 9
              },
              {
                "key": "hungary",
                "value": 1
              },
              {
                "key": "india",
                "value": 6
              },
              {
                "key": "indonesia",
                "value": 13
              },
              {
                "key": "iran",
                "value": 6
              },
              {
                "key": "iraq",
                "value": 5
              },
              {
                "key": "ireland",
                "value": 1
              },
              {
                "key": "italy",
                "value": 10
              },
              {
                "key": "japan",
                "value": 47
              },
              {
                "key": "kuwait",
                "value": 4
              },
              {
                "key": "lebanon",
                "value": 2
              },
              {
                "key": "libya",
                "value": 1
              },
              {
                "key": "liechtenstein",
                "value": 1
              },
              {
                "key": "malaysia",
                "value": 4
              },
              {
                "key": "mexico",
                "value": 1
              },
              {
                "key": "nepal",
                "value": 1
              },
              {
                "key": "netherlands",  
                "value": 8
              },
              {
                "key": "new-zealand",  
                "value": 11
              },
              {
                "key": "nigeria",
                "value": 2
              },
              {
                "key": "oman",
                "value": 1
              },
              {
                "key": "pakistan",
                "value": 4
              },
              {
                "key": "panama",
                "value": 1
              },
              {
                "key": "philippines",  
                "value": 11
              },
              {
                "key": "poland",
                "value": 1
              },
              {
                "key": "portugal",
                "value": 1
              },
              {
                "key": "qatar",
                "value": 3
              },
              {
                "key": "saudi-arabia", 
                "value": 7
              }
            ]
          }
        ])
      done()
    })
  })

  it('should be able to sort facets by key descending', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['reuter']}
    }
    q.categories = [
      {
        name: 'places',
        sort: 'keyDesc'
      }
    ]
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(100)
      results.totalHits.should.be.exactly(922)
      // console.log(JSON.stringify(results.categories, null, 2))

      results.categories.should.eql(
        [ 
          {
            "key": "places",
            "value": [
              {
                "key": "zambia",
                "value": 1
              },
              {
                "key": "zaire",
                "value": 2
              },
              {
                "key": "yemen-demo-republic",
                "value": 1
              },
              {
                "key": "yemen-arab-republic",
                "value": 2
              },
              {
                "key": "west-germany",
                "value": 15
              },
              {
                "key": "venezuela",
                "value": 2
              },
              {
                "key": "ussr",
                "value": 5
              },
              {
                "key": "usa",
                "value": 524
              },
              {
                "key": "us-virgin-islands",
                "value": 1
              },
              {
                "key": "uruguay",
                "value": 1
              },
              {
                "key": "uk",
                "value": 84
              },
              {
                "key": "uae",
                "value": 4
              },
              {
                "key": "turkey",
                "value": 2
              },
              {
                "key": "thailand",
                "value": 5
              },
              {
                "key": "tanzania",
                "value": 1
              },
              {
                "key": "taiwan",
                "value": 8
              },
              {
                "key": "syria",
                "value": 2
              },
              {
                "key": "switzerland",  
                "value": 14
              },
              {
                "key": "sweden",
                "value": 4
              },
              {
                "key": "sri-lanka",
                "value": 1
              },
              {
                "key": "spain",
                "value": 5
              },
              {
                "key": "south-korea",  
                "value": 8
              },
              {
                "key": "south-africa", 
                "value": 3
              },
              {
                "key": "singapore",
                "value": 5
              },
              {
                "key": "saudi-arabia", 
                "value": 7
              },
              {
                "key": "qatar",
                "value": 3
              },
              {
                "key": "portugal",
                "value": 1
              },
              {
                "key": "poland",
                "value": 1
              },
              {
                "key": "philippines",  
                "value": 11
              },
              {
                "key": "panama",
                "value": 1
              },
              {
                "key": "pakistan",
                "value": 4
              },
              {
                "key": "oman",
                "value": 1
              },
              {
                "key": "nigeria",
                "value": 2
              },
              {
                "key": "new-zealand",  
                "value": 11
              },
              {
                "key": "netherlands",  
                "value": 8
              },
              {
                "key": "nepal",
                "value": 1
              },
              {
                "key": "mexico",
                "value": 1
              },
              {
                "key": "malaysia",
                "value": 4
              },
              {
                "key": "liechtenstein",
                "value": 1
              },
              {
                "key": "libya",
                "value": 1
              },
              {
                "key": "lebanon",
                "value": 2
              },
              {
                "key": "kuwait",
                "value": 4
              },
              {
                "key": "japan",
                "value": 47
              },
              {
                "key": "italy",
                "value": 10
              },
              {
                "key": "ireland",
                "value": 1
              },
              {
                "key": "iraq",
                "value": 5
              },
              {
                "key": "iran",
                "value": 6
              },
              {
                "key": "indonesia",
                "value": 13
              },
              {
                "key": "india",
                "value": 6
              },
              {
                "key": "hungary",
                "value": 1
              }
            ]
          }
        ]
      )
      done()
    })
  })

  it('should be able to limit facet length', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['reuter']}
    }
    q.categories = [
      {
        name: 'places',
        limit: 20
      }
    ]
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(100)
      results.totalHits.should.be.exactly(922)
      results.categories[0].value.length.should.be.exactly(20)
      done()
    })
  })

  it('should be able to mark a facet as active', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['reuter', '1987']}
    }
    q.categories = [{
      name: 'places'
    }]
    q.filter = [{
      field: 'places',
      gte: 'zaire',
      lte: 'zaire'
    }]

    q.pageSize = 100
    si.search(q, function (err, results) {
      // console.log(JSON.stringify(results, null, 2))
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(2)
      results.totalHits.should.be.exactly(2)
      results.categories.should.eql(
        [
          {
            "key": "places",
            "value": [
              {
                "key": "malaysia",
                "value": 2
              },
              {
                "active": true,
                "key": "zaire",
                "value": 2
              },
              {
                "key": "australia",
                "value": 1
              },
              {
                "key": "bolivia",
                "value": 1
              },
              {
                "key": "brazil",
                "value": 1
              },
              {
                "key": "china",
                "value": 1
              },
              {
                "key": "indonesia",
                "value": 1
              },
              {
                "key": "nigeria",
                "value": 1
              },
              { 
                "key": "thailand",
                "value": 1
              }
            ]
          }
        ]
      )
      done()
    })
  })

  it('should be able to mark multiple facets as active', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['reuter', '1987']}
    }
    q.categories = [{
      name: 'places'
    }]
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
    q.pageSize = 100
    si.search(q, function (err, results) {
      // console.log(JSON.stringify(results.categories, null, 2))
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(16)
      results.totalHits.should.be.exactly(16)
      results.categories.should.eql(
        [
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
          } 
        ]
      )
      si.close(function(err){
        done()
      })
    })
  })

})
