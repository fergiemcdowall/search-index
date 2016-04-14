/* global it */
/* global describe */

const should = require('should')

describe('Searching World Bank and Checking Faceting: ', function () {

  var si

  it('should initialize the search index', function (done) {
    this.timeout(10000)
    require('../../../')({
      indexPath: 'test/sandbox/si-world-bank',
      logLevel: 'error'
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si = thisSi
      done()
    })
  })


  it('should be able to search and do facet ranges', function (done) {
    var q = {}
    q.query = [
      {
        AND: {'*': ['africa']}
      }
    ]
    q.buckets = [
      {
        field: 'totalamt',
        gte: '000000000000000',
        lte: '000000006000000',
      },
      {
        field: 'totalamt',
        gte: '000000006000001',
        lte: '010000000000000',
      },
      {
        field: 'mjtheme',
        gte: 'A',
        lte: 'J',
      },
      {
        field: 'mjtheme',
        gte: 'K',
        lte: 'Z',
      }
    ]
    q.pageSize = 10
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(10)
      results.totalHits.should.be.exactly(206)
      results.buckets.should.eql(
        [
          {
            "field": "totalamt",
            "gte": "000000000000000",
            "lte": "000000006000000",
            "count": 85
          },
          {
            "field": "totalamt",
            "gte": "000000006000001",
            "lte": "010000000000000",
            "count": 121
          },
          { 
            "field": "mjtheme",
            "gte": "A",
            "lte": "J",
            "count": 135
          },
          { 
            "field": "mjtheme",
            "gte": "K",
            "lte": "Z",
            "count": 161
          }
        ]
      )
      done()
    })
  })

  it('should be able to search for more than 1 word and show facetranges', function (done) {
    var q = {}
    q.query = {
      AND: [{'*': ['africa', 'bank']}]
    }
    q.buckets = [
      {
        field: 'totalamt',
        gte: '000000000000000',
        lte: '000000050000000',
      },
      {
        field: 'totalamt',
        gte: '000000050000001',
        lte: '100000000000000',
      },
      {
        field: 'mjtheme',
        gte: 'A',
        lte: 'J',
      },
      {
        field: 'mjtheme',
        gte: 'K',
        lte: 'Z',
      }
    ]
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(12)
      results.totalHits.should.be.exactly(12)
      results.buckets.should.eql(
        [ 
          {
            "field": "totalamt",
            "gte": "000000000000000",
            "lte": "000000050000000",
            "count": 10
          },
          {
            "field": "totalamt",
            "gte": "000000050000001",  
            "lte": "100000000000000",
            "count": 2
          },
          {
            "field": "mjtheme",
            "gte": "A",
            "lte": "J",
            "count": 8
          },  {
            "field": "mjtheme",
            "gte": "K",
            "lte": "Z",
            "count": 9
          } 
        ] 
      )
      done()
    })
  })


  it('should be able to limit facet length', function (done) {
    var q = {}
    q.query = {
      AND: [{'*': ['africa', 'bank']}]
    }
    q.categories = [
      {
        name: 'totalamt',
        sort: 'valueAsc',
        limit: 2
      },
      {
        name: 'mjtheme',
        sort: 'valueDesc',
        limit: 3
      }
    ]
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(12)
      results.totalHits.should.be.exactly(12)
      results.categories.should.eql(
        [
          {
            "key": "totalamt",
            "value": [
              {
                "key": "000000020000000",
                "value": 1
              },
              {
                "key": "000000070000000",
                "value": 1
              } 
            ] 
          },
          {
            "key": "mjtheme",
            "value": [
              {
                "key": "Financial and private sector development",
                "value": 5
              },
              {
                "key": "Environment and natural resources management",
                "value": 4
              },
              {
                "key": "Social protection and risk management",
                "value": 4
              } 
            ]
          }
        ] 
      )
      done()
    })
  })

  it('should be able to search for more than 1 word with a mix of ranged and unranged facets', function (done) {
    var q = {}
    q.query = {
      AND: [{'*': ['africa', 'bank']}]
    }
    q.categories = [{
      name: 'totalamt',
      sort: 'keyDesc'
    }]
    q.buckets = [{
      field: 'mjtheme',
      gte: 'A',
      lte: 'J'
    }, {
      field: 'mjtheme',
      gte: 'K',
      lte: 'Z'
    }]
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(12)
      results.totalHits.should.be.exactly(12)
      results.categories.should.eql(
        [
          {
            "key": "totalamt",
            "value": [
              {
                "key": "000000300000000",
                "value": 1
              },
              {
                "key": "000000070000000",
                "value": 1
              },
              { 
                "key": "000000020000000",
                "value": 1
              },
              { 
                "key": "000000000000000",
                "value": 9
              }
            ]
          }
        ]
      )
      results.buckets.should.eql(
        [
          {
            "field": "mjtheme",
            "gte": "A",
            "lte": "J",
            "count": 8
          },
          {
            "field": "mjtheme",
            "gte": "K",
            "lte": "Z",
            "count": 9
          } 
        ]
      )
      si.close(function(err){
        done()
      })
    })
  })
})
