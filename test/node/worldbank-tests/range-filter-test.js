/* global it */
/* global describe */

const should = require('should')

describe('searching world bank dataset and filtering on ranges', function () {

  var si

  it('should initialize the search index', function (done) {
    require('../../../')({
      indexPath: 'test/sandbox/si-world-bank',
      logLevel: 'error'
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si = thisSi
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
        lte: '000000050000000'
      },
      {
        field: 'totalamt',
        gte: '000000050000001',
        lte: '100000000000000'
      },
      {
        field: 'mjtheme',
        gte: 'A',
        lte: 'J'
      },
      {
        field: 'mjtheme',
        gte: 'K',
        lte: 'Z'
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
          },
          {
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

  it('should be able to filter on a chosen facetrange', function (done) {
    var q = {}
    q.query = {
      AND: [{'*': ['africa', 'bank']}]
    }
    q.buckets = [
      {
        field: 'totalamt',
        gte: '000000000000000',
        lte: '000000050000000'
      },
      {
        field: 'totalamt',
        gte: '000000050000001',
        lte: '100000000000000'
      },
      {
        field: 'mjtheme',
        gte: 'A',
        lte: 'J'
      },
      {
        field: 'mjtheme',
        gte: 'K',
        lte: 'Z'
      }      
    ]
    q.filter = [
      {
        field: 'totalamt',
        gte: '000000000000000',
        lte: '000000050000000'
      }
    ]
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(10)
      results.totalHits.should.be.exactly(10)

      results.buckets.should.eql([
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
          "count": 0
        },
        {
          "field": "mjtheme",
          "gte": "A",
          "lte": "J",
          "count": 6
        },
        {
          "field": "mjtheme",
          "gte": "K",
          "lte": "Z",
          "count": 8
        }
      ])
      done()
    })
  })

  it('should be able to show facets', function (done) {
    var q = {}
    q.query = {
      AND: [{'*': ['africa', 'bank']}]
    }
    q.buckets = [
      {
        field: 'totalamt',
        gte: '000000000000000',
        lte: '000000050000000'
      },
      {
        field: 'totalamt',
        gte: '000000050000001',
        lte: '100000000000000'
      },
      {
        field: 'mjtheme',
        gte: 'A',
        lte: 'F'
      },
      {
        field: 'mjtheme',
        gte: 'G',
        lte: 'N'
      },
      {
        field: 'mjtheme',
        gte: 'O',
        lte: 'Z'
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
            "lte": "F",
            "count": 7
          },
          {
            "field": "mjtheme",
            "gte": "G",
            "lte": "N",
            "count": 1
          },
          {
            "field": "mjtheme",
            "gte": "O",
            "lte": "Z",
            "count": 9
          }
        ]
      )
      done()
    })
  })

  it('should be able to filter on a chosen facetrange and drill down on two values in same filter', function (done) {
    var q = {}
    q.query = {
      AND: [{'*': ['africa', 'bank']}]
    }
    q.buckets = [
      {
        field: 'totalamt',
        gte: '000000000000000',
        lte: '000000050000000'
      },
      {
        field: 'totalamt',
        gte: '000000050000001',
        lte: '100000000000000'
      },
      {
        field: 'mjtheme',
        gte: 'A',
        lte: 'F'
      },
      {
        field: 'mjtheme',
        gte: 'G',
        lte: 'N'
      },
      {
        field: 'mjtheme',
        gte: 'O',
        lte: 'Z'
      }      
    ]
    q.filter = [
      {
        field: 'mjtheme',
        gte: 'O',
        lte: 'Z'
      },
      {
        field: 'mjtheme',
        gte: 'A',
        lte: 'F'
      }
    ]
    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(4)
      results.totalHits.should.be.exactly(4)

      results.buckets.should.eql(
        [ 
          {
            "field": "totalamt",
            "gte": "000000000000000",
            "lte": "000000050000000",
            "count": 3
          },
          {
            "field": "totalamt",
            "gte": "000000050000001",
            "lte": "100000000000000",
            "count": 1
          },
          {
            "field": "mjtheme",
            "gte": "A",
            "lte": "F",
            "count": 4
          },
          {
            "field": "mjtheme",
            "gte": "G",
            "lte": "N",
            "count": 0
          },
          {
            "field": "mjtheme",
            "gte": "O",
            "lte": "Z",
            "count": 4
          }
        ]
      )
      done()
    })
  })

  it('should be able to filter on a chosen facetrange and drill down on two values in multiple filters', function (done) {
    var q = {}


    q.query = {
      AND: [{'*': ['africa', 'bank']}]
    }
    q.buckets = [
      {
        field: 'totalamt',
        gte: '000000000000000',
        lte: '000000050000000'
      },
      {
        field: 'totalamt',
        gte: '000000050000001',
        lte: '100000000000000'
      },
      {
        field: 'mjtheme',
        gte: 'A',
        lte: 'F'
      },
      {
        field: 'mjtheme',
        gte: 'G',
        lte: 'N'
      },
      {
        field: 'mjtheme',
        gte: 'O',
        lte: 'Z'
      }      
    ]
    q.filter = [
      {
        field: 'mjtheme',
        gte: 'O',
        lte: 'Z'
      },
      {
        field: 'mjtheme',
        gte: 'A',
        lte: 'F'
      },
      {
        field: 'totalamt',
        gte: '000000000000000',
        lte: '000000050000000'
      }
    ]

    q.pageSize = 100
    si.search(q, function (err, results) {
      should.exist(results)
      ;(err === null).should.be.exactly(true)
      results.hits.length.should.be.exactly(3)
      results.totalHits.should.be.exactly(3)
      results.buckets.should.eql(
        [ 
          {
            "field": "totalamt",
            "gte": "000000000000000",
            "lte": "000000050000000",
            "count": 3
          },
          {
            "field": "totalamt",
            "gte": "000000050000001",
            "lte": "100000000000000",
            "count": 0
          },
          {
            "field": "mjtheme",
            "gte": "A",
            "lte": "F",
            "count": 3
          },
          {
            "field": "mjtheme",
            "gte": "G",
            "lte": "N",
            "count": 0
          },
          {
            "field": "mjtheme",
            "gte": "O",
            "lte": "Z",
            "count": 3
          }
        ]
      )
      si.close(function(err){
        done()
      })
    })
  })

})
