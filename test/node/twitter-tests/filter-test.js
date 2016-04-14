/* global it */
/* global describe */

var should = require('should')

var si

it('should initialize the search index', function (done) {
  require('../../../')({
    indexPath: 'test/sandbox/si-twitter',
    logLevel: 'error'
  }, function (err, thisSi) {
    if (err) false.should.eql(true)
    si = thisSi
    done()
  })
})

it('should index twitter data', function (done) {
  this.timeout(60000)
  var data = require('./twitter-tweets.json')
  var opt = {}
  opt.batchName = 'tweetz'
  opt.fieldOptions = [
    {fieldName: 'tags', filter: true},
    {fieldName: 'user', filter: true}
  ]
  si.add(data, opt, function (err) {
    (err === null).should.be.exactly(true)
    done()
  })
})

it('should be able to search in twitter data', function (done) {
  var q = {}
  q.query = {
    AND: {'*': ['search']}
  }
  q.categories = [{
    field: 'user'
  }, {
    field: 'tags'
  }]
  q.pageSize = 100
  si.search(q, function (err, results) {
    should.exist(results)
    ;(err === null).should.be.exactly(true)
    results.hits.length.should.be.exactly(8)
    results.totalHits.should.be.exactly(8)
    results.categories.should.eql([ 
      {
        "key": "user",
        "value": [
          {
            "key": "eklem",
            "value": 8
          },
          {
            "key": "GoogleforWork",
            "value": 1
          }
        ]
      },
      {
        "key": "tags",
        "value": [
          {
            "key": "search",
            "value": 5
          },
          {
            "key": "engine",
            "value": 1
          },
          {
            "key": "eso",
            "value": 1
          },
          {
            "key": "events",
            "value": 1
          },
          {
            "key": "searchable",
            "value": 1
          },
          {
            "key": "sites",
            "value": 1
          }
        ]
      }
    ])
    results.hits.map(function (item) { return item.id }).should.eql(
      [
        "1NsXUW",
        "TEWP",
        "4bU7P5",
        "4EaEkI",
        "3swrN",
        "3VKiNd",
        "3FceLy",
        "2PHH0R"
      ]
    )
    done()
  })
})

it('should be able to filter by user', function (done) {
  var q = {}
  q.query = {
    AND: {'*': ['search']}
  }
  q.categories = [{
    field: 'user'
  }, {
    field: 'tags'
  }]
  q.pageSize = 100
  q.filter = [
    {
      field: 'user',
      gte: 'GoogleforWork',
      lte: 'GoogleforWork'
    }
  ]
  si.search(q, function (err, results) {
    // console.log(JSON.stringify(results, null, 2))
    should.exist(results)
    ;(err === null).should.be.exactly(true)
    results.hits.length.should.be.exactly(1)
    results.totalHits.should.be.exactly(1)
    results.categories.should.eql([
      {
        "key": "user",
        "value": [
          {
            "active": true,
            "key": "GoogleforWork",
            "value": 1
          },
          {
            "key": "eklem",
            "value": 1
          }
        ]
      },
      {
        "key": "tags",
        "value": []
      }
    ])
    results.hits.map(function (item) { return item.id }).should.eql(
      [
        "4EaEkI"
      ]
    )
    done()
  })
})

it('should be able to filter by tag', function (done) {
  var q = {}
  q.query = {
    AND: {'*': ['search']}
  }
  q.categories = [{
    field: 'user'
  }, {
    field: 'tags'
  }]
  q.pageSize = 100
  q.filter = [
    {
      field: 'tags',
      gte: 'search',
      lte: 'search'
    }
  ]
  si.search(q, function (err, results) {
     // console.log(JSON.stringify(results, null, 2))

    should.exist(results)
    ;(err === null).should.be.exactly(true)
    results.hits.length.should.be.exactly(5)
    results.totalHits.should.be.exactly(5)
    results.categories.should.eql(
      [
        {
          "key": "user",
          "value": [
            {
              "key": "eklem",
              "value": 5
            } 
          ] 
        },
        {
          "key": "tags",
          "value": [
            {
              "active": true,
              "key": "search",
              "value": 5
            },
            {
              "key": "engine",
              "value": 1
            },
            {
              "key": "eso",
              "value": 1
            },
            {
              "key": "events",
              "value": 1
            },
            {
              "key": "searchable",
              "value": 1
            },
            {
              "key": "sites",
              "value": 1
            } 
          ]
        } 
      ]
    )
    results.hits.map(function (item) { return item.id }).should.eql(
      [ 
        "TEWP",
        "4bU7P5",
        "3swrN",
        "3VKiNd",
        "2PHH0R"
      ]
    )
    done()
  })
})



it('should be able to search on tokens that are only found in metadata', function (done) {
  var q = {}
  q.query = {
    AND: {'*': ['eklem']}
  }
  q.categories = [{
    field: 'user'
  }, {
    field: 'tags'
  }]
  q.pageSize = 100
  si.search(q, function (err, results) {
    should.exist(results)
    ;(err === null).should.be.exactly(true)
    results.hits.length.should.be.exactly(64)
    results.totalHits.should.be.exactly(64)
    results.hits.slice(0, 10).map(function (item) { return item.id }).should.eql(
      [
        'tLl7B',
        'f9Fl9',
        'dVjBP',
        'cwHKO',
        'c7hQ1',
        'Vd7wZ',
        'TEWP',
        'RHidS',
        'QQjZg',
        'LlDnO'
      ])
    done()
  })
})

