/* global it */
/* global describe */

const should = require('should')
const sandboxPath = 'test/sandbox'
const searchindex = require('../../../')

var logLevel = 'error'
if (process.env.NODE_ENV === 'TEST') logLevel = 'info'

describe('indexing options: ', function () {

  var si1, si2, si3, si4, si5, si6, si7, si8, si9
  var data = require('../../../node_modules/reuters-21578-json/data/justTen/justTen.json')

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-reuters-10-indexing-ops-1',
      fieldedSearch: false,
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si1 = thisSi
      done()
    })
  })

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-reuters-10-indexing-ops-2',
      fieldedSearch: true,
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si2 = thisSi
      done()
    })
  })

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-reuters-10-indexing-ops-3',
      fieldedSearch: true,
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si3 = thisSi
      done()
    })
  })

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-reuters-10-indexing-ops-4',
      fieldedSearch: false,
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si4 = thisSi
      done()
    })
  })

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-reuters-10-indexing-ops-5',
      fieldedSearch: false,
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si5 = thisSi
      done()
    })
  })

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-reuters-10-indexing-ops-6',
      fieldedSearch: false,
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si6 = thisSi
      done()
    })
  })

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-reuters-10-indexing-ops-7',
      fieldedSearch: false,
      deletable: false,
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si7 = thisSi
      done()
    })
  })

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-reuters-10-indexing-ops-8',
      fieldedSearch: false,
      deletable: false,
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si8 = thisSi
      done()
    })
  })

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-reuters-10-indexing-ops-9',
      fieldedSearch: false,
      deletable: false,
      logLevel: logLevel
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si9 = thisSi
      done()
    })
  })

  it('should index one file of test data and set canDoFieldedSearchOn to "title"', function (done) {
    this.timeout(5000)
    var opt = {}
    opt.batchName = 'reuters'
    opt.fieldOptions = [
      {fieldName: 'title', fieldedSearch: true},
      {fieldName: 'places', filter: true},
      {fieldName: 'topics', filter: true}
    ]
    si1.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should search on title', function (done) {
    var q = {}
    q.query = {
      AND: {title: ['stock']}
    }
    si1.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      searchResults.totalHits.should.be.exactly(1)
      searchResults.hits[0].id.should.be.exactly('9')
      done()
    })
  })

  it('should be able to search for a term in the body by using the composite (*) field', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['marathon']}
    }
    si1.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      searchResults.totalHits.should.be.exactly(1)
      searchResults.hits[0].id.should.be.exactly('8')
      done()
    })
  })

  it('should be NOT able to search for a term in the body by using the body field since "body" was not specified as a canDoFieldedSearchOn field', function (done) {
    var q = {}
    q.query = {
      AND: {body: ['marathon']}
    }
    si1.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(0)
      done()
    })
  })

  it('should be NOT able to search for any term at all in the body by using the body field since "body" was not specified as a canDoFieldedSearchOn field', function (done) {
    var q = {}
    q.query = {
      AND:{body: ['*']}
    }
    si1.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(0)
      done()
    })
  })

  it('should index one file of test data into an index with fielded search turned on', function (done) {
    this.timeout(25000)
    var opt = {}
    opt.batchName = 'reuters'
    opt.fieldOptions = [
      {fieldName: 'title', canDoFieldedSearchOn: true},
      {fieldName: 'places', filter: true},
      {fieldName: 'topics', filter: true}
    ]
    si2.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('SHOULD able to search for a term in the body by using the body field since fieldedSearchOnAllFieldsByDefault is true', function (done) {
    var q = {}
    q.query = {
      AND: {body: ['marathon']}
    }
    si2.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      done()
    })
  })

  it('SHOULD be able to do facets', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['reuter']}
    }
    q.categories = [{
      field: 'places'
    }]
    si2.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(10)
      // searchResults.facets[0].key.should.be.exactly('places')
      searchResults.categories.should.eql(
        [
          {
            "key": "places",
            "value": [
              {
                "key": "usa",
                "value": 9
              },
              { 
                "key": "argentina",
                "value": 1
              },
              { 
                "key": "brazil",
                "value": 1
              },
              { 
                "key": "el-salvador",
                "value": 1
              },
              { 
                "key": "uruguay",
                "value": 1
              }
            ]
          }
        ]
      )
      done()
    })
  })

  it('should index one file of test data and set nonSearchableFields to "body"', function (done) {
    this.timeout(5000)
    var opt = {}
    opt.batchName = 'reuters'
    opt.fieldOptions = [
      {fieldName: 'body', searchable: false}
    ]
    si3.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('SHOULD NOT able to search for a term in the body by using the composite field since nonSearchableFields included "body" when data was indexed', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['marathon']}
    }
    si3.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(0)
      done()
    })
  })

  it('SHOULD able to search for a term in the title since nonSearchableFields included "body" when data was indexed', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['grain/oilseed']}
    }
    si3.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      done()
    })
  })

  it('should index one file of test data and set fieldedSearchOnAllFieldsByDefault to false', function (done) {
    this.timeout(5000)
    var opt = {}
    opt.batchName = 'reuters'
    opt.fieldOptions = [
      {fieldName: 'places', filter: true},
      {fieldName: 'topics', filter: true}
    ]
    si4.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('SHOULD NOT able to do fielded search on the body field', function (done) {
    var q = {}
    q.query = {
      AND: {body: ['marathon']}
    }
    si4.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(0)
      done()
    })
  })

  it('SHOULD be able to do fielded search on the composite (*) field', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['marathon']}
    }
    si4.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      done()
    })
  })

  it('should index one file of test data and set fieldedSearchOnAllFieldsByDefault to false', function (done) {
    this.timeout(5000)
    var opt = {}
    opt.batchName = 'reuters'
    opt.fieldOptions = [
      {fieldName: 'body', searchable: false}
    ]
    si5.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('SHOULD NOT able to do fielded search on the body field', function (done) {
    var q = {}
    q.query = {
      AND: {body: ['marathon']}
    }
    si5.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(0)
      done()
    })
  })

  it('SHOULD NOT able to do search on the composite field for tokens present in the body field defined in nonSearchableFields', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['marathon']}
    }
    si5.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(0)
      done()
    })
  })

  it('SHOULD be able to do search on the composite field for tokens present in the title field', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['grain/oilseed']}
    }
    si5.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(1)
      done()
    })
  })

  it('should index one file of test data and weight the body field', function (done) {
    this.timeout(5000)
    var opt = {}
    opt.batchName = 'reuters'
    opt.fieldOptions = [
      //      {fieldName: 'title', weight: 5},
    ]
    si6.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('SHOULD be able to do search on the composite field for tokens present in the title field', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['stock']}
    }
    si6.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(4)
      searchResults.hits[0].id.should.be.exactly('4')
      searchResults.hits[1].id.should.be.exactly('9')
      searchResults.hits[2].id.should.be.exactly('10')
      searchResults.hits[3].id.should.be.exactly('8')
      done()
    })
  })

  it('should index one file of test data non-deletably', function (done) {
    this.timeout(5000)
    var opt = {}
    opt.batchName = 'reuters'
    si7.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('SHOULD be able to do a search', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['stock']}
    }
    si7.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(4)
      searchResults.hits[0].id.should.be.exactly('4')
      searchResults.hits[1].id.should.be.exactly('9')
      searchResults.hits[2].id.should.be.exactly('10')
      searchResults.hits[3].id.should.be.exactly('8')
      done()
    })
  })

  it('SHOULD NOT be able to delete a doc', function (done) {
    si7.del('4', function (err) {
      should.exist(err)
      err.toString().should.be.exactly('Error: this index is non-deleteable, and some of the documents you are deleting have IDs that are already present in the index. Either reinitialize index with "deletable: true" or alter the IDs of the new documents')
      done()
    })
  })

  it('doc should still be present in index', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['stock']}
    }
    si7.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(4)
      searchResults.hits[0].id.should.be.exactly('4')
      searchResults.hits[1].id.should.be.exactly('9')
      searchResults.hits[2].id.should.be.exactly('10')
      searchResults.hits[3].id.should.be.exactly('8')
      done()
    })
  })

  it('should index one file of test data and only store id and title (but allow all other fields to be searchable)', function (done) {
    this.timeout(5000)
    var opt = {}
    opt.fieldsToStore = ['id', 'title']
    opt.batchName = 'reuters'
    si8.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('doc should still be present in index', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['stock']}
    }
    si8.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(4)
      searchResults.hits[0].id.should.be.exactly('4')
      searchResults.hits[1].id.should.be.exactly('9')
      searchResults.hits[2].id.should.be.exactly('10')
      searchResults.hits[3].id.should.be.exactly('8')
      searchResults.hits[0].document.should.eql({
        id: '4',
        title: 'TALKING POINT/BANKAMERICA <BAC> EQUITY OFFER'
      })
      done()
    })
  })

  it('should index one file of test data and only store title (but allow all other fields to be searchable)', function (done) {
    this.timeout(5000)
    var opt = {}
    opt.fieldsToStore = ['title']
    opt.batchName = 'reuters'
    si9.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('doc should still be present in index', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['stock']}
    }
    si9.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(4)
      searchResults.hits[0].id.should.be.exactly('4')
      searchResults.hits[1].id.should.be.exactly('9')
      searchResults.hits[2].id.should.be.exactly('10')
      searchResults.hits[3].id.should.be.exactly('8')
      searchResults.hits[0].document.should.eql({
        title: 'TALKING POINT/BANKAMERICA <BAC> EQUITY OFFER'
      })
      done()
    })
  })
})


