/* global it */
/* global describe */

var should = require('should');
var sandboxPath = 'test/sandbox';
var fs = require('fs');
var logLevel = 'error';
if (process.env.NODE_ENV == 'TEST') logLevel = 'info';

describe('indexing options: ', function () {
  var siOps1 = {indexPath: sandboxPath + '/si-reuters-10-indexing-ops-1',
               fieldedSearchOnAllFieldsByDefault: false,
               logLevel: logLevel}

  var siOps2 = {indexPath: sandboxPath + '/si-reuters-10-indexing-ops-2',
                fieldedSearchOnAllFieldsByDefault: true,
                logLevel: logLevel}


  it('should index one file of test data', function (done) {
    this.timeout(5000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
    var si = require('../../')(siOps1);
    var opt = {};
    opt.batchName = 'reuters';
    opt.canDoFieldedSearchOn = ['title']
    opt.filters = ['places', 'topics'];
    si.add(opt, data, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should search on title', function (done) {
    var si = require('../../')(siOps1);
    var q = {};
    q.query = {'title': ['stock']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      searchResults.totalHits.should.be.exactly(1);
      searchResults.hits[0].id.should.be.exactly('9');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  })

  it('should be able to search for a term in the body by using the composite (*) field', function (done) {
    var si = require('../../')(siOps1);
    var q = {};
    q.query = {'*': ['marathon']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      searchResults.totalHits.should.be.exactly(1);
      searchResults.hits[0].id.should.be.exactly('8');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  })

  it('should be NOT able to search for a term in the body by using the body field since "body" was not specified as a canDoFieldedSearchOn field', function (done) {
    var si = require('../../')(siOps1);
    var q = {};
    q.query = {'body': ['marathon']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(0);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  })

  it('should be NOT able to search for any term at all in the body by using the body field since "body" was not specified as a canDoFieldedSearchOn field', function (done) {
    var si = require('../../')(siOps1);
    var q = {};
    q.query = {'body': ['*']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(0);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  })

  it('should index one file of test data into an index with fielded search turned on', function (done) {
    this.timeout(5000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
    var si = require('../../')(siOps2);
    var opt = {};
    opt.batchName = 'reuters';
    opt.canDoFieldedSearchOn = ['title']
    opt.filters = ['places', 'topics'];
    si.add(opt, data, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('SHOULD able to search for a term in the body by using the body field since fieldedSearchOnAllFieldsByDefault is true', function (done) {
    var si = require('../../')(siOps2);
    var q = {};
    q.query = {'body': ['marathon']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  })

  it('SHOULD able to do facets', function (done) {
    var si = require('../../')(siOps2);
    var q = {};
    q.query = {'*': ['reuter']};
    q.facets = {places: {}};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(10);
      searchResults.facets[0].key.should.be.exactly('places');
      searchResults.facets[0].value.should.eql([ { key: 'usa', gte: 'usa', lte: 'usa', value: 9 },
                                               { key: 'argentina', gte: 'argentina', lte: 'argentina', value: 1 },
                                               { key: 'brazil', gte: 'brazil', lte: 'brazil', value: 1 },
                                               { key: 'el-salvador',
                                                 gte: 'el-salvador',
                                                 lte: 'el-salvador',
                                                 value: 1 },
                                               { key: 'uruguay', gte: 'uruguay', lte: 'uruguay', value: 1 } ]);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  })


});
