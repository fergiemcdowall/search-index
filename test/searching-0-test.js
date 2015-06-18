var assert = require("assert");
var should = require('should');

describe('Searching', function(){
  describe('searching reuters-000.json', function() {
    var data = [];
    var sandboxPath = 'test/sandbox';
    var si = require('../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'error'});
    it('should search on all fields and get results', function(done) {
      var q = {};
      q.query = {'*': ['usa']};
      si.search(q, function(err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.true;
        searchResults.hits.length.should.be.above(1);
        searchResults.hits.length.should.be.exactly(100);
        searchResults.hits[3].id.should.be.exactly('417');
        searchResults.hits[10].id.should.be.exactly('972');
        searchResults.hits[13].id.should.be.exactly('31');
        searchResults.hits[14].id.should.be.exactly('171');
        done();
      });
    }),
    it('should search on all fields and get no results for a valid, yet absent keyword', function(done) {
      var q = {};
      q.query = {'*': ['usaasdadadlkjadj']};
      si.search(q, function(err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.true;
        searchResults.hits.length.should.be.exactly(0);
        done();
      });
    }),
    it('should be able to handle multiword searches', function(done) {
      var q = {};
      q.query = {'*': ['reuter', '1987']};
      si.search(q, function(err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.true;
        searchResults.hits.length.should.be.exactly(100);
        searchResults.totalHits.should.be.exactly(922);
        done();
      });
    }),
    it('should be able to return all results by doing a wildcard (*) search', function(done) {
      var q = {};
      q.query = {'*': ['*']};
      si.search(q, function(err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.true;
        searchResults.hits.length.should.be.exactly(100);
        searchResults.totalHits.should.be.exactly(1000);
        done();
      });
    }),
    it('should be able to handle multi word searches where some words are not present in index', function(done) {
      var q = {};
      q.query = {'*': ['reuter', 'yorkxxxxxxx']};
      si.search(q, function(err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.true;
        searchResults.hits.length.should.be.exactly(0);
//TODO: make this return a full resultset
//        searchResults.totalHits.should.be.exactly(0);
        done();
      });
    }),
    it('should be able to offset', function(done) {
      var q = {};
      q.query = {'*': ['japan']};
      q.offset = 5;
      si.search(q, function(err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.true;
        searchResults.hits.length.should.be.exactly(51);
        searchResults.hits.length.should.be.above(1);
        searchResults.hits[0].id.should.be.exactly('271');
        done();
      });
    }),
    it('should be able to set page size (limit results)', function(done) {
      var q = {};
      q.query = {'*': ['japan']};
      q.pageSize = 5;
      si.search(q, function(err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.true;
        searchResults.hits.length.should.be.exactly(5);
        done();
      });
    }),
    it('should be able to page (set offset and page size)', function(done) {
      var q = {};
      q.query = {'*': ['japan']};
      q.offset = 5;
      q.pageSize = 5;
      si.search(q, function(err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.true;
        searchResults.hits.length.should.be.exactly(5);
        searchResults.hits[0].id.should.be.exactly('271');
        done();
      });
    });
  });
});
