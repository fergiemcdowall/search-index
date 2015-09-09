/* global it */
/* global describe */

var should = require('should');
var sandboxPath = 'test/sandbox';

describe('Searching Reuters and Checking Faceting: ', function () {
  describe('Faceting: ', function () {
    it('should be able to search in indexed data with faceting', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['usa']};
      q.facets = {places: {}};
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(100);
        results.totalHits.should.be.exactly(546);
        results.hits[3].id.should.be.exactly('760');
        results.hits[10].id.should.be.exactly('133');
        results.hits[13].id.should.be.exactly('101');
        results.hits[14].id.should.be.exactly('33');
        results.facets[0].value.length.should.be.exactly(39);
        results.facets[0].key.should.be.exactly('places');
        results.facets[0].value[0].key.should.be.exactly('usa');
        results.facets[0].value[0].gte.should.be.exactly('usa');
        results.facets[0].value[0].lte.should.be.exactly('usa');
        results.facets[0].value[0].value.should.be.exactly(546);
        results.facets[0].value[1].key.should.be.exactly('japan');
        results.facets[0].value[1].gte.should.be.exactly('japan');
        results.facets[0].value[1].lte.should.be.exactly('japan');
        results.facets[0].value[1].value.should.be.exactly(16);
        results.facets[0].value[2].key.should.be.exactly('uk');
        results.facets[0].value[2].gte.should.be.exactly('uk');
        results.facets[0].value[2].lte.should.be.exactly('uk');
        results.facets[0].value[2].value.should.be.exactly(14);
        results.facets[0].value[3].key.should.be.exactly('brazil');
        results.facets[0].value[3].gte.should.be.exactly('brazil');
        results.facets[0].value[3].lte.should.be.exactly('brazil');
        results.facets[0].value[3].value.should.be.exactly(9);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to sort facets by value ascending', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['reuter']};
      q.facets = {places: {sort: 'valueAsc'}};
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(100);
        results.totalHits.should.be.exactly(922);
        results.facets[0].value[0].key.should.be.exactly('algeria');
        results.facets[0].value[0].value.should.be.exactly(1);
        results.facets[0].value[1].key.should.be.exactly('austria');
        results.facets[0].value[1].value.should.be.exactly(1);
        results.facets[0].value[2].key.should.be.exactly('bhutan');
        results.facets[0].value[2].value.should.be.exactly(1);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to sort facets by key ascending', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['reuter']};
      q.facets = {places: {sort: 'keyAsc'}};
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(100);
        results.totalHits.should.be.exactly(922);
        results.hits.length.should.be.exactly(100);
        results.facets[0].value[0].key.should.be.exactly('algeria');
        results.facets[0].value[0].value.should.be.exactly(1);
        results.facets[0].value[1].key.should.be.exactly('argentina');
        results.facets[0].value[1].value.should.be.exactly(5);
        results.facets[0].value[2].key.should.be.exactly('australia');
        results.facets[0].value[2].value.should.be.exactly(17);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to sort facets by key descending', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['reuter']};
      q.facets = {places: {sort: 'keyDesc'}};
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(100);
        results.totalHits.should.be.exactly(922);
        results.facets[0].value[0].key.should.be.exactly('zambia');
        results.facets[0].value[0].value.should.be.exactly(1);
        results.facets[0].value[1].key.should.be.exactly('zaire');
        results.facets[0].value[1].value.should.be.exactly(2);
        results.facets[0].value[2].key.should.be.exactly('yemen-demo-republic');
        results.facets[0].value[2].value.should.be.exactly(1);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to limit facet length', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['reuter']};
      q.facets = {places: {sort: 'keyDesc', limit: 20}};
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(100);
        results.totalHits.should.be.exactly(922);
        results.hits.length.should.be.exactly(100);
        results.facets[0].value.length.should.be.exactly(20);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to mark a facet as active', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['reuter', '1987']};
      q.facets = {places: {sort: 'keyDesc', limit: 20}};
      q.filter = {places: [['zaire', 'zaire']]};
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(2);
        results.totalHits.should.be.exactly(2);
        results.facets[0].value[0].key.should.be.exactly('zaire');
        results.facets[0].value[0].value.should.be.exactly(2);
        results.facets[0].value[0].active.should.be.exactly(true);
        results.facets[0].value[1].key.should.be.exactly('thailand');
        results.facets[0].value[1].value.should.be.exactly(1);
        (results.facets[0].value[1].active === undefined).should.be.exactly(true);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to mark multiple facets as active', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['reuter', '1987']};
      q.facets = {places: {sort: 'valueDesc', limit: 20}};
      q.filter = {places: [['usa', 'usa'], ['japan', 'japan']]};
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(16);
        results.totalHits.should.be.exactly(16);
        results.facets[0].value[0].key.should.be.exactly('usa');
        results.facets[0].value[0].value.should.be.exactly(16);
        results.facets[0].value[0].active.should.be.exactly(true);
        results.facets[0].value[1].key.should.be.exactly('japan');
        results.facets[0].value[1].value.should.be.exactly(16);
        results.facets[0].value[1].active.should.be.exactly(true);
        results.facets[0].value[2].key.should.be.exactly('uk');
        results.facets[0].value[2].value.should.be.exactly(4);
        (results.facets[0].value[2].active === undefined).should.be.exactly(true);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });
  });
});
