/* global it */
/* global describe */

var should = require('should');
var sandboxPath = 'test/sandbox';
var fs = require('fs');

describe('Filters: ', function () {
  describe('indexing', function () {
    it('should index twitter data', function (done) {
      this.timeout(60000);
      var data = JSON.parse(fs.readFileSync('test/twitter-tests/twitter-tweets.json'));
      var si = require('../../')({indexPath: sandboxPath + '/si-twitter',
                                  logLevel: 'error'});
      var opt = {};
      opt.batchName = 'tweetz';
      opt.fieldOptions = [
        {fieldName: 'tags', filter: true},
        {fieldName: 'user', filter: true}
      ];
      si.add(data, opt, function (err) {
        (err === null).should.be.exactly(true);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });
  }),
  describe('searching', function () {
    it('should be able to search in twitter data', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-twitter',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['search']};
      q.facets = {
        user: {},
        tags: {}
      };
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(8);
        results.totalHits.should.be.exactly(8);

        should.exist(results.facets[0]);
        results.facets[0].key.should.be.exactly('user');
        results.facets[0].value[0].key.should.be.exactly('eklem');
        results.facets[0].value[0].value.should.be.exactly(8);
        results.facets[0].value[1].key.should.be.exactly('GoogleforWork');
        (results.hits.length > 1).should.be.exactly(true);
        results.hits.length.should.be.exactly(8);
        results.hits[0].id.should.be.exactly('4EaEkI');
        results.hits[5].id.should.be.exactly('3swrN');
        results.hits[6].id.should.be.exactly('4bU7P5');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to filter by user', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-twitter',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['search']};
      q.facets = {
        user: {},
        tags: {}
      };
      q.filter = {user: [['GoogleforWork', 'GoogleforWork']]};
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(1);
        results.totalHits.should.be.exactly(1);
        should.exist(results.facets[0]);
        results.hits[0].id.should.be.exactly('4EaEkI');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to filter by tag', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-twitter',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['search']};
      q.facets = {
        user: {},
        tags: {}
      };
      q.filter = {tags: [['search', 'search']]};
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(5);
        results.totalHits.should.be.exactly(5);
        should.exist(results.facets[0]);
        results.hits[0].id.should.be.exactly('TEWP');
        results.hits[3].id.should.be.exactly('4bU7P5');
        results.hits[4].id.should.be.exactly('3VKiNd');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to search on tokens that are only found in metadata', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-twitter',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['eklem']};
      q.facets = {
        user: {},
        tags: {}
      };
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(64);
        results.totalHits.should.be.exactly(64);
        should.exist(results.facets[0]);
        results.hits[7].id.should.be.exactly('3bv9Ry');
        results.hits[8].id.should.be.exactly('4wpSkT');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });
  });
});
