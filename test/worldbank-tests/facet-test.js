/* global it */
/* global describe */

var should = require('should');
var sandboxPath = 'test/sandbox';

describe('Searching World Bank and Checking Faceting: ', function () {
  describe('searching world bank dataset', function () {
    it('should be able to search and do facet ranges', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-world-bank',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['africa']};
      q.facets = {
        totalamt: {
          ranges: [
            [
              '000000000000000',
              '000000006000000'
            ],
            [
              '000000006000001',
              '010000000000000'
            ]
          ]
        },
        mjtheme: {
          ranges: [
            [
              'A',
              'J'
            ],
            [
              'K',
              'Z'
            ]
          ]
        }
      };
      q.pageSize = 10;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(10);
        results.totalHits.should.be.exactly(206);
        results.facets[0].value[0].key.should.be.exactly('000000006000001-010000000000000');
        results.facets[0].value[0].gte.should.be.exactly('000000006000001');
        results.facets[0].value[0].lte.should.be.exactly('010000000000000');
        results.facets[0].value[0].value.should.be.exactly(121);
        results.facets[0].value[1].key.should.be.exactly('000000000000000-000000006000000');
        results.facets[0].value[1].gte.should.be.exactly('000000000000000');
        results.facets[0].value[1].lte.should.be.exactly('000000006000000');
        results.facets[0].value[1].value.should.be.exactly(85);
        results.facets[1].value[0].key.should.be.exactly('K-Z');
        results.facets[1].value[0].gte.should.be.exactly('K');
        results.facets[1].value[0].lte.should.be.exactly('Z');
        results.facets[1].value[0].value.should.be.exactly(161);
        results.facets[1].value[1].key.should.be.exactly('A-J');
        results.facets[1].value[1].gte.should.be.exactly('A');
        results.facets[1].value[1].lte.should.be.exactly('J');
        results.facets[1].value[1].value.should.be.exactly(135);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to search for more than 1 word and show facetranges', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-world-bank',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['africa', 'bank']};
      q.facets = {
        totalamt: {
          ranges:[
            [
              '000000000000000',
              '000000050000000'
            ],
            [
              '000000050000001',
              '100000000000000'
            ]
          ]},
        mjtheme: {
          ranges: [
            [
              'A',
              'J'
            ],
            [
              'K',
              'Z'
            ]
          ]}
      };
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(12);
        results.totalHits.should.be.exactly(12);
        results.facets[0].value[0].key.should.be.exactly('000000000000000-000000050000000');
        results.facets[0].value[0].value.should.be.exactly(10);
        results.facets[0].value[1].key.should.be.exactly('000000050000001-100000000000000');
        results.facets[0].value[1].value.should.be.exactly(2);
        results.facets[1].value[0].key.should.be.exactly('K-Z');
        results.facets[1].value[0].value.should.be.exactly(9);
        results.facets[1].value[1].key.should.be.exactly('A-J');
        results.facets[1].value[1].value.should.be.exactly(8);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to search for more than 1 word and no ranges (experiment)', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-world-bank',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['africa', 'bank']};
      q.facets = {
        totalamt: {},
        mjtheme: {}
      };
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(12);
        results.totalHits.should.be.exactly(12);
        results.facets[0].value.length.should.be.exactly(4);
        results.facets[0].value[0].value.should.be.exactly(9);
        results.facets[0].value[1].value.should.be.exactly(1);
        results.facets[0].value[2].value.should.be.exactly(1);
        results.facets[0].value[3].value.should.be.exactly(1);
        results.facets[1].value.length.should.be.exactly(8);
        results.facets[1].value[0].value.should.be.exactly(5);
        results.facets[1].value[1].value.should.be.exactly(4);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to limit facet length', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-world-bank',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['africa', 'bank']};
      q.facets = {
        totalamt: {sort:'valueAsc', limit: 2},
        mjtheme: {sort:'valueDesc', limit: 3}
      };
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(12);
        results.totalHits.should.be.exactly(12);
        results.facets[0].value.length.should.be.exactly(2);
        results.facets[1].value.length.should.be.exactly(3);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to search for more than 1 word with a mix of ranged and unranged facets', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-world-bank',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['africa', 'bank']};
      q.facets = {
        totalamt: {sort: 'keyDesc'},
        mjtheme: {
          sort: 'keyAsc',
          ranges: [
              [
                'A',
                'J'
              ],
              [
                'K',
                'Z'
              ]
          ]
        }
      };
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(12);
        results.totalHits.should.be.exactly(12);
        results.facets[0].value.length.should.be.exactly(4);
        results.facets[0].value[0].key.should.be.exactly('000000300000000');
        results.facets[0].value[0].value.should.be.exactly(1);
        results.facets[0].value[1].key.should.be.exactly('000000070000000');
        results.facets[0].value[1].value.should.be.exactly(1);
        results.facets[0].value[2].key.should.be.exactly('000000020000000');
        results.facets[0].value[2].value.should.be.exactly(1);
        results.facets[0].value[3].key.should.be.exactly('000000000000000');
        results.facets[0].value[3].value.should.be.exactly(9);
        results.facets[1].value.length.should.be.exactly(2);
        results.facets[1].value[0].key.should.be.exactly('A-J');
        results.facets[1].value[0].value.should.be.exactly(8);
        results.facets[1].value[1].key.should.be.exactly('K-Z');
        results.facets[1].value[1].value.should.be.exactly(9);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });
  });
});
