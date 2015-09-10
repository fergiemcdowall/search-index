/* global it */
/* global describe */

var should = require('should');
var sandboxPath = 'test/sandbox';

describe('Range Filters: ', function () {
  describe('searching world bank dataset and filtering on ranges', function () {
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
    it('should be able to filter on a chosen facetrange', function (done) {
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
      q.filter = {
        totalamt: [
          [
            '000000000000000',
            '000000050000000'
          ]
        ]
      };
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(10);
        results.totalHits.should.be.exactly(10);
        results.facets[0].value[0].key.should.be.exactly('000000000000000-000000050000000');
        results.facets[0].value[0].value.should.be.exactly(10);
        results.facets[0].value[1].key.should.be.exactly('000000050000001-100000000000000');
        results.facets[0].value[1].value.should.be.exactly(0);
        results.facets[1].value[0].key.should.be.exactly('K-Z');
        results.facets[1].value[0].value.should.be.exactly(8);
        results.facets[1].value[1].key.should.be.exactly('A-J');
        results.facets[1].value[1].value.should.be.exactly(6);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to show facets', function (done) {
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
          ]
        },
        mjtheme: {
          ranges: [
            ['A', 'F'],
            ['G', 'N'],
            ['O', 'Z']
          ]
        }
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
        results.facets[1].value[0].key.should.be.exactly('O-Z');
        results.facets[1].value[0].value.should.be.exactly(9);
        results.facets[1].value[1].key.should.be.exactly('A-F');
        results.facets[1].value[1].value.should.be.exactly(7);
        results.facets[1].value[2].key.should.be.exactly('G-N');
        results.facets[1].value[2].value.should.be.exactly(1);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to filter on a chosen facetrange and drill down on two values in same filter', function (done) {
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
          ]
        },
        mjtheme: {
          ranges: [
            ['A', 'F'],
            ['G', 'N'],
            ['O', 'Z']
          ]
        }
      };
      q.filter =  {
        mjtheme: [
          ['O', 'Z'],
          ['A', 'F']
        ]
      };
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(4);
        results.totalHits.should.be.exactly(4);
        results.facets[0].value[0].key.should.be.exactly('000000000000000-000000050000000');
        results.facets[0].value[0].value.should.be.exactly(3);
        results.facets[0].value[1].key.should.be.exactly('000000050000001-100000000000000');
        results.facets[0].value[1].value.should.be.exactly(1);
        results.facets[1].value[0].key.should.be.exactly('O-Z');
        results.facets[1].value[0].value.should.be.exactly(4);
        results.facets[1].value[1].key.should.be.exactly('A-F');
        results.facets[1].value[1].value.should.be.exactly(4);
        results.facets[1].value[2].key.should.be.exactly('G-N');
        results.facets[1].value[2].value.should.be.exactly(0);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to filter on a chosen facetrange and drill down on two values in multiple filters', function (done) {
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
          ]
        },
        mjtheme: {
          ranges: [
            ['A', 'F'],
            ['G', 'N'],
            ['O', 'Z']
          ]
        }
      };
      q.filter =  {
        totalamt: [
          ['000000000000000', '000000050000000']
        ],
        mjtheme: [
          ['O', 'Z'],
          ['A', 'F']
        ]
      };
      q.pageSize = 100;
      si.search(q, function (err, results) {
        should.exist(results);
        (err === null).should.be.exactly(true);
        results.hits.length.should.be.exactly(3);
        results.totalHits.should.be.exactly(3);
        results.facets[0].value[0].key.should.be.exactly('000000000000000-000000050000000');
        results.facets[0].value[0].gte.should.be.exactly('000000000000000');
        results.facets[0].value[0].lte.should.be.exactly('000000050000000');
        results.facets[0].value[0].active.should.be.exactly(true);
        results.facets[0].value[0].value.should.be.exactly(3);
        results.facets[0].value[1].key.should.be.exactly('000000050000001-100000000000000');
        results.facets[0].value[1].gte.should.be.exactly('000000050000001');
        results.facets[0].value[1].lte.should.be.exactly('100000000000000');
        should.not.exist(results.facets[0].value[1].active);
        results.facets[0].value[1].value.should.be.exactly(0);
        results.facets[0].value[0].key.should.be.exactly('000000000000000-000000050000000');
        results.facets[0].value[0].gte.should.be.exactly('000000000000000');
        results.facets[0].value[0].lte.should.be.exactly('000000050000000');
        results.facets[0].value[0].active.should.be.exactly(true);
        results.facets[1].value[0].value.should.be.exactly(3);
        results.facets[1].value[1].key.should.be.exactly('A-F');
        results.facets[1].value[1].gte.should.be.exactly('A');
        results.facets[1].value[1].lte.should.be.exactly('F');
        results.facets[1].value[1].active.should.be.exactly(true);
        results.facets[1].value[1].value.should.be.exactly(3);
        results.facets[1].value[2].key.should.be.exactly('G-N');
        results.facets[1].value[2].gte.should.be.exactly('G');
        results.facets[1].value[2].lte.should.be.exactly('N');
        should.not.exist(results.facets[1].value[2].active);
        results.facets[1].value[2].value.should.be.exactly(0);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });
  });
});
