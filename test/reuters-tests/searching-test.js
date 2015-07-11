/* global it */
/* global describe */

var should = require('should');
var sandboxPath = 'test/sandbox';

describe('Searching Reuters: ', function () {
  describe('searching reuters-000.json', function () {
    it('should search on all fields and get results', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                  logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['usa']};  //TODO: add error message if this is
      //      not an array
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.above(1);
        searchResults.hits.length.should.be.exactly(100);
        searchResults.hits[3].id.should.be.exactly('760');
        searchResults.hits[10].id.should.be.exactly('133');
        searchResults.hits[13].id.should.be.exactly('101');
        searchResults.hits[14].id.should.be.exactly('33');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should search on all fields and get no results for a valid, yet absent keyword', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['usaasdadadlkjadj']};
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(0);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to handle multiword searches', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['reuter', '1987']};
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(100);
        searchResults.totalHits.should.be.exactly(922);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to return all results by doing a wildcard (*) search', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['*']};
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(100);
        searchResults.totalHits.should.be.exactly(1000);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to handle multi word searches where some words are not present in index', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['reuter', 'yorkxxxxxxx']};
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(0);
        //TODO: make this return a full resultset
        //        searchResults.totalHits.should.be.exactly(0);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to offset', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['japan']};
      q.offset = 5;
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(51);
        searchResults.hits.length.should.be.above(1);
        searchResults.hits[0].id.should.be.exactly('287');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to set page size (limit results)', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['japan']};
      q.pageSize = 5;
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(5);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to page (set offset and page size)', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['japan']};
      q.offset = 5;
      q.pageSize = 5;
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(5);
        searchResults.hits[0].id.should.be.exactly('287');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to search in indexed data with faceting', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['usa']};
      q.facets = {places: {}};
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(100);
        searchResults.hits[3].id.should.be.exactly('760');
        searchResults.hits[10].id.should.be.exactly('133');
        searchResults.hits[13].id.should.be.exactly('101');
        searchResults.hits[14].id.should.be.exactly('33');
        searchResults.facets[0].value.length.should.be.exactly(39);
        searchResults.facets[0].key.should.be.exactly('places');
        searchResults.facets[0].value[0].key.should.be.exactly('usa');
        searchResults.facets[0].value[0].value.should.be.exactly(546);
        searchResults.facets[0].value[1].key.should.be.exactly('japan');
        searchResults.facets[0].value[1].value.should.be.exactly(16);
        searchResults.facets[0].value[2].key.should.be.exactly('uk');
        searchResults.facets[0].value[2].value.should.be.exactly(14);
        searchResults.facets[0].value[3].key.should.be.exactly('brazil');
        searchResults.facets[0].value[3].value.should.be.exactly(9);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to filter search results', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      //TODO: this test generates an empty facetRanges object which
      //should be removed
      var q = {};
      q.query = {'*': ['usa']};
      q.facets = {places: {}};
      q.filter = {places: [['japan', 'japan']]};
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(16);
        searchResults.hits[0].id.should.be.exactly('287');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to search on all fields', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['reagan']};
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(20);
        searchResults.hits[0].id.should.be.exactly('28');
        searchResults.hits[1].id.should.be.exactly('804');
        searchResults.hits[2].id.should.be.exactly('231');
        searchResults.hits[3].id.should.be.exactly('386');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to search on one field', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {title: ['reagan']};
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(10);
        searchResults.hits[8].id.should.be.exactly('877');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to search on one field for two terms', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {title: ['reagan', 'baker']};
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(4);
        searchResults.hits[0].id.should.be.exactly('386');
        searchResults.hits[1].id.should.be.exactly('790');
        searchResults.hits[2].id.should.be.exactly('796');
        searchResults.hits[3].id.should.be.exactly('804');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to search on on two fields for seperate terms', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {
        title: ['reagan'],
        body: ['intelligence']
      };
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(4);
        searchResults.hits[0].id.should.be.exactly('801');
        searchResults.hits[1].id.should.be.exactly('869');
        searchResults.hits[2].id.should.be.exactly('386');
        searchResults.hits[3].id.should.be.exactly('28');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to search on on two fields for multiple terms', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {
        title: ['reagan'],
        body: ['intelligence', 'agency', 'contra']
      };
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(2);
        searchResults.hits[0].id.should.be.exactly('869');
        searchResults.hits[1].id.should.be.exactly('386');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to weight search results', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {
        title: ['reagan'],
        body: ['reagan']
      };
      q.weight = {body: 20};
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(8);
        searchResults.hits[0].id.should.be.exactly('28');
        searchResults.hits[1].id.should.be.exactly('231');
        searchResults.hits[4].id.should.be.exactly('869');
        searchResults.hits[5].id.should.be.exactly('877');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to generate teasers', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['advertising']};
      q.teaser = 'title';
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(3);
        searchResults.hits[0].document.teaser.should.be
          .exactly('GREY <span class=\"sc-em\">advertising</span> <GREY> FORMS NEW DIVISION');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to display information about the index', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      //TODO: there should probably be an error object in this function
      si.tellMeAboutMySearchIndex(function (info) {
        should.exist(info);
        info.totalDocs.should.be.exactly(1000);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to filter on a chosen facetrange and drill down on two values in multiple filters', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['reuter']};
      q.facets = {topics:{}, places: {}, organisations: {}};
      q.filter = {places:[['usa', 'usa'], ['japan', 'japan']]};
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(16);
        searchResults.totalHits.should.be.exactly(16);
        searchResults.hits[0].id.should.be.exactly('676');
        searchResults.hits[1].id.should.be.exactly('753');
        searchResults.hits[2].id.should.be.exactly('287');
        searchResults.hits[3].id.should.be.exactly('893');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to display information about the index', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      //TODO: there should probably be an error object in this function
      si.tellMeAboutMySearchIndex(function (info) {
        should.exist(info);
        info.totalDocs.should.be.exactly(1000);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('should be able to filter on a chosen facetrange and drill down on two values in multiple filters', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'warn'});
      var q = {};
      q.query = {'*': ['reuter']};
      q.facets = {topics: {}, places: {}, organisations: {}};
      q.filter = {topics:[['earn', 'earn'], ['alum', 'alum']]};
      si.search(q, function (err, searchResults) {
        should.exist(searchResults);
        (err === null).should.be.exactly(true);
        searchResults.hits.length.should.be.exactly(2);
        searchResults.totalHits.should.be.exactly(2);
        searchResults.hits[0].id.should.be.exactly('938');
        searchResults.hits[1].id.should.be.exactly('921');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });
  });
});
