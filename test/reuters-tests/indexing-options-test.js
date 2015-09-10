/* global it */
/* global describe */

var should = require('should');
var sandboxPath = 'test/sandbox';
var fs = require('fs');
var logLevel = 'error';
if (process.env.NODE_ENV == 'TEST') logLevel = 'info';

describe('indexing options: ', function () {
  var siOps1 = {indexPath: sandboxPath + '/si-reuters-10-indexing-ops-1',
               fieldedSearch: false,
               logLevel: logLevel};

  var siOps2 = {indexPath: sandboxPath + '/si-reuters-10-indexing-ops-2',
                fieldedSearch: true,
                logLevel: logLevel};

  var siOps3 = {indexPath: sandboxPath + '/si-reuters-10-indexing-ops-3',
                fieldedSearch: true,
                logLevel: logLevel};

  var siOps4 = {indexPath: sandboxPath + '/si-reuters-10-indexing-ops-4',
                fieldedSearch: false,
                logLevel: logLevel};

  var siOps5 = {indexPath: sandboxPath + '/si-reuters-10-indexing-ops-5',
                fieldedSearch: false,
                logLevel: logLevel};

  var siOps6 = {indexPath: sandboxPath + '/si-reuters-10-indexing-ops-6',
                fieldedSearch: false,
                logLevel: logLevel};

  var siOps7 = {indexPath: sandboxPath + '/si-reuters-10-indexing-ops-7',
                fieldedSearch: false,
                deletable: false,
                logLevel: logLevel};

  var siOps8 = {indexPath: sandboxPath + '/si-reuters-10-indexing-ops-8',
                fieldedSearch: false,
                deletable: false,
                logLevel: logLevel};

  var siOps9 = {indexPath: sandboxPath + '/si-reuters-10-indexing-ops-9',
                fieldedSearch: false,
                deletable: false,
                logLevel: logLevel};

  it('should index one file of test data and set canDoFieldedSearchOn to "title"', function (done) {
    this.timeout(5000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
    var si = require('../../')(siOps1);
    var opt = {};
    opt.batchName = 'reuters';
    opt.fieldOptions = [
      {fieldName: 'title', fieldedSearch: true},
      {fieldName: 'places', filter: true},
      {fieldName: 'topics', filter: true}
    ];
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should search on title', function (done) {
    var si = require('../../')(siOps1);
    var q = {};
    q.query = {title: ['stock']};
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
  });

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
  });

  it('should be NOT able to search for a term in the body by using the body field since "body" was not specified as a canDoFieldedSearchOn field', function (done) {
    var si = require('../../')(siOps1);
    var q = {};
    q.query = {body: ['marathon']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(0);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be NOT able to search for any term at all in the body by using the body field since "body" was not specified as a canDoFieldedSearchOn field', function (done) {
    var si = require('../../')(siOps1);
    var q = {};
    q.query = {body: ['*']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(0);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should index one file of test data into an index with fielded search turned on', function (done) {
    this.timeout(25000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
    var si = require('../../')(siOps2);
    var opt = {};
    opt.batchName = 'reuters';
    opt.fieldOptions = [
      {fieldName: 'title', canDoFieldedSearchOn: true},
      {fieldName: 'places', filter: true},
      {fieldName: 'topics', filter: true}
    ];
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('SHOULD able to search for a term in the body by using the body field since fieldedSearchOnAllFieldsByDefault is true', function (done) {
    var si = require('../../')(siOps2);
    var q = {};
    q.query = {body: ['marathon']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

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
      searchResults.facets[0].value.should.eql(
        [ { key: 'usa', gte: 'usa', lte: 'usa', value: 9 },
          { key: 'uruguay', gte: 'uruguay', lte: 'uruguay', value: 1 },
          { key: 'el-salvador',
            gte: 'el-salvador',
            lte: 'el-salvador',
            value: 1 },
          { key: 'brazil', gte: 'brazil', lte: 'brazil', value: 1 },
          { key: 'argentina', gte: 'argentina', lte: 'argentina', value: 1 } ]
      );
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should index one file of test data and set nonSearchableFields to "body"', function (done) {
    this.timeout(5000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
    var si = require('../../')(siOps3);
    var opt = {};
    opt.batchName = 'reuters';
    opt.fieldOptions = [
      {fieldName: 'body', searchable: false}
    ];
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('SHOULD NOT able to search for a term in the body by using the composite field since nonSearchableFields included "body" when data was indexed', function (done) {
    var si = require('../../')(siOps3);
    var q = {};
    q.query = {'*': ['marathon']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(0);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('SHOULD able to search for a term in the title since nonSearchableFields included "body" when data was indexed', function (done) {
    var si = require('../../')(siOps3);
    var q = {};
    q.query = {'*': ['GRAIN/OILSEED']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should index one file of test data and set fieldedSearchOnAllFieldsByDefault to false', function (done) {
    this.timeout(5000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
    var si = require('../../')(siOps4);
    var opt = {};
    opt.batchName = 'reuters';
    opt.fieldOptions = [
      {fieldName: 'places', filter: true},
      {fieldName: 'topics', filter: true}
    ];
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('SHOULD NOT able to do fielded search on the body field', function (done) {
    var si = require('../../')(siOps4);
    var q = {};
    q.query = {body: ['marathon']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(0);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('SHOULD be able to do fielded search on the composite (*) field', function (done) {
    var si = require('../../')(siOps4);
    var q = {};
    q.query = {'*': ['marathon']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should index one file of test data and set fieldedSearchOnAllFieldsByDefault to false', function (done) {
    this.timeout(5000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
    var si = require('../../')(siOps5);
    var opt = {};
    opt.batchName = 'reuters';
    opt.fieldOptions = [
      {fieldName: 'body', searchable: false}
    ];
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('SHOULD NOT able to do fielded search on the body field', function (done) {
    var si = require('../../')(siOps5);
    var q = {};
    q.query = {body: ['marathon']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(0);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('SHOULD NOT able to do search on the composite field for tokens present in the body field defined in nonSearchableFields', function (done) {
    var si = require('../../')(siOps5);
    var q = {};
    q.query = {'*': ['marathon']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(0);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('SHOULD be able to do search on the composite field for tokens present in the title field', function (done) {
    var si = require('../../')(siOps5);
    var q = {};
    q.query = {'*': ['GRAIN/OILSEED']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(1);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should index one file of test data and weight the body field', function (done) {
    this.timeout(5000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
    var si = require('../../')(siOps6);
    var opt = {};
    opt.batchName = 'reuters';
    opt.fieldOptions = [
//      {fieldName: 'title', weight: 5},
    ];
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('SHOULD be able to do search on the composite field for tokens present in the title field', function (done) {
    var si = require('../../')(siOps6);
    var q = {};
    q.query = {'*': ['stock']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(4);
      searchResults.hits[0].id.should.be.exactly('9');
      searchResults.hits[1].id.should.be.exactly('4');
      searchResults.hits[2].id.should.be.exactly('10');
      searchResults.hits[3].id.should.be.exactly('8');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should index one file of test data non-deletably', function (done) {
    this.timeout(5000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
    var si = require('../../')(siOps7);
    var opt = {};
    opt.batchName = 'reuters';
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('SHOULD be able to do a search', function (done) {
    var si = require('../../')(siOps7);
    var q = {};
    q.query = {'*': ['stock']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(4);
      searchResults.hits[0].id.should.be.exactly('9');
      searchResults.hits[1].id.should.be.exactly('4');
      searchResults.hits[2].id.should.be.exactly('10');
      searchResults.hits[3].id.should.be.exactly('8');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('SHOULD NOT be able to delete a doc', function (done) {
    var si = require('../../')(siOps7);
    si.del('4', function (err) {
      should.exist(err);
      err.toString().should.be.exactly('Error: this index is non-deleteable- set "deletable: true" in startup options');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('doc should still be present in index', function (done) {
    var si = require('../../')(siOps7);
    var q = {};
    q.query = {'*': ['stock']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(4);
      searchResults.hits[0].id.should.be.exactly('9');
      searchResults.hits[1].id.should.be.exactly('4');
      searchResults.hits[2].id.should.be.exactly('10');
      searchResults.hits[3].id.should.be.exactly('8');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should index one file of test data and only store id and title (but allow all other fields to be searchable)', function (done) {
    this.timeout(5000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
    var si = require('../../')(siOps8);
    var opt = {};
    opt.fieldsToStore = ['id', 'title'];
    opt.batchName = 'reuters';
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('doc should still be present in index', function (done) {
    var si = require('../../')(siOps8);
    var q = {};
    q.query = {'*': ['stock']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(4);
      searchResults.hits[0].id.should.be.exactly('9');
      searchResults.hits[1].id.should.be.exactly('4');
      searchResults.hits[2].id.should.be.exactly('10');
      searchResults.hits[3].id.should.be.exactly('8');
      searchResults.hits[0].document.should.eql({
        id: '9',
        title: 'CHAMPION PRODUCTS <CH> APPROVES STOCK SPLIT'
      });
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should index one file of test data and only store title (but allow all other fields to be searchable)', function (done) {
    this.timeout(5000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
    var si = require('../../')(siOps9);
    var opt = {};
    opt.fieldsToStore = ['title'];
    opt.batchName = 'reuters';
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('doc should still be present in index', function (done) {
    var si = require('../../')(siOps9);
    var q = {};
    q.query = {'*': ['stock']};
    si.search(q, function (err, searchResults) {
      should.exist(searchResults);
      (err === null).should.be.exactly(true);
      searchResults.hits.length.should.be.exactly(4);
      searchResults.hits[0].id.should.be.exactly('9');
      searchResults.hits[1].id.should.be.exactly('4');
      searchResults.hits[2].id.should.be.exactly('10');
      searchResults.hits[3].id.should.be.exactly('8');
      searchResults.hits[0].document.should.eql({
        title: 'CHAMPION PRODUCTS <CH> APPROVES STOCK SPLIT'
      });
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

});
