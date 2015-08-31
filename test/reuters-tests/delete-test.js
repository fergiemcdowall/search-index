/* global it */
/* global describe */

var should = require('should');
var fs = require('fs');

describe('deleting and reindexing: ', function () {
  it('should index documents', function (done) {
    this.timeout(10000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/full/reuters-000.json'));
    var singleDoc = [];
    singleDoc.push(data['746']);
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    var opt = {batchName: 'document 747'};
    opt.fieldOptions = [
      {fieldName: 'places', filter: true},
      {fieldName: 'topics', filter: true}
    ];
    si.add(singleDoc, opt, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('document is present in search', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.search({query: {'*': ['western']}}, function (err, result) {
      should.exist(result);
      (err === null).should.be.exactly(true);
      result.totalHits.should.be.exactly(14);
      result.hits.length.should.be.exactly(14);
      result.hits[5].id.should.be.exactly('747');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should be able to delete documents from index (747)', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.del('747', function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should verify delete', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.get('747', function (err, doc) {
      (err === null).should.be.exactly(false);
      (doc === null).should.be.exactly(true);
      err.toString().should.be.exactly('NotFoundError: Key not found in database [DOCUMENT￮747￮]');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('verifies recalibration after delete', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.indexValue({key:'TF￮*￮mccaw￮￮'}, function (err, value) {
      (err === null).should.be.exactly(false);
      (value === null).should.be.exactly(true);
      err.toString().should.be.exactly('NotFoundError: Key not found in database [TF￮*￮mccaw￮￮]');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('verifies recalibration after delete', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.indexValue({key:'TF￮*￮1987￮￮'}, function (err, value) {
      (err === null).should.be.exactly(true);
      value.length.should.be.exactly(999);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('deleted document is not appearing in results', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.search({query: {'*': ['western']}}, function (err, result) {
      should.exist(result);
      (err === null).should.be.exactly(true);
      result.totalHits.should.be.exactly(13);
      result.hits.length.should.be.exactly(13);
      for (var i = 0; i < result.hits.length; i++)
        result.hits[i].id.should.not.be.exactly('747');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should reindex deleted document', function (done) {
    this.timeout(10000);
    var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/full/reuters-000.json'));
    var singleDoc = [];
    singleDoc.push(data['746']);
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    var opt = {batchName: 'document 747'};
    opt.fieldOptions = [
      {fieldName: 'places', filter: true},
      {fieldName: 'topics', filter: true}
    ];
    si.add(singleDoc, opt, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('document reappears in search', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.search({query: {'*': ['western']}}, function (err, result) {
      should.exist(result);
      (err === null).should.be.exactly(true);
      result.totalHits.should.be.exactly(14);
      result.hits.length.should.be.exactly(14);
      result.hits[5].id.should.be.exactly('747');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('verifies recalibration after document is REadded', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.indexValue({key:'TF￮*￮mccaw￮￮'}, function (err, value) {
      (err === null).should.be.exactly(true);
      value.should.have.lengthOf(1);
      value[0].should.be.exactly('747');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('verifies recalibration after document is REadded', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.indexValue({key:'TF￮*￮1987￮￮'}, function (err, value) {
      (err === null).should.be.exactly(true);
      value.length.should.be.exactly(1000);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });
});
