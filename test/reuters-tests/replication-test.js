/* global it */
/* global describe */

var should = require('should');
var sandboxPath = 'test/sandbox';
var fs = require('fs');

describe('Replication, Reuters: ', function () {

  describe('Replication: ', function () {
    it('should index one file of test data', function (done) {
      this.timeout(5000);
      var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10-replication',
                                  logLevel: 'error'});
      var opt = {};
      opt.batchName = 'reuters';
      si.add(data, opt, function (err) {
        (err === null).should.be.exactly(true);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });

    it('should be able to create a snapshot', function (done) {
      this.timeout(5000);
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10-replication',
                                  logLevel: 'error'});
      si.snapShot(function (rs) {
        rs.pipe(fs.createWriteStream(sandboxPath + '/backup.gz'))
          .on('close', function () {
            (true).should.be.exactly(true);
            si.close(function (err) {
              if (err) false.should.eql(true);done();
            });
          })
          .on('error', function (err) {
            (err === null).should.be.exactly(true);
          });
      });
    });

    it('should empty the index', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10-replication',
                                  logLevel: 'error'});
      si.empty(function (err) {
        //Is this a bug in levelUP? Should undefined be null?
        (err === undefined).should.be.exactly(true);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });

    it('should be able to display information about the index (index is empty)', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10-replication',
                                  logLevel: 'error'});
      si.tellMeAboutMySearchIndex(function (result) {
        result.totalDocs.should.be.exactly(0);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });

    it('should be able to refeed from a snapshot', function (done) {
      this.timeout(5000);
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10-replication',
                                  logLevel: 'error'});
      si.replicate(fs.createReadStream(sandboxPath + '/backup.gz'), function (err) {
        (err === undefined).should.be.exactly(true);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });

    it('should be able to display information about the index (index has 10 docs)', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10-replication',
                                  logLevel: 'error'});
      si.tellMeAboutMySearchIndex(function (result) {
        should.exist(result);
        result.totalDocs.should.be.exactly(10);
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });

  });
});
