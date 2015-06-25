var should = require('should');
var sandboxPath = 'test/sandbox';
var fs = require('fs');

describe('Replication, Reuters: ', function(){
  describe('Replication: ', function() {
    it('should index one file of test data', function(done) {
      this.timeout(5000);
      var data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/justTen/justTen.json'));
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10',
                                  logLevel: 'warn'});
      var opt = {};
      opt.batchName = 'reuters';
      opt.filters = ['places', 'topics'];
      si.add(opt, data, function(err) {
        (err === null).should.be.exactly(true);
        si.close(function(err){done();})
      });
    }),
    it('should be able to create a snapshot', function(done) {
      this.timeout(5000);
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10',
                                  logLevel: 'warn'});
      si.snapShot(function(rs) {
        rs.pipe(fs.createWriteStream(sandboxPath + '/backup.gz'))
          .on('close', function() {
            (true).should.be.exactly(true);
            si.close(function(err){done();})
          })
          .on('error', function(err) {
            (err === null).should.be.exactly(true);
          });
      });
    }),
    it('should empty the index', function(done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10',
                                  logLevel: 'warn'});
      si.empty(function(err) {
        //Is this a bug in levelUP? Should undefined be null?
        (err === undefined).should.be.exactly(true);
        si.close(function(err){done();})
      });
    }),
    it('should be able to display information about the index (index is empty)', function(done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10',
                                  logLevel: 'warn'});
      si.tellMeAboutMySearchIndex(function(result) {
        result.totalDocs.should.be.exactly(0);
        si.close(function(err){done();})
      });
    }),
    it('should be able to refeed from a snapshot', function(done) {
      this.timeout(5000);
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10',
                                  logLevel: 'warn'});
      si.replicate(fs.createReadStream(sandboxPath + '/backup.gz'), function(err) {
        (err === undefined).should.be.exactly(true);
        si.close(function(err){done();})
      });
    }),
    it('should be able to display information about the index (index has 10 docs)', function(done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters-10',
                                  logLevel: 'warn'});
      si.tellMeAboutMySearchIndex(function(result) {
        result.totalDocs.should.be.exactly(10);
        si.close(function(err){done();})
      });
    })
  })
})
