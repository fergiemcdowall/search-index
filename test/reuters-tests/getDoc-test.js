var assert = require("assert");
var should = require('should');
var _ = require('lodash');


describe('get-ting on the reuters dataset: ', function() {
  it('should be able to retreive a document by its id', function(done){
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.get(9, function(err, result) {
      (err === null).should.be.true;
      result.id.should.be.exactly('9');
      result.title.should.be.exactly('CHAMPION PRODUCTS <CH> APPROVES STOCK SPLIT');
      result.date.should.be.exactly('26-FEB-1987 15:17:11.20'); 
      si.close(function(err) {
        done();
      })
    });
  }),
  it('should be able to retreive a document by its id', function(done){
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.get(92827382, function(err, result) {
      (err === null).should.be.true;
      si.close(function(err){
        done();
      })
    });
  })  
});
