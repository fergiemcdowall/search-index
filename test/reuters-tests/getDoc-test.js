/* global it */
/* global describe */

var should = require('should');

describe('get-ting on the reuters dataset: ', function () {
  it('should be able to retreive a document by its id', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.get(9, function (err, result) {
      (err === null).should.be.exactly(true);
      result.id.should.be.exactly('9');
      result.title.should.be.exactly('CHAMPION PRODUCTS <CH> APPROVES STOCK SPLIT');
      result.date.should.be.exactly('26-FEB-1987 15:17:11.20');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should be able to retreive a document by its id', function (done) {
    var sandboxPath = 'test/sandbox';
    var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                logLevel: 'error'});
    si.get(92827382, function (err, result) {
      should.exist(err);
      (err === null).should.be.exactly(false);
      (result === null).should.be.exactly(true);
      err.toString().should.be.exactly('NotFoundError: Key not found in database [DOCUMENT￮92827382￮]');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });
});
