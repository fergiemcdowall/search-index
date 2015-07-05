/* global it */
/* global describe */

var should = require('should');
var sandboxPath = 'test/sandbox';

describe('Matching Reuters: ', function () {
  describe('Matches stuff in reuters-000.json', function () {
    it('should search on all fields and get results', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                  logLevel: 'warn'});
      var str = 'lon';
      si.match(str, function (err, matches) {
        should.exist(matches);
        (err === null).should.be.exactly(true);
        matches.length.should.be.exactly(6);
        matches[0].should.be.exactly('long');
        matches[1].should.be.exactly('london');
        matches[2].should.be.exactly('longer');
        matches[3].should.be.exactly('longrange');
        matches[4].should.be.exactly('longstanding');
        matches[5].should.be.exactly('longtime');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    }),
    it('handles match strings that are below threshold', function (done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-reuters',
                                  logLevel: 'warn'});
      var str = 'lo';
      si.match(str, function (err, matches) {
        should.exist(matches);
        matches.length.should.be.exactly(0);
        (err instanceof Error).should.be.exactly(true);
        err.toString().should.be.exactly('Error: string below threshold length (3)');
        si.close(function (err) {
          if (err) false.should.eql(true);done();
        });
      });
    });
  });
});
