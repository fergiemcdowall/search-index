/* global it */
/* global describe */

var should = require('should')

describe('Matching Reuters: ', function () {
  it('should search on all fields and get results', function (done) {
    require('../../')({
      indexPath: 'test/sandbox/si-reuters',
      logLevel: 'warn'
    }, function (err, si) {
      si.match({beginsWith: 'lon'}, function (err, matches) {
        should.exist(matches)
        ;(err === null).should.be.exactly(true)
        matches.length.should.be.exactly(6)
        matches[0].should.be.exactly('long')
        matches[1].should.be.exactly('london')
        matches[2].should.be.exactly('longer')
        matches[3].should.be.exactly('longrange')
        matches[4].should.be.exactly('longstanding')
        matches[5].should.be.exactly('longtime')
        si.close(function(err){
          done()
        })
      })
    })
  })
})
