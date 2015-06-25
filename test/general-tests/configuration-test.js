var fs = require('fs');
var should = require('should');

describe('Configuration: ', function() {
  it('should accept configuration', function(){
    si = require('../../')({ indexPath: 'si-config' });
    should.exist(si);
    fs.existsSync('si-config').should.be.exactly(true);
  }),
  it('does not leak variables', function(){
    (typeof countDocuments).should.be.exactly('undefined');
    (typeof _).should.be.exactly('undefined');
  });
});
