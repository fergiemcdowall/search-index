var fs = require('fs');
var should = require('should');
var sandboxPath = 'test/sandbox';

describe('Configuration: ', function () {
  it('should accept configuration', function () {
    siPath = sandboxPath + '/si-config';
    si = require('../../')({ indexPath: siPath });
    should.exist(si);
    fs.existsSync(siPath).should.be.exactly(true);
  }),
  it('does not leak variables', function () {
    (typeof countDocuments).should.be.exactly('undefined');
    (typeof _).should.be.exactly('undefined');
  });
});
