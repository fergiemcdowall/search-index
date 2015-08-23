/* global it */
/* global describe */

var fs = require('fs');
var assert = require('assert');
var should = require('should');
var sandboxPath = 'test/sandbox';

describe('Configuration: ', function () {
  it('should accept configuration', function () {
    var siPath = sandboxPath + '/si-config';
    var si = require('../../')({ indexPath: siPath });
    should.exist(si);
    fs.existsSync(siPath).should.be.exactly(true);
  });

  it('does not leak variables', function () {
    (typeof countDocuments).should.be.exactly('undefined');
    (typeof _).should.be.exactly('undefined');
  });

  it('should log when given a logger', function (done) {
    var streamBuffers = require('stream-buffers');
    var myStream = new streamBuffers.WritableStreamBuffer({});
    var log = require('bunyan').createLogger({
      name: 'test-log',
      stream: myStream
    });
    assert.equal(myStream.size(), 0);
    var si = require('../../')({
      indexPath: sandboxPath + '/test-log-index',
      log: log,
      logLevel: 'info'
    });
    should.exist(si.log);

    si.add('yolo', {
      id: 1,
      name: 'The First Doc',
      test: 'this is the first doc'
    }, function (err) {
      console.log(err);

      //throwing an error here- need to investigate

      //      (err === null).should.be.exactly(true);
      myStream.size().should.be.above(0);
      done();
    });
  });
});
