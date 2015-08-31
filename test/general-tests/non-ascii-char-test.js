/* global it */
/* global describe */

var should = require('should');
var sandboxPath = 'test/sandbox';

describe('Indexing and searching non-ascii characters: ', function () {
  var data = [
    {
      id: 1,
      names: 'ståle synnøve Kjærsti',
      test: 'this doc should give hits smør for peaches peaches all of the tokens in the names field'
    },
    {
      id: 2,
      names: 'Gerät Grünnerløkka',
      test: 'everything in names doc field smør should be searchable searchable searchable'
    }];
  it('should index test data', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-non-ascii',
                                logLevel: 'error'});
    si.add(data, {}, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should be able to search in test data', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-non-ascii',
                                logLevel: 'error'});
    var q = {};
    q.query = {'*': ['ståle', 'synnøve', 'kjærsti']};
    si.search(q, function (err, results) {
      should.exist(results);
      (err === null).should.be.exactly(true);
      results.hits.length.should.be.exactly(1);
      results.totalHits.should.be.exactly(1);
      results.hits[0].id.should.be.exactly('1');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  }),
  it('should be able to search in test data', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-non-ascii',
                                logLevel: 'error'});
    var q = {};
    q.query = {'*': ['gerät', 'grünnerløkka']};
    si.search(q, function (err, results) {
      should.exist(results);
      (err === null).should.be.exactly(true);
      results.hits.length.should.be.exactly(1);
      results.totalHits.should.be.exactly(1);
      results.hits[0].id.should.be.exactly('2');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });
});
