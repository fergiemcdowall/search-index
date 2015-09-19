/* global it */
/* global describe */

var assert = require('assert');
var _ = require('lodash');
var sandboxPath = 'test/sandbox';

var doc = {
  id: undefined,
  title: 'Mad Science is on the Rise',
  body: 'Mad,  mad things are happening.'
};

describe('Indexing API', function () {
  var si = require('../../')({indexPath: sandboxPath + '/indexing-test', logLevel: 'error'});

  it('should allow indexing a plain object (as opposed to an array)', function (done)  {
    si.add(_.assign(doc, {id: 1}), {}, function (err) {
      assert.equal(err, null);
      si.get(1, function (err, res) {
        assert(_.isEqual(res, doc));
        done();
      });
    });
  });

  it('should allow indexing with undefined options object', function (done)  {
    si.add([_.assign(doc, {id: 2})], undefined, function (err) {
      assert.equal(err, null);
      si.get(2, function (err, res) {
        assert(_.isEqual(res, doc));
        done();
      });
    });
  });

  it('should allow indexing with options object omitted', function (done)  {
    si.add([_.assign(doc, {id: 3})], function (err) {
      assert.equal(err, null);
      si.get(3, function (err, res) {
        assert(_.isEqual(res, doc));
        done();
      });
    });
  });

  it('should allow indexing with a custom separator', function (done)  {
    si.add([{
      id: 'testing',
      content: 'Nexion Smart ERP 14.2.1.0\n\nRelease de ejemplo'
    }], {
      separator:/[ (\n)]+/
    }, function (err) {
      assert.equal(err, null);
      si.search({query: {'*': ['14.2.1.0']}}, function (err, res) {
        assert(res.totalHits == 1)
        done();
      });
    });
  });


});
