/* global it */
/* global describe */

var assert = require('assert');
var should = require('should');
var fs = require('fs');
var _ = require('lodash');
var sandboxPath = 'test/sandbox';

var doc = {
  id: undefined,
  title: 'Mad Science is on the Rise',
  body: 'Mad,  mad things are happening.'
};

describe('Indexing API', function () {
  var si = require('../../')({indexPath: sandboxPath + '/indexing-test', logLevel: 'error'});
  it('should allow indexing with undefined options object', function (done)  {
    si.add([_.assign(doc, {id: 1})], undefined, function (err) {
      assert.equal(err, null);
      si.get(1, function (err, res) {
        assert(_.isEqual(res, doc));
        done();
      });
    });
  });
  it('should allow indexing with options object omitted', function (done)  {
    si.add([_.assign(doc, {id: 2})], function (err) {
      assert.equal(err, null);
      si.get(2, function (err, res) {
        assert(_.isEqual(res, doc));
        done();
      });
    });
  });
});
