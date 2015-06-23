var assert = require("assert");
var should = require('should');
var fs = require('fs');
var rimraf = require('rimraf');
var _ = require('lodash');


describe('Indexing', function(){
  describe('indexing reuters-000.json', function() {
    var data = [];
    var sandboxPath = 'test/sandbox';
    var si = null;
    try {rimraf.sync(sandboxPath);}catch(e){};
    try {fs.mkdirSync(sandboxPath, 0755);}catch(e){};
    it('should find the data and set up a sandbox', function(){
      data = JSON.parse(fs.readFileSync('node_modules/reuters-21578-json/data/full/reuters-000.json'));
      assert.equal(data.length, 1000);
      assert.equal(data[0].id, '1');
      try {
        stats = fs.lstatSync(sandboxPath);
        si = require('../')({indexPath: sandboxPath + '/si-reuters', logLevel: 'error'});
        assert(stats.isDirectory());
      }
      catch (e) {
        console.log(e);
        assert(false);
      }
    }),
    it('should index the data', function(done) {
      this.timeout(60000);
      var opt = {};
      opt.batchName = 'reuters';
      opt.filters = ['places', 'topics'];
      si.add(opt, data, function(err) {
        (err === null).should.be.true;
        done();
      });
    }),
    it('should verify indexing', function(done) {
      var opt = {};
      opt.batchName = 'reuters';
      opt.filters = ['places', 'topics'];
      si.tellMeAboutMySearchIndex(function(info) {
        should.exist(info);
        (info.totalDocs).should.be.exactly(1000);
        done();
      });
    });
  });
});
