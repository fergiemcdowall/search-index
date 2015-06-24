var assert = require("assert");
var should = require('should');
var fs = require('fs');
var _ = require('lodash');


describe('Indexing World Bank: ', function(){
  describe('indexing the world bank dataset', function() {
    var data = [];
    var sandboxPath = 'test/sandbox';
    it('should find the data and set up a sandbox', function(){
      data = JSON.parse(fs.readFileSync('node_modules/world-bank-dataset/world-bank-projects.json'));
      assert.equal(data.length, 500);
      assert.equal(data[0].id, 'P129828');
      try {
        stats = fs.lstatSync(sandboxPath);
        assert(stats.isDirectory());
      }
      catch (e) {
        console.log(e);
        assert(false);
      }
    }),
    it('should throw an error when indexing an empty batch', function(done) {
      var si = require('../../')({indexPath: sandboxPath + '/si-empty',
                                  logLevel: 'error'});
      si.add({}, [], function(err) {
        err.should.be.an.Error;
        err.toString().should.equal('Error: No docs to add');
        si.close(function(err){done();})
      });
    }),
    it('should index the data', function(done) {
      var si = require('../../')({indexPath:'si-world-bank',
                                  logLevel: 'error'});
      this.timeout(60000);
      var padInt = function(intIn) {
        return ("000000000000000" + intIn).slice(-15);
      }
      var processDoc = function(datum) {
        delete datum._id;
        delete datum.projectdocs;
        delete datum.theme1;
        delete datum.majorsector_percent;
        delete datum.mjsector_namecode;
        delete datum.mjtheme_namecode;
        delete datum.sector;
        delete datum.sector_namecode;
        delete datum.sector1;
        delete datum.sector2;
        delete datum.theme_namecode;
        datum.totalamt = [padInt(datum.totalamt)];
        return datum;
      };
      var opt = {};
      opt.batchName = 'world-bank-projects.json';
      opt.filters = ['mjtheme', 'totalamt'];
      si.add(opt, _.map(data, processDoc), function(err) {
        (err === null).should.be.exactly(true);
        si.close(function(err){done();})
      });
    });
  });
});
