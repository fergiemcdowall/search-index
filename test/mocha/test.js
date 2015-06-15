var assert = require("assert")
var fs = require('fs');
var si = require('../../')({indexPath:'si-world-bank', logLevel: 'info'});
var _ = require('lodash');

describe('Array', function(){
  describe('#indexOf()', function(){
    it('should return -1 when the value is not present', function(){
      assert.equal(-1, [1,2,3].indexOf(5));
      assert.equal(-1, [1,2,3].indexOf(0));
    })
  })
})


describe('Indexing', function(){
  describe('indexing the world bank dataset', function() {
    var data = [];
    it('should find the data and set up a sandbox', function(){
      var data = JSON.parse(fs.readFileSync('node_modules/world-bank-dataset/world-bank-projects.json'));
      var sandboxPath = 'test/sandbox';
      assert.equal(data.length, 500);
      assert.equal(data[0].id, 'P129828');
      try {
        fs.mkdirSync(sandboxPath, 0755);
        stats = fs.lstatSync(sandboxPath);
        assert(stats.isDirectory());
      }
      catch (e) {
        assert(false);
      }
    })
    it('should index the data', function(){
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
      si.add({'batchName': 'world-bank-projects.json',
              'filters': ['mjtheme', 'totalamt']},
             _.map(data, processDoc), function(err) {
               done();
             });
    })
  })
})
