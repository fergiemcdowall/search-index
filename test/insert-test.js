var buster = require('buster');
var fs = require('fs');
var si = require('../lib/search-index/index.js');

buster.testCase("insert stuff into search index", {
  setUp: function(done) {
    this.timeout = 60000;
    var thisholder = this;
    this.batchFileName = 'testdata/justOne.json';
    this.facets = ['places','topics','organisations'];
    fs.readFile(this.batchFileName, 'utf8', function (err, data) {
      if (err) {
        console.log('Error: ' + err);
        done(assert(false));
      }
      thisholder.batch = data;
      done(assert(true));
    });
  },

  'indexing': function (done) {
    si.index(this.batch, this.batchFileName, this.facets, function(msg) {
      done(assert(true));
    });
  },

  'validate reverse index': function (done) {
    si.indexPeek('REVERSEINDEX', 'REVERSEINDEX', function(msg) {
      var reverseIndexAsItShouldBe =
        fs.readFileSync('test-responses/justOneReverseIndex.json');
      if (JSON.stringify(msg) == reverseIndexAsItShouldBe)
        done(assert(true));
      else 
        done(assert(false))
    });
  }

});

