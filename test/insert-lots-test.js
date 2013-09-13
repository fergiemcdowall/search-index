var buster = require('buster');
var fs = require('fs');
var si = require('../lib/search-index/index.js');

buster.testCase("insert stuff into search index", {
  setUp: function (done) {
    this.timeout = 60000;
    var filters = ['places','topics','organisations'];
    var batch = fs.readFileSync('testdata/reuters-000.json');
    si.index(batch, 'afile.json', filters, function(msg) {
      done(assert(true));
    });
  },
  'querying': function (done) {
    var q = {};
    q['query'] = ['moscow'];
    q['offset'] = 0;
    q['pageSize'] = 10;
    si.search(q, function(msg) {
      console.log(q);
      fs.writeFileSync('response-query-moscow-offset-0-pagesize-10.json', JSON.stringify(msg));
      console.log(msg);
      var correctResponse =
        fs.readFileSync('test-responses/response-query-moscow-offset-0-pagesize-10.json');
      if (JSON.stringify(msg) == correctResponse) {
        done(assert(true));
      }
      else {
        done(assert(false));
      }
    });
  }
});

