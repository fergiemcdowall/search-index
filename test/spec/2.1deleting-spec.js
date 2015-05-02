var fs = require('fs');
var sidel = require('../../')({logLevel:false, indexPath: 'sideleting'});

describe('deleting', function () {

  var data1 = [
    {
      'id':1,
      'name':'The First Doc',
      'test':'this is the first doc'
    },
    {
      'id':2,
      'name':'The Second Doc',
      'test':'this is the second doc'
    },
    {
      'id':3,
      'name':'The Third Doc',
      'test':'this is the third doc'
    },
    {
      'id':4,
      'name':'The Fourth Doc',
      'test':'this is the fourth doc'
    }];

  var data2 = [
    {
      'id':1,
      'name':'The First Doc',
      'test':'this is the first doc'
    }
  ];

  var data3 = [
    {
      'id':2,
      'name':'The Second Doc',
      'test':'this is the second doc'
    },
    {
      'id':3,
      'name':'The Third Doc',
      'test':'this is the third doc'
    },
    {
      'id':4,
      'name':'The Fourth Doc',
      'test':'this is the fourth doc'
    }];


  it('should index test data into the index', function () {
    runs(function() {
      this.err = undefined;
      this.done = false;
      var that = this;
      sidel.add({'batchName': 'data1'}, data1, function(err) {
        that.err = err;
        that.done = true;
      });
    });
    waitsFor(function() {
      return this.done != false;
    }, 'err not to be empty (search err returned)', 1000)
    runs(function () {
      expect(this.err).toEqual(null);
    });
  });


  it('should be able to return all documents in index', function () {
    runs(function () {
      this.searchResults = '';
      var that = this;
      sidel.search({
        'query': {
          '*': ['*']
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
//      console.log(this.searchResults);
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toEqual(4);
    });
  });


  it('should be able to delete a document without throwing errorness', function () {
    runs(function () {
      this.functionErr = 'unreported';
      var that = this;
      sidel.del('2' , function(err) {
        that.functionErr = err;
      });
    });
    waitsFor(function() {
      return this.functionErr != 'unreported';
    }, 'waiting for search results', 1000)
    runs(function() {
      expect(this.functionErr).toBe(null);
    });
  });



  it('should be able to return all documents in index, with one document deleted', function () {
    runs(function () {
      this.searchResults = '';
      var that = this;
      sidel.search({
        'query': {
          '*': ['*']
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
//      console.log(this.searchResults)
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toEqual(3);
      expect(this.searchResults.hits[0].id).toEqual('1');
      expect(this.searchResults.hits[1].id).toEqual('3');
      expect(this.searchResults.hits[2].id).toEqual('4');
    });
  });

  it('should index duplicate test data into the index', function () {
    runs(function() {
      this.err = undefined;
      this.done = false;
      var that = this;
      sidel.add({'batchName': 'data2'}, data2, function(err) {
        that.err = err;
        that.done = true;
      });
    });
    waitsFor(function() {
      return this.done != false;
    }, 'err not to be empty (search err returned)', 1000)
    runs(function () {
      expect(this.err).toEqual(null);
    });
  });


  it('should return 3 docs, since the previously indexed doc is a duplicate', function () {
    runs(function () {
      this.searchResults = '';
      var that = this;
      sidel.search({
        'query': {
          '*': ['*']
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
//      console.log(this.searchResults)
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toEqual(3);
      expect(this.searchResults.hits[0].id).toEqual('1');
      expect(this.searchResults.hits[1].id).toEqual('3');
      expect(this.searchResults.hits[2].id).toEqual('4');
    });
  });


  it('should index duplicate test data into the index', function () {
    runs(function() {
      this.err = undefined;
      this.done = false;
      var that = this;
      sidel.add({'batchName': 'data3'}, data3, function(err) {
        that.err = err;
        that.done = true;
      });
    });
    waitsFor(function() {
      return this.done != false;
    }, 'err not to be empty (search err returned)', 1000)
    runs(function () {
      expect(this.err).toEqual(null);
    });
  });


  it('should be able to return all documents in corpus, since reindexing all documents should reinstate the deleted doc, yet not add any extra docs', function () {
    runs(function () {
      this.searchResults = '';
      var that = this;
      sidel.search({
        'query': {
          '*': ['*']
        }
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 1000)
    runs(function() {
//      console.log(this.searchResults)
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.hits.length).toEqual(4);
      expect(this.searchResults.hits[0].id).toEqual('2');
      expect(this.searchResults.hits[1].id).toEqual('3');
      expect(this.searchResults.hits[2].id).toEqual('4');
      expect(this.searchResults.hits[3].id).toEqual('1');
    });
  });

  it('should be able to create a snapshot', function () {    
    runs(function () {
      this.completed = false;
      this.error = false;
      var that = this;
      sidel.snapShot(function(rs) {
        rs.pipe(fs.createWriteStream('backup.gz'))
          .on('close', function() {
            that.completed = true;
          })
          .on('error', function() {
            that.error = true;
          });
      });
    });
    waitsFor(function() {
      return this.completed;
    }, 'waiting for search results', 3000)
    runs(function() {
      expect(this.completed).toEqual(true);
      expect(this.error).toEqual(false);
    });
  });


});
