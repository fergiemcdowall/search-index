var fs = require('fs');
var si = require('../../')({indexPath: 'si-world-bank', logSilent: false});


describe('indexing and search', function () {


/*
  it('should be able to create a snapshot', function () {    
    runs(function () {
      this.completed = false;
      this.error = false;
      var that = this;
      si.snapShot(function(rs) {
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
    }, 'waiting for search results', 60000)
    runs(function() {
      expect(this.completed).toEqual(true);
      expect(this.error).toEqual(false);
    });
  });
*/


  it('should be able to search for more than 1 word and show facetranges', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        "query": {
          "*": [
            "africa", "bank"
          ]
        },
        "facets": {
          "totalamt": {
            "ranges":[
              [
                "000000000000000",
                "000000050000000"
              ],
              [
                "000000050000001",
                "100000000000000"
              ]
            ]
          },
          "mjtheme": {
            "ranges": [
              [
                "A",
                "J"
              ],
              [
                "K",
                "Z"
              ]
            ]
          }
        },
        "offset": 0,
        "pageSize": 100
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
//      console.log(JSON.stringify(this.searchResults, null, 2));
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.totalHits).toEqual(12);
      expect(this.searchResults.facets[0].value[0].key).toEqual("000000000000000-000000050000000");
      expect(this.searchResults.facets[0].value[0].value).toEqual(10);
      expect(this.searchResults.facets[0].value[1].key).toEqual("000000050000001-100000000000000");
      expect(this.searchResults.facets[0].value[1].value).toEqual(2);
      expect(this.searchResults.facets[1].value[0].key).toEqual("K-Z");
      expect(this.searchResults.facets[1].value[0].value).toEqual(9);      
      expect(this.searchResults.facets[1].value[1].key).toEqual("A-J");
      expect(this.searchResults.facets[1].value[1].value).toEqual(8);
    });
  });


  it('should be able to filter on a chosen facetrange', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        "query": {
          "*": [
            "africa", "bank"
          ]
        },
        "filter": {
          "totalamt": [
            [
              "000000000000000",
              "000000050000000"
            ]
          ]
        },
        "facets": {
          "totalamt": {
            "ranges":[
              [
                "000000000000000",
                "000000050000000"
              ],
              [
                "000000050000001",
                "100000000000000"
              ]
            ]
          },
          "mjtheme": {
            "ranges": [
              [
                "A",
                "J"
              ],
              [
                "K",
                "Z"
              ]
            ]
          }
        },
        "offset": 0,
        "pageSize": 100
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.totalHits).toEqual(10);
      expect(this.searchResults.facets[0].value[0].key).toEqual("000000000000000-000000050000000");
      expect(this.searchResults.facets[0].value[0].value).toEqual(10);
      expect(this.searchResults.facets[0].value[1].key).toEqual("000000050000001-100000000000000");
      expect(this.searchResults.facets[0].value[1].value).toEqual(0);
      expect(this.searchResults.facets[1].value[0].key).toEqual("K-Z");
      expect(this.searchResults.facets[1].value[0].value).toEqual(8);      
      expect(this.searchResults.facets[1].value[1].key).toEqual("A-J");
      expect(this.searchResults.facets[1].value[1].value).toEqual(6);
    });
  });


});

