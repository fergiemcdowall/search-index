describe('faceting', function () {

  var fs = require('fs');
  var si = require('../../')({indexPath:'si-world-bank'});
  it('should be able to search and do facet ranges', function () {    
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        "query": {
          "*": [
            "africa"
          ]
        },
        "facetRanges": {
          "totalamt": {
            "ranges": [
              [
                "000000000000000",
                "000000006000000"
              ],
              [
                "000000006000001",
                "010000000000000"
              ]
            ]},
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
            ]}
        },
        "offset": 0,
        "pageSize": 10
      }, function(err, searchResults) {
        that.searchResults = searchResults;
      });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facetRanges[0].value[0].key).toEqual("000000006000001-010000000000000");
      expect(this.searchResults.facetRanges[0].value[0].value).toEqual(121);
      expect(this.searchResults.facetRanges[0].value[1].key).toEqual("000000000000000-000000006000000");
      expect(this.searchResults.facetRanges[0].value[1].value).toEqual(85);
      expect(this.searchResults.facetRanges[1].value[0].key).toEqual("K-Z");
      expect(this.searchResults.facetRanges[1].value[0].value).toEqual(161);      
      expect(this.searchResults.facetRanges[1].value[1].key).toEqual("A-J");
      expect(this.searchResults.facetRanges[1].value[1].value).toEqual(135);
    });
  });



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
        "facetRanges": {
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
            ]},
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
              ]}
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
      expect(this.searchResults.totalHits).toEqual(12);
      expect(this.searchResults.facetRanges[0].value[0].key).toEqual("000000000000000-000000050000000");
      expect(this.searchResults.facetRanges[0].value[0].value).toEqual(10);
      expect(this.searchResults.facetRanges[0].value[1].key).toEqual("000000050000001-100000000000000");
      expect(this.searchResults.facetRanges[0].value[1].value).toEqual(2);
      expect(this.searchResults.facetRanges[1].value[0].key).toEqual("K-Z");
      expect(this.searchResults.facetRanges[1].value[0].value).toEqual(9);      
      expect(this.searchResults.facetRanges[1].value[1].key).toEqual("A-J");
      expect(this.searchResults.facetRanges[1].value[1].value).toEqual(8);
    });
  });



  it('should be able to search for more than 1 word and no ranges (experiment)', function () { 
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        "query": {
          "*": [
            "africa", "bank"
          ]
        },
        "facetRanges": {
          "totalamt": {},
          "mjtheme": {}
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
      expect(this.searchResults.totalHits).toEqual(12);
      expect(this.searchResults.facetRanges[0].value.length).toEqual(4);
      expect(this.searchResults.facetRanges[0].value[0].value).toEqual(9);
      expect(this.searchResults.facetRanges[0].value[1].value).toEqual(1);
      expect(this.searchResults.facetRanges[0].value[2].value).toEqual(1);
      expect(this.searchResults.facetRanges[0].value[3].value).toEqual(1);
      expect(this.searchResults.facetRanges[1].value.length).toEqual(8);
      expect(this.searchResults.facetRanges[1].value[0].value).toEqual(5);
      expect(this.searchResults.facetRanges[1].value[1].value).toEqual(4);
    });
  });

  it('should be able to search for more than 1 word and no ranges (experiment)', function () { 
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        "query": {
          "*": [
            "africa", "bank"
          ]
        },
        "facetRanges": {
          "totalamt": {},
          "mjtheme": {}
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
      expect(this.searchResults.totalHits).toEqual(12);
      expect(this.searchResults.facetRanges[0].value.length).toEqual(4);
      expect(this.searchResults.facetRanges[0].value[0].value).toEqual(9);
      expect(this.searchResults.facetRanges[0].value[1].value).toEqual(1);
      expect(this.searchResults.facetRanges[0].value[2].value).toEqual(1);
      expect(this.searchResults.facetRanges[0].value[3].value).toEqual(1);
      expect(this.searchResults.facetRanges[1].value.length).toEqual(8);
      expect(this.searchResults.facetRanges[1].value[0].value).toEqual(5);
      expect(this.searchResults.facetRanges[1].value[1].value).toEqual(4);
    });
  });


  it('should be able to limit facet length', function () { 
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        "query": {
          "*": [
            "africa", "bank"
          ]
        },
        "facetRanges": {
          "totalamt": {"sort":"valueAsc"},
          "mjtheme": {"sort":"valueDesc", "limit": 3}
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
      expect(this.searchResults.totalHits).toEqual(12);
      expect(this.searchResults.facetRanges[0].value.length).toEqual(4);
      expect(this.searchResults.facetRanges[1].value.length).toEqual(3);
    });
  });


  it('should be able to search for more than 1 word with a mix of ranged and unranged facets', function () { 
    runs(function () {
      this.searchResults = '';
      var that = this;
      si.search({
        "query": {
          "*": [
            "africa", "bank"
          ]
        },
        "facetRanges": {
          "totalamt": {"sort":"keyDesc"},
          "mjtheme": {
            "sort": "keyAsc",
            "ranges": [
              [
                "A",
                "J"
              ],
              [
                "K",
                "Z"
              ]
            ]}
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
      expect(this.searchResults.totalHits).toEqual(12);
      expect(this.searchResults.facetRanges[0].value.length).toEqual(4);
      expect(this.searchResults.facetRanges[0].value[0].key).toEqual('000000300000000');
      expect(this.searchResults.facetRanges[0].value[0].value).toEqual(1);
      expect(this.searchResults.facetRanges[0].value[1].key).toEqual('000000070000000');
      expect(this.searchResults.facetRanges[0].value[1].value).toEqual(1);
      expect(this.searchResults.facetRanges[0].value[2].key).toEqual('000000020000000');
      expect(this.searchResults.facetRanges[0].value[2].value).toEqual(1);
      expect(this.searchResults.facetRanges[0].value[3].key).toEqual('000000000000000');
      expect(this.searchResults.facetRanges[0].value[3].value).toEqual(9);
      expect(this.searchResults.facetRanges[1].value.length).toEqual(2);
      expect(this.searchResults.facetRanges[1].value[0].key).toEqual('A-J');
      expect(this.searchResults.facetRanges[1].value[0].value).toEqual(8);
      expect(this.searchResults.facetRanges[1].value[1].key).toEqual('K-Z');
      expect(this.searchResults.facetRanges[1].value[1].value).toEqual(9);
    });
  });


})
