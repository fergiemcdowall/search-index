describe('faceting', function () {

  var fs = require('fs');
  var si = require('../../')({indexPath:'si-world-bank'});


  it('should be able to search in indexed data with faceting', function () {    
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
          "totalamt": [
            [
              "000000000000000",
              "000000006000000"
            ],
            [
              "000000006000001",
              "000000012000000"
            ]
          ],
          "mjtheme": [
            [
              "A",
              "J"
            ],
            [
              "K",
              "Z"
            ]
          ]
        },
        "offset": 0,
        "pageSize": 10,
        "facets": [
          "totalamt",
          "mjtheme"
        ],
        "facetSort": "keyAsc",
        "facetLength": 10}, function(err, searchResults) {
          that.searchResults = searchResults;
        });
    });
    waitsFor(function() {
      return this.searchResults != '';
    }, 'waiting for search results', 5000)
    runs(function() {
      expect(this.searchResults).toBeDefined();
      expect(this.searchResults.facets.totalamt[0].key).toEqual("000000000000000");
      expect(this.searchResults.facets.totalamt[0].value).toEqual(76);
      expect(this.searchResults.facets.mjtheme[0].key).toEqual("Economic management");
      expect(this.searchResults.facets.mjtheme[0].value).toEqual(13);
      expect(this.searchResults.facetRanges.totalamt[0].key).toEqual("000000000000000-000000006000000");
      expect(this.searchResults.facetRanges.totalamt[0].value).toEqual(85);
      expect(this.searchResults.facetRanges.totalamt[1].key).toEqual("000000006000001-000000012000000");
      expect(this.searchResults.facetRanges.totalamt[1].value).toEqual(12);
      expect(this.searchResults.facetRanges.mjtheme[0].key).toEqual("A-J");
      expect(this.searchResults.facetRanges.mjtheme[0].value).toEqual(135);
      expect(this.searchResults.facetRanges.mjtheme[1].key).toEqual("K-Z");
      expect(this.searchResults.facetRanges.mjtheme[1].value).toEqual(161);      
    });
  });

})
