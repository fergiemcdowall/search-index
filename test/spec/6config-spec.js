var fs = require('fs');

describe('configuration', function () {
  it('should accept configuration', function () {
    runs(function() {
      si = require('../../')({ indexPath: 'si2' });
      expect(si).toBeDefined();
    });
  });

  it('should accept indexPath in configuration', function () {
    var si;

    runs(function() {
      si = require('../../')({ indexPath: 'si22' });
    });

    waitsFor(function () {
      return fs.existsSync('si2');
    }, 5000);
  });

//add in some bunyanny tests here


/*
  it('should accept logSilent in configuration', function () {
    var si;

    runs(function () {
      si = require('../../')({ logSilent: true });
    });
    
    waitsFor(function() {
      return si.searchIndexLogger.transports.console.silent;
    }, 5000);
  });
*/
});
