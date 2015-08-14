/* global it */
/* global describe */

var logLevel = 'error';
if (process.env.NODE_ENV == 'TEST') logLevel = 'info';
var should = require('should');
var sandboxPath = 'test/sandbox';

describe('Matching epub: ', function () {

  it('should index test data into the index', function (done) {
    var data = [
      {
        "title": "Accessible EPUB 3",
        "body": "EPUB  is great.",
        "spineItemPath": "../node_modules/epub3-samples/accessible_epub_3/EPUB/index1.xhtml"
      },
      {
        "title": "Even More Accessible EPUB 3",
        "body": "EPUB is epubtastic",
        "spineItemPath": "../node_modules/epub3-samples/accessible_epub_3/EPUB/index2.xhtml"
      },
      {
        "title": "EPUB 3 FTW",
        "body": "EPUB is fantabulous",
        "spineItemPath": "../node_modules/epub3-samples/accessible_epub_3/EPUB/index3.xhtml"
      }
    ];
    var si = require('../../')({indexPath: sandboxPath + '/si-epub-matching-test',
                                logLevel: logLevel});
    si.add({batchName: 'epubdata'}, data, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);
        done();
      });
    });
  })


  it('should search on all fields and get results', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-epub-matching-test',
                                logLevel: logLevel});
    var str = 'epub';
    si.match(str, function (err, matches) {
      should.exist(matches);
      (err === null).should.be.exactly(true);
      matches.length.should.be.exactly(2);
      matches[0].should.be.exactly('epub');
      matches[1].should.be.exactly('epubtastic');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  })
  
  it('handles match strings that are below threshold', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-epub-matching-test',
                                logLevel: logLevel});
    var str = 'lo';
    si.match(str, function (err, matches) {
      should.exist(matches);
      matches.length.should.be.exactly(0);
      (err instanceof Error).should.be.exactly(true);
      err.toString().should.be.exactly('Error: string below threshold length (3)');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });
  
});
