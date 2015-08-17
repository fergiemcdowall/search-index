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
        "id": "doc101",
        "title": "Accessible EPUB 3",
        "body": "EPUB  is great.",
        "spineItemPath": "epub_content/accessible_epub_3/EPUB/ch03s06.xhtml"
      },
      {
        "id": "doc102",
        "title": "Even More Accessible EPUB 3",
        "body": "EPUB is epubtastic",
        "spineItemPath": "epub_content/accessible_epub_3/EPUB/ch03s07.xhtml"
      },
      {
        "id": "doc103",
        "title": "EPUB 3 FTW",
        "body": "EPUB is fantabulous",
        "spineItemPath": "epub_content/accessible_epub_3/EPUB/ch03s08.xhtml"
      },
      {
        "id": "doc104",
        "title": "中文的标题",
        "body": "中文的字符",
        "spineItemPath": "epub_content/accessible_epub_3/EPUB/ch03s09.xhtml"
      }
    ];
    var si = require('../../')({indexPath: sandboxPath + '/si-epub-matching-test',
                                logLevel: logLevel});
    si.add({
      batchName: 'epubdata',
      fieldOptions: [{
        fieldName: 'id',
        searchable: false
      }, {
        fieldName: 'spineItemPath',
        searchable: false
      }]
    }, data, function (err) {
      (err === null).should.be.exactly(true);
      si.close(function (err) {
        if (err) false.should.eql(true);
        done();
      });
    });
  });

  it('should search on all fields and get results', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-epub-matching-test',
                                logLevel: logLevel});
    var str = 'epub';
    si.match(str, function (err, matches) {
      console.log(matches);
      should.exist(matches);
      (err === null).should.be.exactly(true);
      matches.length.should.be.exactly(2);
      matches[0].should.be.exactly('epub');
      matches[1].should.be.exactly('epubtastic');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('should work for Unicode', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-epub-matching-test',
      logLevel: logLevel});
    var str = '中文的';
    si.match(str, function (err, matches) {
      console.log(matches);
      should.exist(matches);
      (err === null).should.be.exactly(true);
      matches.length.should.be.exactly(2);
      matches.should.containEql('中文的标题');
      matches.should.containEql('中文的字符');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });

  it('handles match strings that are empty', function (done) {
    var si = require('../../')({indexPath: sandboxPath + '/si-epub-matching-test',
                                logLevel: logLevel});
    var str = '';
    si.match(str, function (err, matches) {
      should.exist(matches);
      matches.length.should.be.exactly(0);
      (err instanceof Error).should.be.exactly(true);
      err.toString().should.be.exactly('Error: match string can not be empty');
      si.close(function (err) {
        if (err) false.should.eql(true);done();
      });
    });
  });
  
});
