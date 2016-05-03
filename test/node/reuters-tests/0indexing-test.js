/* global it */
/* global describe */

const assert = require('assert')
const fs = require('fs')
const searchindex = require('../../../')
const should = require('should')

describe('Indexing Reuters reuters-000.json: ', function () {
  const sandboxPath = 'test/sandbox'
  var data = []
  var si

  it('should initialize the search index', function (done) {
    searchindex({
      indexPath: sandboxPath + '/si-reuters',
      logLevel: 'error'
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si = thisSi
      done()
    })
  })

  it('should find the data and set up a sandbox', function (done) {
    data = require('../../../node_modules/reuters-21578-json/data/full/reuters-000.json')
    assert.equal(data.length, 1000)
    assert.equal(data[0].id, '1')
    try {
      var stats = fs.lstatSync(sandboxPath)
      assert(stats.isDirectory())
    } catch (e) {
      console.log(e)
      assert(false)
    }
    done()
  })

  it('should index the data', function (done) {
    this.timeout(120000)
    var opt = {}
    opt.batchName = 'reuters'
    opt.fieldOptions = [
      {fieldName: 'places', filter: true},
      {fieldName: 'topics', filter: true}
    ]
    si.add(data, opt, function (err) {
      (err === null).should.be.exactly(true)
      done()
    })
  })

  it('should verify indexing', function (done) {
    si.tellMeAboutMySearchIndex(function (err, info) {
      (err === null).should.be.exactly(true)
      should.exist(info)
      ;(info.totalDocs).should.be.exactly(1000)
      info.indexPath.should.be.exactly('test/sandbox/si-reuters')
      info.logLevel.should.be.exactly('error')
      info.deletable.should.be.exactly(true)
      info.fieldedSearch.should.be.exactly(true)
      info.nGramLength.should.be.exactly(1)
      info.stopwords.should.eql(
        [ '$',
          '0',
          '1',
          '2',
          '3',
          '4',
          '5',
          '6',
          '7',
          '8',
          '9',
          '_',
          'a',
          'about',
          'after',
          'all',
          'also',
          'am',
          'an',
          'and',
          'another',
          'any',
          'are',
          'as',
          'at',
          'b',
          'be',
          'because',
          'been',
          'before',
          'being',
          'between',
          'both',
          'but',
          'by',
          'c',
          'came',
          'can',
          'come',
          'could',
          'd',
          'did',
          'do',
          'e',
          'each',
          'f',
          'for',
          'from',
          'g',
          'get',
          'got',
          'h',
          'had',
          'has',
          'have',
          'he',
          'her',
          'here',
          'him',
          'himself',
          'his',
          'how',
          'i',
          'if',
          'in',
          'into',
          'is',
          'it',
          'j',
          'k',
          'l',
          'like',
          'm',
          'make',
          'many',
          'me',
          'might',
          'more',
          'most',
          'much',
          'must',
          'my',
          'n',
          'never',
          'now',
          'o',
          'of',
          'on',
          'only',
          'or',
          'other',
          'our',
          'out',
          'over',
          'p',
          'q',
          'r',
          's',
          'said',
          'same',
          'see',
          'should',
          'since',
          'some',
          'still',
          'such',
          't',
          'take',
          'than',
          'that',
          'the',
          'their',
          'them',
          'then',
          'there',
          'these',
          'they',
          'this',
          'those',
          'through',
          'to',
          'too',
          'u',
          'under',
          'up',
          'v',
          'very',
          'w',
          'was',
          'way',
          'we',
          'well',
          'were',
          'what',
          'where',
          'which',
          'while',
          'who',
          'with',
          'would',
          'x',
          'y',
          'you',
          'your',
          'z' ])
      done()
    })
  })

  it('should be able to return all documents in index', function (done) {
    var q = {}
    q.query = {
      AND: {'*': ['*']}
    }
    si.search(q, function (err, searchResults) {
      should.exist(searchResults)
      ;(err === null).should.be.exactly(true)
      searchResults.hits.length.should.be.exactly(100)
      searchResults.totalHits.should.be.exactly(1000)
      si.close(function(err){
        done()
      })
    })
  })


})
