/* global it */
/* global describe */

const _ = require('lodash')
const should = require('should')

describe('Indexing World Bank: ', function () {

  var data = require('../../node_modules/world-bank-dataset/world-bank-projects.json')
  var si

  it('should initialize the search index', function (done) {
    require('../../')({
      indexPath: 'test/sandbox/si-world-bank',
      logLevel: 'error'
    }, function (err, thisSi) {
      if (err) false.should.eql(true)
      si = thisSi
      done()
    })
  })


  it('should index the data', function (done) {
    this.timeout(60000)
    var padInt = function (intIn) {
      return ('000000000000000' + intIn).slice(-15)
    }
    var processDoc = function (datum) {
      delete datum._id
      delete datum.projectdocs
      delete datum.theme1
      delete datum.majorsector_percent
      delete datum.mjsector_namecode
      delete datum.mjtheme_namecode
      delete datum.sector
      delete datum.sector_namecode
      delete datum.sector1
      delete datum.sector2
      delete datum.theme_namecode
      datum.totalamt = [padInt(datum.totalamt)]
      return datum
    }
    var opt = {}
    opt.batchName = 'world-bank-projects.json'
    opt.fieldOptions = [
      {fieldName: 'mjtheme', filter: true},
      {fieldName: 'totalamt', filter: true}
    ]
    si.add(_.map(data, processDoc), opt, function (err) {
      (err === null).should.be.exactly(true)
      si.close(function(err){
        done()
      })
    })
  })
})
