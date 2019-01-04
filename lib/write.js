const util = require('./util.js')
const trav = require('traverse')
const tv = require('term-vector')

module.exports = fii => {
  const invertDoc = function (obj) {
    var invertedDoc = {}
    // take a plain old JSON object and parse out all of the leaf-nodes
    trav(obj).forEach(function (node) {
      if (typeof node === 'undefined') return
      var that = this
      var searchable = true
      this.path.forEach(item => {
        // denotes that a field is indexable
        if (item === '_id') searchable = false
        if (item.substring(0, 1) === '!') searchable = false
      })
      if (searchable && this.isLeaf) {
        invertedDoc[that.path.filter(item => {  // eslint-disable-line
          return isNaN(item)
        }).join('.')] = (that.node + '').split(' ')
      }
    })
    return invertedDoc
  }

  // calculate term frequency on flattened object
  const calculateTermFrequency = invertedDoc => Object.keys(invertedDoc).reduce(
    (newInvertedDoc, key) => {
      // for fields that are length 1 with a number return number
      // (trying to find a sensible way to deal with numbers)
      if ((invertedDoc[key].length === 1) && (!isNaN(invertedDoc[key][0]))) {
        // invertedDoc[key] = {'_SELF': invertedDoc[key][0]}
        const val = invertedDoc[key][0]
        newInvertedDoc[key] = {}
        newInvertedDoc[key][val] = val
        return newInvertedDoc
      }
      // for all other fields generate term frequency
      newInvertedDoc[key] = tv(invertedDoc[key]).reduce((acc, cur, i, arr) => {
        // TODO: make scoring precision an option
        acc[cur.term] = (cur.positions.length / arr.length).toFixed(2)
        return acc
      }, {})
      return newInvertedDoc
    }, {})

  const addSearchableFields = (iDocs) => new Promise((resolve, reject) => {
    const fields = new Set([].concat.apply([], iDocs.map(Object.keys)))
    fields.delete('_id') // id not searchable
    fields.delete('!doc') // !doc not searchable
    var batch = fii.STORE.batch()
    Array.from(fields).map(f => {
      batch.put('￮FIELD￮' + f + '￮', true)
    })
    batch.write()
      .then(util(fii).calibrate)
      .then(resolve)
  })

  const PUT = docs => fii.PUT(docs
    .map(invertDoc)
    .map(calculateTermFrequency)
    .map(
      (doc, i) => {
        doc._id = docs[i]._id
        doc['!doc'] = docs[i]
        return doc
      }
    )).then(addSearchableFields)

  return {
    DELETE: (..._ids) => fii.DELETE(..._ids),
    PUT: PUT
  }
}
