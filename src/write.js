import util from './util.js'
import trav from 'traverse'
import tv from 'term-vector'

export default function (fii) {
  const invertDoc = function (obj) {
    const invertedDoc = {}
    // take a plain old JSON object and parse out all of the leaf-nodes
    trav(obj).forEach(function (node) {
      if (typeof node === 'undefined') return
      const self = this
      // by default all fields are searchable
      let searchable = true
      this.path.forEach(item => {
        // make these fields unsearchable (exclude from inverted index)
        if (item === '_id') searchable = false
        if (item.substring(0, 1) === '!') searchable = false
      })
      if (searchable && this.isLeaf) {
        let fieldName = self.path.filter(item => {  // eslint-disable-line
          return isNaN(item)
        }).join('.')
        // create inverted field as empty array, or existing array
        invertedDoc[fieldName] = invertedDoc[fieldName] || []
        // push this leaf value to array
        invertedDoc[fieldName].push((self.node + '').split(' '))
        // Since this code generates a nested array, flatten
        // (means that we can index both arrays and strings with the same code)
        invertedDoc[fieldName].flat()
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
        const val = invertedDoc[key][0]
        newInvertedDoc[key] = {}
        newInvertedDoc[key][val] = val
        return newInvertedDoc
      }
      // for all other fields generate term frequency
      newInvertedDoc[key] = tv(invertedDoc[key]).reduce((acc, cur, _, arr) => {
        // TODO: make scoring precision an option
        acc[cur.term] = (cur.positions.length / arr.length).toFixed(2)
        return acc
      }, {})
      return newInvertedDoc
    }, {})

  const addSearchableFields = iDocs => new Promise((resolve) => {
    const fields = new Set([].concat.apply([], iDocs.map(Object.keys)))
    fields.delete('_id') // id not searchable
    fields.delete('!doc') // !doc not searchable
    fii.STORE.batch(Array.from(fields).map(f => {
      return {
        type: 'put',
        key: '￮FIELD￮' + f + '￮',
        value: true
      }
    }), err => {
      if (err) console.log(err)
      util(fii).calibrate().then(resolve)
    })
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
