class DocumentProcessor {
  constructor(ops) {
    this.ops = ops
  }

  isArray = item => Array.isArray(item)

  isObject = item =>
    typeof item === 'object' && item !== null && !Array.isArray(item)

  isString = item => typeof item === 'string' || item instanceof String

  processValueArray = arr => Promise.all(arr.map(this.processValueUnknownType))

  processValueObject = obj =>
    new Promise(async resolve => {
      const acc = {}
      for (const key in obj) {
        acc[key] = await this.processValueUnknownType(obj[key], key, this.ops)
      }
      return resolve(acc)
    })

  processValueUnknownType = (unknown, key) => {
    return new Promise(async resolve => {
      if (Number.isInteger(unknown))
        return resolve(JSON.stringify([unknown, unknown]))
      if (this.isString(unknown))
        return resolve(this.ops.tokenizer(unknown, key, this.ops))
      if (this.isObject(unknown))
        return resolve(this.processValueObject(unknown))
      if (this.isArray(unknown)) return resolve(this.processValueArray(unknown))
      return resolve(unknown)
    })
  }

  processDocument = async doc =>
    new Promise(async resolve => {
      // Documents that are Strings are converted into { body: ... }
      if (this.isString(doc)) doc = { body: doc }

      const acc = {}
      for (const key in doc) {
        if (key === '_id') {
          acc[key] = doc[key]
          continue
        }
        acc[key] = await this.processValueUnknownType(doc[key], key)
      }
      return resolve(acc)
    })

  processDocuments = docs => Promise.all(docs.map(this.processDocument))
  // .then(docs => {
  //   console.log(docs)
  //   return docs
  // })
}

module.exports = DocumentProcessor
