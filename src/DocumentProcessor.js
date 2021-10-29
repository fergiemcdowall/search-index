module.exports = ops => {
  const isObject = item =>
    typeof item === 'object' && item !== null && !Array.isArray(item)

  const isString = item => typeof item === 'string' || item instanceof String

  const isArray = item => Array.isArray(item)

  const processValueArray = arr => Promise.all(arr.map(processValueUnknownType))

  const processValueObject = obj =>
    // eslint-disable-next-line
    new Promise(async resolve => {
      const acc = {}
      for (const key in obj) {
        acc[key] = await processValueUnknownType(obj[key], key, ops)
      }
      return resolve(acc)
    })

  const processValueUnknownType = (unknown, key) =>
    // eslint-disable-next-line
    new Promise(async resolve => {
      if (Number.isInteger(unknown)) {
        return resolve(JSON.stringify([unknown, unknown]))
      }
      if (isString(unknown)) return resolve(ops.tokenizer(unknown, key, ops))
      if (isObject(unknown)) return resolve(processValueObject(unknown))
      if (isArray(unknown)) return resolve(processValueArray(unknown))
      if (unknown === null) return resolve(JSON.stringify([null, '1.00']))
      return resolve(unknown)
    })

  const processDocument = async doc =>
    // eslint-disable-next-line
    new Promise(async resolve => {
      // Documents that are Strings are converted into { body: ... }
      if (isString(doc)) doc = { body: doc }

      // Docs with no _id are auto-assigned an ID
      // if (!doc.hasOwnProperty('_id')) doc._id = ops.idGenerator.next().value
      if (!Object.prototype.hasOwnProperty.call(doc, '_id')) { doc._id = ops.idGenerator.next().value }

      const acc = {}
      for (const key in doc) {
        if (key === '_id') {
          acc[key] = doc[key]
          continue
        }
        acc[key] = await processValueUnknownType(doc[key], key)
      }
      return resolve(acc)
    })

  return {
    processDocuments: docs => Promise.all(docs.map(processDocument))
  }
}
