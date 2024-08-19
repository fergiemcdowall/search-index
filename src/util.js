import { packageVersion } from './version.js'

export const validateVersion = index => {
  return new Promise((resolve, reject) => {
    const key = ['CREATED_WITH']
    const version = 'search-index@' + packageVersion
    return index.STORE.get(key)
      .then(v =>
        // throw a rejection if versions do not match
        version === v
          ? resolve()
          : reject(
            new Error(
              'This index was created with ' +
                  v +
                  ', you are running ' +
                  version
            )
          )
      )
      .catch(e => index.STORE.put(key, version).then(resolve))
  })
}
