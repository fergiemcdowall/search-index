import { packageVersion } from './version.js'

export const validateVersion = index => {
  const key = ['CREATED_WITH']
  return index.STORE.get(key).then(v => {
    const version = 'search-index@' + packageVersion
    // if no created timestamp then create one (this is a new index)
    if (typeof v === 'undefined') return index.STORE.put(key, version)
    // throw a rejection if versions do not match
    if (version !== v) {
      return new Error(
        'This index was created with ' + v + ', you are running ' + version
      )
    }
  })
}
