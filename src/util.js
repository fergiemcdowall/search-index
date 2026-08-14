import { packageVersion } from './version.js'

// the minor version is compared: patch releases do not change index topography,
// so an index created with 6.0.0 can be opened by 6.0.1. Minor versions might
// change index topography and so they do not validate
const minorVersion = version =>
  String(version).split('@').pop().split('.').slice(0, 2).join('.')

export const validateVersion = index => {
  const key = ['CREATED_WITH']
  const version = 'search-index@' + packageVersion
  return (
    index.STORE.get(key)
      // not every abstract-level backend resolves undefined for a missing key
      .catch(() => undefined)
      .then(v => {
        // if no version stamp then create one (this is a new index)
        if (v === undefined || v === null) return index.STORE.put(key, version)
        // reject if versions are not compatible
        if (minorVersion(v) !== minorVersion(version)) {
          throw new Error(
            'This index was created with ' + v + ', you are running ' + version
          )
        }
      })
  )
}
