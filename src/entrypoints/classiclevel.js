import { SearchIndex as _SearchIndex } from '../SearchIndex.js'
import { ClassicLevel } from 'classic-level'

export class SearchIndex {
  constructor (ops = {}) {
    return new _SearchIndex({
      Level: ClassicLevel,
      ...ops
    })
  }
}
