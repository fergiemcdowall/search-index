import { SearchIndex as MainSearchIndex } from '../main.js'
import { ClassicLevel } from 'classic-level'

export class SearchIndex {
  constructor (ops = {}) {
    return new MainSearchIndex({
      db: new ClassicLevel(ops.name || 'fii', {
        valueEncoding: 'json'
      }),
      ...ops
    })
  }
}
