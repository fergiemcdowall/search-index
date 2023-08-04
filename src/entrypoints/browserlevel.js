import { SearchIndex as MainSearchIndex } from '../main.js'
import { BrowserLevel } from 'browser-level'

export class SearchIndex {
  constructor (ops = {}) {
    return new MainSearchIndex({
      db: new BrowserLevel(ops.name || 'fii', {
        valueEncoding: 'json'
      }),
      ...ops
    })
  }
}
