import { SearchIndex as _SearchIndex } from '../SearchIndex.js'
import { BrowserLevel } from 'browser-level'
import { UI as _UI } from '../ui/UI.js'

export class SearchIndex {
  constructor (ops = {}) {
    return new _SearchIndex({
      Level: BrowserLevel,
      ...ops
    })
  }
}

export class UI {
  constructor (ops = {}) {
    return new _UI(ops)
  }
}
