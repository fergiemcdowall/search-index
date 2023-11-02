import { Main } from '../main.js'
import { BrowserLevel } from 'browser-level'
import { UI as UIX } from '../ui/UI.js'

export class SearchIndex {
  constructor (ops = {}) {
    return new Main({
      Level: BrowserLevel,
      ...ops
    })
  }
}

export class UI {
  constructor (ops = {}) {
    return new UIX(ops)
  }
}
