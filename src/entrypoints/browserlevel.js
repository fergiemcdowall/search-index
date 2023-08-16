import { Main } from '../main.js'
import { BrowserLevel } from 'browser-level'

export class SearchIndex {
  constructor (ops = {}) {
    return new Main({
      Level: BrowserLevel,
      ...ops
    })
  }
}
