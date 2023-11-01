import { Main } from '../main.js'
import { BrowserLevel } from 'browser-level'
import { ui as uix } from '../ui/ui.js'

export class SearchIndex {
  constructor(ops = {}) {
    return new Main({
      Level: BrowserLevel,
      ...ops
    })
  }
}

export const ui = ops => uix(ops)
