import { Main } from '../main.js'
import { ClassicLevel } from 'classic-level'

export class SearchIndex {
  constructor (ops = {}) {
    return new Main({
      Level: ClassicLevel,
      ...ops
    })
  }
}
