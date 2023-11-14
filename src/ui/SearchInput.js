import { Autocomplete } from './Autocomplete.js'

export class SearchInput {
  constructor(
    {
      elementId = 'searchInput',
      suggestions = {},
      el = document.getElementById(elementId)
      // autoCompleteFunction = new Promise()
    },
    search,
    paging
  ) {
    el.addEventListener('input', e => {
      paging.page.NUMBER = 0 // always go back to page 1 on a new search
      search('searchInput')
    })

    this.autocomplete = new Autocomplete(el, search, suggestions)

    this.el = el
  }
}
