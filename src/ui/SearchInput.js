import { Autocomplete } from './Autocomplete.js'

export class SearchInput {
  constructor(
    {
      elementId = 'searchInput',
      suggestionsElementId = 'suggestions',
      el = document.getElementById(elementId),
      autoCompleteFunction = new Promise()
    },
    search,
    paging
  ) {
    el.addEventListener('input', e => {
      paging.page.NUMBER = 0 // always go back to page 1 on a new search
      search('searchInput')
    })

    new Autocomplete(el, autoCompleteFunction, search, suggestionsElementId)

    this.el = el
  }
}
