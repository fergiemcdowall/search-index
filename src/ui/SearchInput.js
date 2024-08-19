import { Autocomplete } from './Autocomplete.js'

/**
 * The search input box
 * @typedef { Object } SearchInputOptions
 * @memberof UI
 * @property { string } [ elementId="searchInput" ] - The id of the HTML element that contains the search input box
 * @property { UI.SuggestionsOptions } [ suggestions={} ] - options for suggestions
 */
export class SearchInput {
  constructor ({ elementId = 'searchInput', suggestions = {} }, search, paging) {
    this.el = document.getElementById(elementId)
    this.el.addEventListener('input', e => {
      paging.page.NUMBER = 0 // always go back to page 1 on a new search
      search('searchInput')
    })
    this.autocomplete = new Autocomplete(this.el, search, suggestions)
  }
}
