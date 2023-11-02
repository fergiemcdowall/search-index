import { autocomplete } from './autocomplete.js'

// TODO: should be possible to customise templates
export class SearchInput {
  constructor(
    {
      elementId = 'searchInput',
      el = document.getElementById(elementId),
      autoCompleteFunction = new Promise()
    },
    search,
    paging
  ) {
    el.addEventListener('input', e => {
      paging.page.NUMBER = 0 // always go back to page 1 on a new search
      search(this.value)
    })

    autocomplete(el, autoCompleteFunction, search)

    this.el = el
  }
}
