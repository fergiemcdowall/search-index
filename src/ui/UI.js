import { Count } from './Count.js'
import { Hits } from './Hits.js'
import { Paging } from './Paging.js'
import { Facet } from './Facet.js'
import { SearchInput } from './SearchInput.js'

export class UI {
  constructor ({
    index = null,
    count = {},
    hits = {},
    facets = [],
    searchInput = {},
    paging = {}
  }) {
    this.index = index
    this.count = new Count(count)
    this.paging = new Paging(paging, this.search)
    this.hits = new Hits(hits)
    this.searchInput = new SearchInput(
      {
        autoCompleteFunction: this.index.DICTIONARY,
        ...searchInput
      },
      this.search,
      this.paging
    )
    this.facets = facets.map(r => new Facet(r, this.search))
    this.search()
  }

  queryOptions = () => ({
    // TODO: this should be somehow controlled by the Facet class so that its turned off when ORing
    FACETS: this.facets.map(r => ({
      FIELD: r.field
    })),
    DOCUMENTS: true,
    PAGE: this.paging.page
  })

  // treat empty search as a special case: instead of showing nothing,
  // show everything.
  emptySearchQuery = () => [
    {
      ALL_DOCUMENTS: true
    },
    this.queryOptions()
  ]

  searchQuery = () => {
    return [
      [
        ...this.searchInput.el.value.split(/\s+/).filter(item => item),
        ...this.facets.map(r => r.getActiveFilters()).flat(Infinity)
      ],
      this.queryOptions()
    ]
  }

  search = source => {
    return (
      this.searchInput.el.value.length +
      this.facets.map(r => r.getActiveFilters()).flat(Infinity).length
        ? this.index.SEARCH(...this.searchQuery())
        : this.index.QUERY(...this.emptySearchQuery())
    ).then(res => {
      this.hits.update(res.RESULT)
      this.count.update(res.RESULT_LENGTH)
      this.facets.forEach(r => r.update(res.FACETS, source))
      this.paging.update(res.PAGING)
    })
  }
}
