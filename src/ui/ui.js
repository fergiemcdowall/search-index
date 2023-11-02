import { Count } from './count.js'
import { Hits } from './hits.js'
import { Paging } from './paging.js'
import { Refiner } from './Refiner.js'
import { SearchInput } from './SearchInput.js'

export class UI {
  constructor({
    index = null,
    count = {},
    hits = {},
    refiners = [],
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
    this.refiners = refiners.map(r => new Refiner(r, this.search))
    this.search()
  }

  // treat empty search as a special case: instead of showing nothing,
  // show everything.
  emptySearchQuery = () => [
    {
      ALL_DOCUMENTS: true
    },
    {
      FACETS: [
        {
          FIELD: ['month', 'year']
        }
      ],
      PAGE: this.paging.page
    }
  ]

  searchQuery = () => {
    console.log('in searchQuery')
    const q = [
      [
        ...this.searchInput.el.value.split(/\s+/).filter(item => item),
        ...this.refiners.map(r => r.activeFilters).flat(Infinity)
      ],
      {
        FACETS: [
          {
            FIELD: ['month']
          },
          {
            FIELD: ['year']
          }
        ],
        DOCUMENTS: true,
        PAGE: this.paging.page
      }
    ]
    return q
  }

  search = () =>
    (this.searchInput.el.value.length +
    this.refiners.map(r => r.activeFilters).flat(Infinity).length
      ? this.index.SEARCH(...this.searchQuery())
      : this.index.QUERY(...this.emptySearchQuery())
    ).then(this.displaySearch)

  displaySearch = res => {
    this.hits.update(res.RESULT)
    this.count.update(res.RESULT_LENGTH)
    this.refiners.forEach(r => r.render(res.FACETS))
    this.paging.update(res.PAGING)
  }
}
