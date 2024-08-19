import { Count } from './Count.js'
import { Hits } from './Hits.js'
import { Paging } from './Paging.js'
import { Facet } from './Facet.js'
import { SearchInput } from './SearchInput.js'

/**
 * Options for writing documents to the index
 * @typedef { Object } Options
 * @memberof UI
 * @property { SearchIndex } [index] - the search index
 * @property { UI.CountOptions } [count] - initialise the <code>count</code> component
 * @property { Array.<UI.FacetOptions> } [facets] - initialise the <code>facet</code> components
 * @property { UI.HitsOptions } [hits] - initialise the <code>hits</code> component
 * @property { UI.PagingOptions } [paging] - initialise the paging component
 * @property { UI.SearchInputOptions } [searchInput] - initialise the search box component
 * @property { UI.SuggestionsOptions } [suggestions] - initialise the suggestions component
 */

/**
 * (Beta) Class that can be used to create a simple UI for a search index. See
 * the <a href="https://github.com/fergiemcdowall/search-index-demo">live
 * demo</a>
 * @param { UI.Options } [ops] - initialisation options
 * @example
 * import { SearchIndex, UI } from 'search-index'
 *
 * // ...
 *
 * new UI({
 *   index: si,
 *   count: {
 *     elementId: 'count'
 *   },
 *   facets: [
 *     {
 *       elementId: 'year-refiner',
 *       titleTemplate: '<p class="h6">YEAR</p>',
 *       field: 'year',
 *       mode: 'OR'
 *     },
 *     {
 *       elementId: 'month-refiner',
 *       titleTemplate: '<p class="h6">MONTH</p>',
 *       field: 'month',
 *       mode: 'OR',
 *       sort: (a, b) => {
 *         const monthNumber = month =>
 *           new Date(Date.parse(month + ' 1, 2012')).getMonth() + 1
 *         return monthNumber(a.VALUE) - monthNumber(b.VALUE)
 *       }
 *     }
 *   ],
 *   hits: {
 *     elementId: 'hits',
 *     template: doc => `<p>${JSON.stringify(doc)}</p>`
 *   },
 *   paging: { elementId: 'paging', pageSize: 2 },
 *   searchInput: {
 *     elementId: 'searchbox',
 *     suggestions: {
 *       elementId: 'suggestions',
 *       limit: 10,
 *       threshold: 1
 *     }
 *   }
 * })
 *
 */
export class UI {
  constructor ({
    index = null,
    count = {},
    hits = {},
    facets = [],
    searchInput = {},
    paging = {},
    suggestions = {}
  }) {
    this.index = index
    this.count = new Count(count)
    this.paging = new Paging(paging, this.search)
    this.hits = new Hits(hits)
    this.searchInput = new SearchInput(
      {
        // autoCompleteFunction: this.index.DICTIONARY,
        ...searchInput,
        suggestions: {
          autoCompleteFunction: this.index.DICTIONARY,
          ...searchInput.suggestions
        }
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
