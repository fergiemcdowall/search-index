// should be eventually bundled in with search-index
import { autocomplete } from '/lib/autocomplete.js'

export const ui = ({
  index = null,
  count = {},
  hits = {},
  refiners = [],
  searchInput = {}
} = {}) => {
  const countTemplate = count => `showing ${count} hits`
  const hitsTemplate = doc => `<p>${JSON.stringify(doc)}</p>`
  const refinerOptionTemplate = (rOption, active) => {
    console.log(active)
    return `
    <input class="filter-select"
           id=${rOption.FIELD + ':' + rOption.VALUE}
           name=${rOption.FIELD + ':' + rOption.VALUE}
           type="checkbox"
           data-field=${rOption.FIELD}
           data-value=${rOption.VALUE}
           ${active ? 'checked' : ''}>
    <label for=${rOption.FIELD + ':' + rOption.VALUE}>${rOption.VALUE} (${
      rOption._id.length
    })</label>
    <br>`
  }
  const refinerTitleTemplate = title => `<h2>${title}</h2>`

  count = { template: countTemplate, elementId: 'count', ...count }
  count.el = document.getElementById(count.elementId)

  searchInput = {
    elementId: 'searchbox',
    ...searchInput
  }
  searchInput.el = document.getElementById(searchInput.elementId)

  hits = {
    template: hitsTemplate,
    elementId: 'hits',
    ...hits
  }
  hits.el = document.getElementById(hits.elementId)

  refiners = refiners.map(refiner => ({
    el: document.getElementById(refiner.elementId),
    sort: (a, b) => {
      a.VALUE.localeCompare(b.VALUE)
    },
    ...refiner
  }))

  // treat empty search as a special case: instead of showing nothing,
  // show everything.
  const emptySearchQuery = () => [
    {
      ALL_DOCUMENTS: true
    },
    {
      FACETS: [
        {
          FIELD: ['month', 'year']
        }
      ]
    }
  ]

  const searchQuery = () => {
    const q = [
      [
        ...searchInput.el.value.split(/\s+/).filter(item => item),
        ...activeFilters
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
        DOCUMENTS: true
      }
    ]
    return q
  }

  const search = () =>
    (searchInput.el.value.length + activeFilters.length
      ? index.SEARCH(...searchQuery())
      : index.QUERY(...emptySearchQuery())
    )
      .then(result => ({
        // query: q,
        result // TODO: should this be nested like this?
      }))
      .then(res => {
        console.log(res)
        return res
      })
      .then(displaySearch)

  const displaySearch = res => {
    hits.el.innerHTML = res.result.RESULT.map(({ _doc }) =>
      hits.template(_doc)
    ).join('\n')
    count.el.innerHTML = count.template(res.result.RESULT_LENGTH)
    displayRefiners(res.result.FACETS)
  }

  let activeFilters = []

  const displayRefiners = facets => {
    // List all refiner options
    refiners.forEach(refiner => {
      refiner.el.innerHTML =
        refinerTitleTemplate(refiner.title) +
        facets
          .filter(item => item.FIELD == refiner.field)
          .sort(refiner.sort)
          .reduce(
            (acc, refinerOption) =>
              acc +
              refinerOptionTemplate(
                refinerOption,
                activeFilters.includes(
                  refinerOption.FIELD + ':' + refinerOption.VALUE
                )
              ),
            ''
          )

      // Display active filters even when no hits in filter
      activeFilters.forEach(refinerOption => {
        if (!document.getElementById(refinerOption)) {
          const [FIELD, VALUE] = refinerOption.split(':')
          refiner.el.innerHTML += refinerOptionTemplate(
            { FIELD: FIELD, VALUE: VALUE, _id: [] },
            true
          )
        }
      })
    })

    // Event listener for selection/unselection
    const filterCheckboxes = document.getElementsByClassName('filter-select')
    for (let i = 0; i < filterCheckboxes.length; i++) {
      filterCheckboxes[i].addEventListener('input', function (e) {
        const token =
          e.target.attributes['data-field'].value +
          ':' +
          e.target.attributes['data-value'].value
        if (e.target.checked) {
          activeFilters.push(token)
        } else {
          activeFilters = activeFilters.filter(item => item !== token)
        }
        search()
      })
    }
  }

  searchInput.el.addEventListener('input', function (e) {
    search(this.value)
  })

  autocomplete(
    document.getElementById(searchInput.elementId),
    index.DICTIONARY,
    search
  )

  search()
}
