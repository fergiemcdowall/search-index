// should be eventually bundled in with search-index

const ui = ({
  index = null,
  count = {},
  hits = {},
  refiners = [],
  searchInput = {}
} = {}) => {
  const countTemplate = count => `showing ${count} hits`
  const hitsTemplate = doc => `<p>${JSON.stringify(doc)}</p>`
  const refinerOptionTemplate = (value, set, token, active) => `
    <input class="filter-select"
           id=${token}
           name=${token}
           type="checkbox"
           ${active ? 'checked' : ''}>
    <label for=${token}>${value} (${set.length})</label>
    <br>`
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
    // debugger
    return [
      [
        ...searchInput.el.value.split(/\s+/).filter(item => item),
        ...activeFilters
      ],
      {
        FACETS: [
          {
            FIELD: ['month', 'year']
          }
        ],
        DOCUMENTS: true
      }
    ]
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
    refiners.forEach(refiner => {
      refiner.el.innerHTML =
        refinerTitleTemplate(refiner.title) +
        facets
          .filter(item => item.FIELD == refiner.field)
          .reduce(
            (acc, { VALUE, _id }) =>
              acc +
              refinerOptionTemplate(
                VALUE,
                _id,
                refiner.field + ':' + VALUE,
                activeFilters.includes(refiner.field + ':' + VALUE)
              ),
            ''
          )
    })
    const filterCheckboxes = document.getElementsByClassName('filter-select')
    for (let i = 0; i < filterCheckboxes.length; i++) {
      filterCheckboxes[i].addEventListener('input', function (e) {
        if (e.target.checked) {
          activeFilters.push(e.target.name)
        } else {
          activeFilters = activeFilters.filter(item => item !== e.target.name)
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
