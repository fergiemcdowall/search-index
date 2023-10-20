const si = new SearchIndex.SearchIndex({
  name: 'mySearchIndex',
  stopwords
})

const ui = ops => {
  ops = {
    count: 'count',
    searchInput: 'searchInput',
    hits: 'hits',
    filters: 'refiner',
    ...ops
  }

  const searchInput = document.getElementById(ops.searchInput)
  const hits = document.getElementById(ops.hits)
  const count = document.getElementById(ops.count)
  const filters = document.getElementsByClassName(ops.filters)

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
        ...searchInput.value.split(/\s+/).filter(item => item),
        ...getActiveFilters()
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
    (searchInput.value.length + getActiveFilters().length
      ? si.SEARCH(...searchQuery())
      : si.QUERY(...emptySearchQuery())
    )
      .then(result => ({
        // query: q,
        result // TODO: should this be nested like this?
      }))
      .then(displaySearch)

  const getActiveFilters = () =>
    [...document.querySelectorAll('.filter-select:checked')].map(e => e.name)

  const el = (type, innerHTML, attributes = {}) => {
    const el = document.createElement(type)
    if (innerHTML) el.innerHTML = innerHTML
    for (const [attr, value] of Object.entries(attributes)) {
      el.setAttribute(attr, value)
    }
    return el
  }

  const displaySearch = res => {
    hits.innerHTML = res.result.RESULT.map(
      ({ _doc }) =>
        '<b>' +
        _doc.title +
        '</b><br><a href=' +
        _doc.url_overridden_by_dest +
        ' target=_blank><img src=' +
        _doc.thumbnail +
        '></a><div>' +
        JSON.stringify(_doc) +
        '</div>'
    ).join('\n')
    count.innerHTML = `showing ${res.result.RESULT_LENGTH} hits`
    for (let i = 0; i < filters.length; i++) {
      let field = filters[i].getAttribute('data-field')
      const activeFilters = getActiveFilters()

      filters[i].innerHTML = ''
      filters[i].appendChild(el('h2', field.toUpperCase()))
      res.result.FACETS.filter(item => item.FIELD == field).forEach(
        ({ VALUE, _id }) => {
          const checkbox = el('input', null, {
            class: 'filter-select',
            id: field + ':' + VALUE,
            name: field + ':' + VALUE,
            type: 'checkbox'
          })
          if (activeFilters.includes(field + ':' + VALUE))
            checkbox.setAttribute('checked', true)
          filters[i].appendChild(checkbox)
          filters[i].appendChild(
            el('label', VALUE + ' (' + _id.length + ')', {
              for: field + ':' + VALUE
            })
          )
          filters[i].appendChild(el('br'))

          const isThisAnActiveFilter = activeFilters.indexOf(
            field + ':' + VALUE
          )
          if (isThisAnActiveFilter !== -1) {
            activeFilters.splice(isThisAnActiveFilter, 1)
          }
        }
      )

      activeFilters
        .filter(item => item.split(':')[0] == field)
        .forEach(item => {
          filters[i].appendChild(
            el('input', null, {
              class: 'filter-select',
              id: item,
              name: item,
              type: 'checkbox',
              checked: true
            })
          )
          filters[i].appendChild(
            el('label', item.split(':')[1] + ' (0)', {
              for: item
            })
          )
          filters[i].appendChild(el('br'))
        })

      const filterCheckboxes = document.getElementsByClassName('filter-select')
      for (let i = 0; i < filterCheckboxes.length; i++) {
        filterCheckboxes[i].addEventListener('input', function (e) {
          search()
        })
      }
    }
  }

  fetch('data/EarthPorn-top-search-index.json')
    .then(res => res.json())
    .then(dump => si.IMPORT(dump))
    .then(search)

  searchInput.addEventListener('input', function (e) {
    search(this.value)
  })

  // TODO autocomplete should take a promise
  autocomplete(document.getElementById('searchbox'), si.DICTIONARY, search)
}

window.onload = function () {
  ui({
    hits: 'hits',
    searchInput: 'searchbox'
  })
}
