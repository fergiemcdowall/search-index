// should be eventually bundled in with search-index

const ui = ({
  _import = '',
  count = {},
  hits = {},
  refiners = [],
  searchInput = {}
} = {}) => {
  const el = (type, innerHTML = '', attributes = {}) => {
    const el = document.createElement(type)
    el.innerHTML = innerHTML
    for (const [attr, value] of Object.entries(attributes)) {
      el.setAttribute(attr, value)
    }
    return el
  }

  const countTemplate = count => `showing ${count} hits`
  const hitsTemplate = doc => `<div>${JSON.stringify(doc)}</div>`

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
  // refiners = refiners.map(({ elementId }) => document.getElementById(elementId))

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
    (searchInput.el.value.length + getActiveFilters().length
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

  const displaySearch = res => {
    hits.el.innerHTML = res.result.RESULT.map(({ _doc }) =>
      hits.template(_doc)
    ).join('\n')
    count.el.innerHTML = count.template(res.result.RESULT_LENGTH)
    refiners.forEach(refiner => {
      const activeFilters = getActiveFilters()
      refiner.el.innerHTML = ''
      refiner.el.appendChild(el('h2', refiner.title))
      res.result.FACETS.filter(item => item.FIELD == refiner.field).forEach(
        ({ VALUE, _id }) => {
          const checkbox = el('input', null, {
            class: 'filter-select',
            id: refiner.field + ':' + VALUE,
            name: refiner.field + ':' + VALUE,
            type: 'checkbox'
          })
          if (activeFilters.includes(refiner.field + ':' + VALUE))
            checkbox.setAttribute('checked', true)
          refiner.el.appendChild(checkbox)
          refiner.el.appendChild(
            el('label', VALUE + ' (' + _id.length + ')', {
              for: refiner.field + ':' + VALUE
            })
          )
          refiner.el.appendChild(el('br'))

          const isThisAnActiveFilter = activeFilters.indexOf(
            refiner.field + ':' + VALUE
          )
          if (isThisAnActiveFilter !== -1) {
            activeFilters.splice(isThisAnActiveFilter, 1)
          }
        }
      )

      activeFilters
        .filter(item => item.split(':')[0] == refiner.field)
        .forEach(item => {
          refiner.el.appendChild(
            el('input', null, {
              class: 'filter-select',
              id: item,
              name: item,
              type: 'checkbox',
              checked: true
            })
          )
          refiner.el.appendChild(
            el('label', item.split(':')[1] + ' (0)', {
              for: item
            })
          )
          refiner.el.appendChild(el('br'))
        })

      const filterCheckboxes = document.getElementsByClassName('filter-select')
      for (let i = 0; i < filterCheckboxes.length; i++) {
        filterCheckboxes[i].addEventListener('input', function (e) {
          search()
        })
      }
    })
  }

  if (_import)
    fetch(_import)
      .then(res => res.json())
      .then(dump => si.IMPORT(dump))
      .then(search)
      .catch(e => console.error(_import + ' is unreachable'))

  searchInput.el.addEventListener('input', function (e) {
    search(this.value)
  })

  autocomplete(document.getElementById('searchbox'), si.DICTIONARY, search)
}
