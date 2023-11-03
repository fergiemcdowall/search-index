export class Facet {
  constructor (
    {
      facetOptionTemplate = (rOption, active) => `
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
    <br>`,
      facetTitleTemplate = title => `<h2>${title}</h2>`,
      elementId = '', // TODO: what should default element name be?
      sort = (a, b) => {
        a.VALUE.localeCompare(b.VALUE)
      },
      mode = 'OR',
      el = document.getElementById(elementId),
      title = null,
      field = null
    },
    search
  ) {
    this.activeFilters = []
    this.el = el
    this.elementId = elementId
    this.facetOptionTemplate = facetOptionTemplate
    this.facetTitleTemplate = facetTitleTemplate
    this.field = field
    this.mode = mode
    this.search = search
    this.sort = sort
    this.title = title
  }

  getActiveFilters = () =>
    this.activeFilters.length
      ? [
          {
            [this.mode]: this.activeFilters
          }
        ]
      : []

  update = (facets, source) => {
    // if ORing, dont redraw facets when selecting facet options
    if (source === 'facet' && this.mode === 'OR') return

    this.el.innerHTML =
      this.facetTitleTemplate(this.title) +
      facets
        .filter(item => item.FIELD === this.field)
        .sort(this.sort)
        .reduce(
          (acc, facetOption) =>
            acc +
            this.facetOptionTemplate(
              facetOption,
              this.activeFilters.includes(
                facetOption.FIELD + ':' + facetOption.VALUE
              )
            ),
          ''
        )

    // Display active filters even when no hits in filter
    this.activeFilters.forEach(filterOption => {
      if (!document.getElementById(filterOption)) {
        const [FIELD, VALUE] = filterOption.split(':')
        this.el.innerHTML += this.facetOptionTemplate(
          { FIELD, VALUE, _id: [] },
          true
        )
      }
    })

    // Event listener for selection/unselection
    // for (const filterCheckbox of document.getElementsByClassName(
    for (const filterCheckbox of document.querySelectorAll(
      '#' + this.elementId + ' > .filter-select'
    )) {
      filterCheckbox.addEventListener('input', e => {
        console.log(this.elementId)
        console.log(this.mode)
        const token =
          e.target.attributes['data-field'].value +
          ':' +
          e.target.attributes['data-value'].value
        if (e.target.checked) {
          this.activeFilters.push(token)
          // this.search(this.elementId)
        } else {
          this.activeFilters = this.activeFilters.filter(item => item !== token)
          // this.search()
        }
        this.search('facet')
      })
    }
  }
}
