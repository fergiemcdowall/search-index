/**
 * Options for elements used to display faceted navigation
 * @typedef { Object } FacetOptions
 * @memberof UI
 * @property { Array.<string> } [ activeFilters=[] ] - An array of the filters that are currently active
 * @property { string } elementId - The <code>id</code> of the element to be used
 * @property { function } [facetOptionTemplate=(rOption, active) => `<input class="filter-select"id=${rOption.FIELD + ':' + rOption.VALUE}name=${rOption.FIELD + ':' + rOption.VALUE}type="checkbox"data-field=${rOption.FIELD}data-value=${rOption.VALUE}${active ? 'checked' : ''}><label for=${rOption.FIELD + ':' + rOption.VALUE}>${rOption.VALUE} (${ rOption._id.length })</label> <br>`] - The template to be used per facet option
 * @property { string } field - The name of the field that this facet applies to
 * @property { string } [ mode="OR" ] - Can be <code>"AND"</code> or <code>"OR"</code>. Combine filters with AND or OR.
 * @property { string } [ sort=(a, b) => { a.VALUE.localeCompare(b.VALUE) } ] - Function that determines sorting of facet options
 * @property { string } [ title="<p>" + field.toUpperCase() + "</p>" ] - The HTML template to be used per facet title
 */

export class Facet {
  constructor (
    {
      elementId = '',
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
      field = null,
      mode = 'OR',
      sort = (a, b) => {
        a.VALUE.localeCompare(b.VALUE)
      },
      title = '<p>' + field.toUpperCase() + '</p>'
    },
    search
  ) {
    this.activeFilters = []
    this.el = document.getElementById(elementId)
    this.elementId = elementId
    this.facetOptionTemplate = facetOptionTemplate
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
    const options = facets.filter(item => item.FIELD === this.field)

    this.el.innerHTML = options.length
      ? this.title +
        options
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
      : ''

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
        } else {
          this.activeFilters = this.activeFilters.filter(item => item !== token)
        }
        this.search('facet')
      })
    }
  }
}
