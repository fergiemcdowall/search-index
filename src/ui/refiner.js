export class Refiner {
  constructor(
    {
      refinerOptionTemplate = (rOption, active) => `
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
      refinerTitleTemplate = title => `<h2>${title}</h2>`,
      elementId = 'count',
      sort = (a, b) => {
        a.VALUE.localeCompare(b.VALUE)
      },
      el = document.getElementById(elementId),
      title = null,
      field = null
    },
    search
  ) {
    this.activeFilters = []
    this.el = el
    this.field = field
    this.refinerOptionTemplate = refinerOptionTemplate
    this.refinerTitleTemplate = refinerTitleTemplate
    this.search = search
    this.sort = sort
    this.title = title
  }

  render = facets => {
    this.el.innerHTML =
      this.refinerTitleTemplate(this.title) +
      facets
        .filter(item => item.FIELD == this.field)
        .sort(this.sort)
        .reduce(
          (acc, refinerOption) =>
            acc +
            this.refinerOptionTemplate(
              refinerOption,
              this.activeFilters.includes(
                refinerOption.FIELD + ':' + refinerOption.VALUE
              )
            ),
          ''
        )

    // Event listener for selection/unselection
    for (let filterCheckbox of document.getElementsByClassName(
      'filter-select'
    )) {
      filterCheckbox.addEventListener('input', e => {
        const token =
          e.target.attributes['data-field'].value +
          ':' +
          e.target.attributes['data-value'].value
        if (e.target.checked) {
          this.activeFilters.push(token)
        } else {
          this.activeFilters = this.activeFilters.filter(item => item !== token)
        }
        this.search()
      })
    }
  }
}
