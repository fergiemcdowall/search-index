const si = new SearchIndex.SearchIndex({
  name: 'mySearchIndex',
  stopwords
})

fetch('data/EarthPorn-top-search-index.json')
  .then(res => res.json())
  .then(si.IMPORT)
  .catch(e => console.error('problem with import'))

window.onload = function () {
  ui({
    index: si,
    count: {
      elementId: 'count'
    },
    hits: {
      elementId: 'hits',
      template: doc => `
        <b>${doc.title}</b>
        <br>
        <a href=${doc.url_overridden_by_dest} target=_blank>
          <img src=${doc.thumbnail}>
        </a>
        <p>${JSON.stringify(doc)}</p>`
    },
    refiners: [
      { elementId: 'year-refiner', title: 'YEAR', field: 'year' },
      { elementId: 'month-refiner', title: 'MONTH', field: 'month' }
    ],
    searchInput: { elementId: 'searchbox' }
  })
}
