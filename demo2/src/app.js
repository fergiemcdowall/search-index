const si = new SearchIndex.SearchIndex({
  name: 'mySearchIndex',
  stopwords
})

window.onload = function () {
  ui({
    _import: 'data/EarthPorn-top-search-index.json',
    count: {
      elementId: 'count'
    },
    hits: {
      elementId: 'hits',
      template: doc =>
        '<b>' +
        doc.title +
        '</b><br><a href=' +
        doc.url_overridden_by_dest +
        ' target=_blank><img src=' +
        doc.thumbnail +
        '></a><div>' +
        JSON.stringify(doc) +
        '</div>'
    },
    refiners: [
      { elementId: 'year-refiner', title: 'Yeario', field: 'year' },
      { elementId: 'month-refiner', title: 'month', field: 'month' }
    ],
    searchInput: { elementId: 'searchbox' }
  })
}
