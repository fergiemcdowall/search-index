import { stopwords } from '../../../../../../lib/stopwords.js'
import { ui } from '../../../../../../lib/ui.js'
import { SearchIndex } from '../../../../../../lib/search-index-esm-5.0.0-rc1.js'

const si = new SearchIndex({
  name: 'mySearchIndex',
  stopwords
})

Promise.all([
  fetch('data/EarthPorn-top-search-index.json')
    .then(res => res.json())
    .then(si.IMPORT),
  new Promise(resolve => (window.onload = () => resolve()))
]).then(() =>
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
<!--        <p>${JSON.stringify(doc)}</p>-->`
    },
    refiners: [
      { elementId: 'year-refiner', title: 'YEAR', field: 'year' },
      {
        elementId: 'month-refiner',
        title: 'MONTH',
        field: 'month',
        sort: (a, b) => {
          const monthNumber = month =>
            new Date(Date.parse(month + ' 1, 2012')).getMonth() + 1
          return monthNumber(a.VALUE) - monthNumber(b.VALUE)
        }
      }
    ],
    paging: { elementId: 'paging', pageSize: 2 },
    searchInput: { elementId: 'searchbox' }
  })
)
