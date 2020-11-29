var si = null



// const search = q => si.QUERY({
//   SEARCH: q.trim().split(/\s+/)
// }, {
//   DOCUMENTS: true
// }).then(result => {

const search = q => si.QUERY({
  AGGREGATE: {
    FACETS: {
      FIELD: 'year'
    },
    QUERY: {
      SEARCH: q.trim().split(/\s+/)
    }
  }
}).then(({ RESULT, FACETS }) => {
  console.log(RESULT)
  const rUl = document.getElementById('result')
  rUl.innerHTML = ''
  RESULT.forEach(({ _doc }) => {
    console.log(_doc)
    const li = document.createElement('li')
    li.innerHTML = `
      <img src="${_doc.thumbnail}" />
      ${_doc.title}
      <p>      
        <a href="https://www.reddit.com/user/${_doc.author}"
           target="_blank">
          ${_doc.author}</a> 
        ${_doc.month}
        ${_doc.year}
      </p>
    `
    rUl.appendChild(li)
  })
})

Promise.all([
  SearchIndex({ name: 'mySearchIndex' }),
  fetch('./EarthPorn-top-search-index.json').then(res => res.json())
]).then(([ thisSi, dump ]) => {
  si = thisSi
  si.IMPORT(dump).then(() => search(''))
})

document.getElementById('query')
  .addEventListener('input', function (e) {
    search(this.value)
  })

