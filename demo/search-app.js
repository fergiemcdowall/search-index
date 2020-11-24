var si = null

const search = q => si.QUERY(q, {
  DOCUMENTS: true
}).then(result => {
  const rUl = document.getElementById('result')
  rUl.innerHTML = ''
  result.forEach(({ _doc }) => {
    console.log(_doc)
    const li = document.createElement('li')
    li.innerHTML = `
      <img src="${_doc.thumbnail}" />
      ${_doc.title}
    `
    rUl.appendChild(li)
  })
})

Promise.all([
  SearchIndex({ name: 'mySearchIndex' }),
  fetch('./EarthPorn-top-search-index.json').then(res => res.json())
]).then(([ thisSi, dump ]) => {
  si = thisSi
  si.IMPORT(dump)
    .then(() => search(''))
})

document.getElementById('query')
  .addEventListener('input', function (e) {
    console.log(this.value)
    search(this.value)
  })

