/**
 * Paging element
 * @typedef { Object } PagingOptions
 * @memberof UI
 * @property { string } [ elementId="paging" ] - The id of the HTML element that contains paging
 * @property { function } [ pageLinkTemplate=(label, pageNumber, activeNumber) => `<li class="page-item ${pageNumber === activeNumber ? 'active' : ''}"><a class="page-link" data-page=${pageNumber} href="#">${label}</a></li>` ] - A template for each page number link
 * @property { function } [ navLinkTemplate=(label, pageNumber, disabled) => `<li class="page-item ${disabled ? 'disabled' : ''}"><a class="page-link" data-page=${pageNumber} href="#">${label}</a></li>` ] - A template for each pagination "arrow" link
 */
export class Paging {
  constructor (
    {
      pageLinkTemplate = (label, pageNumber, activeNumber) => `
    <li class="page-item ${pageNumber === activeNumber ? 'active' : ''}">
      <a class="page-link" data-page=${pageNumber} href="#">
        ${label}
      </a>
    </li>`,
      navLinkTemplate = (label, pageNumber, disabled) => `
    <li class="page-item ${disabled ? 'disabled' : ''}">
      <a class="page-link" data-page=${pageNumber} href="#">
        ${label}
      </a>
    </li>`,
      elementId = 'paging'
    },
    search
  ) {
    this.pageLinkTemplate = pageLinkTemplate
    this.navLinkTemplate = navLinkTemplate
    this.el = document.getElementById(elementId)
    this.search = search
    this.page = {
      NUMBER: 0,
      SIZE: 10
    }
  }

  template = p => {
    const pagePadding = 3 // Display 3 pages before and after current page
    const maxDisplayedPages = pagePadding * 2 + 1

    let pageFrom = 0
    let pageTo = p.TOTAL

    if (p.TOTAL > maxDisplayedPages) {
      pageFrom = p.NUMBER > pagePadding ? p.NUMBER - pagePadding : 0
      pageTo = maxDisplayedPages + pageFrom
    }
    if (pageTo > p.TOTAL) {
      pageFrom = p.TOTAL - pagePadding * 2 - 1
      pageTo = p.TOTAL
    }

    const getPageNumberLinks = () => {
      let html = ''
      for (let i = pageFrom; i < pageTo; i++) {
        html += this.pageLinkTemplate(i + 1, i, p.NUMBER)
      }
      return html
    }

    return `
     <ul class="pagination">
       ${this.navLinkTemplate('First', 0, p.NUMBER === 0)}
       ${this.navLinkTemplate('‹', p.NUMBER - 1, p.NUMBER === 0)}
       ${getPageNumberLinks()}
       ${this.navLinkTemplate('›', p.NUMBER + 1, p.NUMBER >= p.TOTAL - 1)}
       ${this.navLinkTemplate('Last', p.TOTAL - 1, p.NUMBER >= p.TOTAL - 1)}
     </ul>`
  }

  update = p => {
    // if only one page, don't show paging
    console.log(p)
    if (p.TOTAL <= 1) {
      this.el.innerHTML = ''
      return
    }
    this.el.innerHTML = this.template(p)
    const pagingLinks = document.getElementsByClassName('page-link')
    for (let i = 0; i < pagingLinks.length; i++) {
      pagingLinks[i].addEventListener('mousedown', e => {
        console.log('AYO!')
        console.log(this.search())
        this.page.NUMBER = +e.target.attributes['data-page'].value
        this.search()
      })
    }
  }
}
