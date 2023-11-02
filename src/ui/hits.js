export class Hits {
  constructor({
    template = doc => `<p>${JSON.stringify(doc)}</p>`,
    elementId = 'hits',
    el = document.getElementById(elementId)
  }) {
    this.template = template
    this.el = el
  }

  update = results => {
    console.log('UPDATING')
    console.log(results)

    return (this.el.innerHTML = results
      .map(({ _doc }) => this.template(_doc))
      .join('\n'))
  }
}
