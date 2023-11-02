export class Hits {
  constructor ({
    template = doc => `<p>${JSON.stringify(doc)}</p>`,
    elementId = 'hits',
    el = document.getElementById(elementId)
  }) {
    this.template = template
    this.el = el
  }

  update = results =>
    (this.el.innerHTML = results
      .map(({ _doc }) => this.template(_doc))
      .join('\n'))
}
