export class Count {
  constructor({
    template = count => `${count} hits`,
    elementId = 'count',
    el = document.getElementById(elementId)
  }) {
    this.template = template
    this.el = el
  }

  update = count => (this.el.innerHTML = this.template(count))
}
