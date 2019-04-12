'use strict'

const Node = require('./Node')

class LoopNode extends Node {
  constructor (children) {
    super('', children)
  }

  toStrValue () {
    return `for (let ${this.children[0].toStrValue()} in ${this.children[1].toStrValue()}) {
  ${this.children[2].toStrValue()}
}`
  }
}

module.exports = LoopNode
