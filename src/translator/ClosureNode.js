'use strict'

const Node = require('./Node')

class ClosureNode extends Node {
  constructor (children) {
    super('', children)
  }

  toStrValue () {
    return `(${this.children[0].plainList()}) => {
  return ${this.children[1].toStrValue()}
}`
  }
}

module.exports = ClosureNode
