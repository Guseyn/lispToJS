'use strict'

const Node = require('./Node')

class BinaryOperationNode extends Node {
  constructor (value, children) {
    super(value, children)
  }

  toStrValue () {
    return `( ${this.children.map(c => c.toStrValue()).join(` ${this.value} `)} )`
  }
}

module.exports = BinaryOperationNode
