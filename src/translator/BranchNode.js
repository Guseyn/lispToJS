'use strict'

const Node = require('./Node')

class BranchNode extends Node {
  constructor (children) {
    super('', children)
  }

  toStrValue () {
    return `if (${this.children[0].toStrValue()}) {
  return ${this.children[1].toStrValue()}
}${this.children[2] ? `\nreturn ${this.children[2].toStrValue()}` : '' }`
  }
}

module.exports = BranchNode
