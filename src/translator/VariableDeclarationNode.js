'use strict'

const Node = require('./Node')

class VariableDeclarationNode extends Node {
  constructor (value, children) {
    super(value, children)
  }

  toStrValue () {
    return this.children.map(c => this.value + ' ' + c.plainList().split(', ').join(' = ')).join('\n')
  }
}

module.exports = VariableDeclarationNode
