'use strict'

const Node = require('./Node')

class FunctionDeclarationNode extends Node {
  constructor (value, children) {
    super(value, children)
  }

  toStrValue () {
    return `function ${this.children[0].toStrValue()} (${this.children[1].plainList()}) {
  ${this.canReturn(this.children[2].toStrValue()) ? 'return ' : ''}${this.children[2].toStrValue()}
}`
  }

  canReturn (str) {
    return !str.startsWith('if') && !str.startsWith('for')
  }
}

module.exports = FunctionDeclarationNode 
