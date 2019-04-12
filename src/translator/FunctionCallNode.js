'use strict'

const Node = require('./Node')
const ListNode = require('./ListNode')

class FunctionCallNode extends Node {
  constructor (value, children) {
    super(value, children)
  }

  toStrValue () {
    if (this.children instanceof ListNode) {
      return `${this.value}(${this.children.plainList()})`
    }
    return `${this.value}(${this.children.map(c => c.toStrValue()).join(', ')})`
  }
}

module.exports = FunctionCallNode
