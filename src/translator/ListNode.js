'use strict'

const Node = require('./Node')

class ListNode extends Node {
  constructor (children) {
    super(', ', children)
  }

  toStrValue () {
    return `[${this.children.map(c => c.toStrValue()).join(this.value)}]`
  }

  plainList () {
    return  this.children.map(c => c.toStrValue()).join(', ')
  }
}

module.exports = ListNode
