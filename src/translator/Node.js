'use strict'

class Node {
  constructor (value, children) {
    this.value = value
    this.children = children
  }

  toStrValue () {
    return this.value
  }
}

module.exports = Node
