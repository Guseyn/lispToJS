'use strict'

const { AsyncObject } = require('@cuties/cutie')
const lispToJS = require('./../../../index')

class JSFromLisp extends AsyncObject {
  constructor (code) {
    super(code)
  }

  syncCall () {
    return (code) => {
      try {
        return { js: lispToJS(code) }
      } catch (err) {
        return { js: 'Not Valid Lisp' }
      }
    }
  }
}

module.exports = JSFromLisp
