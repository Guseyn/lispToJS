'use strict'

const { AsyncObject } = require('@cuties/cutie')
const lispToJS = require('./../../../index')

class IsValidLisp extends AsyncObject {
  constructor (code) {
    super(code)
  }

  syncCall () {
    return (code) => {
      try {
        lispToJS(code)
        return { isValid: true }
      } catch (err) {
        return { isValid: false }
      }
    }
  }
}

module.exports = IsValidLisp
