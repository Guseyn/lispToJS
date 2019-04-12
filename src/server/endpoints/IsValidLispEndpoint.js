'use strict'

const { CreatedReadStream } = require('@cuties/fs')
const { ResponseWithStatusCode, ResponseWithHeader, ResponseWithHeaders, EndedResponse } = require('@cuties/http')
const { Endpoint, RequestBody } = require('@cuties/rest')
const { StringifiedJSON } = require('@cuties/json')
const { StringFromBuffer } = require('@cuties/buffer')
const IsValidLisp = require('./../async/IsValidLisp')

class IsValidLispEndpoint extends Endpoint {
  constructor (regexpUrl, type) {
    super(regexpUrl, type)
  }

  body (request, response) {
    return new EndedResponse(
      new ResponseWithStatusCode(
        new ResponseWithHeader(
          response, 'Content-Type', 'application/json'
        ), 200
      ),
      new StringifiedJSON(
        new IsValidLisp(
          new StringFromBuffer(
            new RequestBody(
              request
            )
          )
        )
      )
    )
  }
}

module.exports = IsValidLispEndpoint
