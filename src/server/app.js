'use strict'

const path = require('path')
const { Backend, RestApi, ServingFilesEndpoint } = require('@cuties/rest')
const CustomNotFoundEndpoint = require('./endpoints/CustomNotFoundEndpoint')
const CustomInternalServerErrorEndpoint = require('./endpoints/CustomInternalServerErrorEndpoint')
const CustomIndexEndpoint = require('./endpoints/CustomIndexEndpoint')
const IsValidLispEndpoint = require('./endpoints/IsValidLispEndpoint')
const JSFromLispEndpoint = require('./endpoints/JSFromLispEndpoint')
const notFoundEndpoint = new CustomNotFoundEndpoint(new RegExp(/\/not-found/))
const internalServerErrorEndpoint = new CustomInternalServerErrorEndpoint(new RegExp(/^\/internal-server-error/))

const mapper = (url) => {
  return path.join('src', 'server', 'static', ...url.split('/').filter(path => path !== ''))
}

new Backend(
  'http', 
  8000, 
  '127.0.0.1',
  new RestApi(
    new CustomIndexEndpoint('./src/server/static/html/index.html', notFoundEndpoint),
    new ServingFilesEndpoint(new RegExp(/^\/(html|css|js)/), mapper, {}, notFoundEndpoint),
    new IsValidLispEndpoint(new RegExp(/^\/isValidLisp/), 'POST'),
    new JSFromLispEndpoint(new RegExp(/^\/convertToJS/), 'POST'),
    notFoundEndpoint,
    internalServerErrorEndpoint
  )
).call()
