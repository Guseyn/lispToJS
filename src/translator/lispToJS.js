'use strict'

const parse = require('./parse')
const beautify = require('js-beautify').js
const Node = require('./Node')
const BinaryOperationNode = require('./BinaryOperationNode')
const VariableDeclarationNode = require('./VariableDeclarationNode')
const FunctionDeclarationNode = require('./FunctionDeclarationNode')
const FunctionCallNode = require('./FunctionCallNode')
const ListNode = require('./ListNode')
const LoopNode = require('./LoopNode')
const BranchNode = require('./BranchNode')
const ClosureNode = require('./ClosureNode')
const funcNames = ['format']

function buildTree (parsedObj) {
  let node
  if (Array.isArray(parsedObj)) {
    if (parsedObj[0]) {
      if (parsedObj[0].type === 'identifier') {
        if (['+', '-', '*', '/', '>', '>=', '<', '<=', '==', '!=', '&&', '&', '||', '|'].indexOf(parsedObj[0].value) !== -1) {
          node = new BinaryOperationNode(
            parsedObj[0].value,
            parsedObj.slice(1).map(
              p => buildTree(p)
            )
          )
        } else if (parsedObj[0].value === 'let') {
          node = new VariableDeclarationNode(
            parsedObj[0].value,
            parsedObj[1].map(
              p => buildTree(p)
            )
          )
        } else if (parsedObj[0].value === 'defun') {
          funcNames.push(parsedObj[1].value)
          node = new FunctionDeclarationNode(
            parsedObj[1].value,
            [
              buildTree(parsedObj[1]), // name
              buildTree(parsedObj[2]), // params
              buildTree(parsedObj[3]) // body
            ]
          )
        } else if (funcNames.indexOf(parsedObj[0].value) !== -1) {
          node = new FunctionCallNode(
            parsedObj[0].value,
            Array.isArray(parsedObj[1]) 
              ? buildTree(parsedObj[1])
              : parsedObj.slice(1).map(
                  p => buildTree(p)
                )
          )
        } else if (parsedObj[0].value === 'loop') {
          node = new LoopNode(
            [
              buildTree(parsedObj[2]),
              buildTree(parsedObj[4]),
              buildTree(parsedObj[6])
            ]
          )
        } else if (parsedObj[0].value === 'if') {
          node = new BranchNode(
            [
              buildTree(parsedObj[1]),
              buildTree(parsedObj[2]),
              parsedObj[3] ? buildTree(parsedObj[3]) : undefined
            ]
          )
        } else if (parsedObj[0].value === 'lambda') {
          node = new ClosureNode(
            [
              buildTree(parsedObj[1]),
              buildTree(parsedObj[2])
            ]
          )
        } else {
          node = new ListNode(
            parsedObj.map(p => buildTree(p))
          )
        }
      } else {
        node = new ListNode(
          parsedObj.map(p => buildTree(p))
        )
      }
    }
  } else {
    if (parsedObj.type === 'identifier' || parsedObj.type === 'number') {
      node = new Node(parsedObj.value)
    } else if (parsedObj.type === 'string') {
      node = new Node(`'${parsedObj.value}'`)
    }
  }
  return node
}

function lispToJS (lispCode) {
  let jsCode = ''
  lispCode = lispCode.replace(/\n/g, '')
  let lb = 0
  let rb = 0
  for (let i = 0; i < lispCode.length; i++) {
    if (lispCode[i] === '(') {
      lb += 1
    } else if (lispCode[i] === ')') {
      rb += 1
      if (lb === rb) {
        lispCode = lispCode.slice(0, i + 1) + '\n' + lispCode.slice(i + 1)
      }
    }
  }
  lispCode.split('\n').forEach(line => {
    jsCode += `${buildTree(parse(line)).toStrValue()}\n`
  })
  return beautify(jsCode, { indent_size: 2, space_in_empty_paren: true })
}

module.exports = lispToJS
