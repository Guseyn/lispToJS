'use strict'

const parse = require('./../src/translator/parse')
const assert = require('assert')
const util = require('util')

// arithmetic
assert.deepStrictEqual(parse('(+ a (- b (* c (/ d e))))'), 
  [
    { type: 'identifier', value: '+' },
    { type: 'identifier', value: 'a' },
    [ 
      { type: 'identifier', value: '-' },
      { type: 'identifier', value: 'b' },
      [
        { type: 'identifier', value: '*' },
        { type: 'identifier', value: 'c' },
        [
          { type: 'identifier', value: '/' },
          { type: 'identifier', value: 'd' },
          { type: 'identifier', value: 'e' } 
        ]
      ]
    ]
  ]
)

// variable declaration
assert.deepStrictEqual(parse('(let ((var1 val1) (var2 val2)) ())'),
  [ { type: 'identifier', value: 'let' },
    [ [ { type: 'identifier', value: 'var1' },
        { type: 'identifier', value: 'val1' } ],
      [ { type: 'identifier', value: 'var2' },
        { type: 'identifier', value: 'val2' } ] ],
    [] 
  ]
)

// function declaration
assert.deepStrictEqual(parse('(defun name (v1 v2) (+ v1 v2))'),
  [ 
    { type: 'identifier', value: 'defun' },
    { type: 'identifier', value: 'name' },
    [ 
      { type: 'identifier', value: 'v1' },
      { type: 'identifier', value: 'v2' }
    ],
    [
      { type: 'identifier', value: '+' },
      { type: 'identifier', value: 'v1' },
      { type: 'identifier', value: 'v2' } 
    ]
  ]
)

// function call
assert.deepStrictEqual(parse('(name 10 10)'),
  [ 
    { type: 'identifier', value: 'name' },
    { type: 'number', value: 10 },
    { type: 'number', value: 10 } 
  ]
)

// loops
assert.deepStrictEqual(parse(`(loop for x in ("tom" "dick" "harry") do (format t " ~s" x))`),
  [ { type: 'identifier', value: 'loop' },
  { type: 'identifier', value: 'for' },
  { type: 'identifier', value: 'x' },
  { type: 'identifier', value: 'in' },
  [ { type: 'string', value: 'tom' },
    { type: 'string', value: 'dick' },
    { type: 'string', value: 'harry' } ],
  { type: 'identifier', value: 'do' },
  [ { type: 'identifier', value: 'format' },
    { type: 'identifier', value: 't' },
    { type: 'string', value: ' ~s' },
    { type: 'identifier', value: 'x' } ] ]
)


// branches
assert.deepStrictEqual(parse('(if (> a 20) (format t "~% a is less than 20"))'), 
  [
    { type: 'identifier', value: 'if' },
      [ { type: 'identifier', value: '>' },
        { type: 'identifier', value: 'a' },
        { type: 'number', value: 20 } ],
      [ { type: 'identifier', value: 'format' },
        { type: 'identifier', value: 't' },
        { type: 'string', value: '~% a is less than 20' } ] ])

// closures
assert.deepStrictEqual(parse('((lambda (x) (rest x)) ("a" "b" "c"))'), 
  [ 
    [ { type: 'identifier', value: 'lambda' },
      [ { type: 'identifier', value: 'x' } ],
      [ { type: 'identifier', value: 'rest' },
        { type: 'identifier', value: 'x' } ] 
    ],
    [ { type: 'string', value: 'a' },
      { type: 'string', value: 'b' },
      { type: 'string', value: 'c' } ] ])

// bad code
assert.throws(() => { parse('((l') }, new Error('invalid code'))
assert.throws(() => { parse('"l') }, new Error('invalid code'))


