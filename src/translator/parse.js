'use strict'

function parse (input) {
  return parenthesize(tokenize(validate(input)));
}

function validate (input) {
  let leftBracketsCount = 0
  let rightBracketsCount = 0
  let quotesCount = 0
  for (let i = 0; i < input.length; i++) {
    if (input[i] === '(') {
      leftBracketsCount += 1
    } else if (input[i] === ')') {
      rightBracketsCount += 1
    } else if (input[i] === '"') {
      quotesCount += 1
    }
  }
  if (leftBracketsCount !== rightBracketsCount || quotesCount % 2 !== 0) {
    throw new Error('invalid code')
  }
  return input
}

function tokenize (input) {
  return input.split('"')
    .map(function(x, i) {
      if (i % 2 === 0) { // not in string
        return x.replace(/\(/g, ' ( ')
                .replace(/\)/g, ' ) ')
      } else { // in string
        return x.replace(/ /g, '!whitespace!')
      }
    })
    .join('"')
    .trim()
    .split(/\s+/)
    .map(function(x) {
      return x.replace(/!whitespace!/g, " ")
    });
}

function parenthesize (input, list = []) {
  var token = input.shift()
  if (token === undefined) {
    return list.pop();
  } else if (token === "(") {
    list.push(parenthesize(input, []))
    return parenthesize(input, list)
  } else if (token === ")") {
    return list
  } else {
    return parenthesize(input, list.concat(categorize(token)))
  }
}

function categorize (input) {
  if (!isNaN(parseFloat(input))) {
    return { type:'number', value: parseFloat(input) }
  } else if (input[0] === '"' && input.slice(-1) === '"') {
    return { type:'string', value: input.slice(1, -1) }
  } else {
    return { type:'identifier', value: input }
  }
}

module.exports = parse
