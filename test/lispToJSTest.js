const assert = require('assert')

const lispToJS = require ('./../index')

// assert.strictEqual('')
assert.strictEqual(lispToJS('(x (y (z) (a)))'), '[x, [y, [z],\n  [a]\n]]')
assert.strictEqual(lispToJS('(+ a (- b (* c (/ d e))) (+ f g) )'), '(a + (b - (c * (d / e))) + (f + g))')
assert.strictEqual(lispToJS('(let ((var1 val1) (var2 val2)) ())'), 'let var1 = val1\nlet var2 = val2')
assert.strictEqual(lispToJS('(defun name (v1 v2) (+ v1 v2))'), 'function name(v1, v2) {\n  return (v1 + v2)\n}')
assert.strictEqual(lispToJS('(defun name (v1 v2) (+ v1 v2)) \n (name (10, 10, "bad"))'), 'function name(v1, v2) {\n  return (v1 + v2)\n}\nname(10, 10, \'bad\')')
assert.strictEqual(lispToJS(`(loop for x in ("tom" "dick" ("harry" "nina")) do (format (t "~s" x)))`), 'for (let x in [\'tom\', \'dick\', [\'harry\', \'nina\']]) {\n  format(t, \'~s\', x)\n}')
assert.strictEqual(lispToJS('(if (< a 20) (format (t "~% a is less than 20")))'), 'if ((a < 20)) {\n  return format(t, \'~% a is less than 20\')\n}')
assert.strictEqual(lispToJS('(lambda (x y) (+ x 10))'), '(x, y) => {\n  return (x + 10)\n}')

// Find max
assert.strictEqual(lispToJS(`(defun max (v1 v2) (if (> v1 v2) v1 v2))`), 'function max(v1, v2) {\n  if ((v1 > v2)) {\n    return v1\n  }\n  return v2\n}')
