function initHighlightUpdater () {
  let lispEditor = document.getElementById('lisp')
  let jsEditor = document.getElementById('js')
  lispEditor.addEventListener('blur', () => {
    hljs.highlightBlock(lispEditor)
  })
  lispEditor.addEventListener('input', () => {
    updateEditors(lispEditor, jsEditor)
  })
  lispEditor.addEventListener('keypress', (e) => {
    if (e.keyCode === 13) {
      document.execCommand('insertHTML', false, '\n');
      return false;
    }
  })
  updateEditors(lispEditor, jsEditor)
}

function updateEditors (lispEditor, jsEditor) {
  postData('/convertToJS', lispEditor.textContent)
  .then(data => {
    jsEditor.innerHTML = data.js
    hljs.highlightBlock(jsEditor)
  })
  .catch(error => console.error(error))
}

function postData(url = '', data = {}) {
  console.log(data)
  return fetch(url, {
    method: 'POST',
    headers: {
      'Content-Type': 'text/plain'
    },
    body: data,
  })
  .then(response => response.json())
}
