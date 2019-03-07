// noinspection JSUnresolvedVariable
import { getCached, setCache } from './cache-helpers'
import './main.scss'
import { Elm } from './Main.elm'
import * as R from 'ramda'
import PouchDB from 'pouchdb-browser'
import debounce from 'lodash.debounce'
import ms from 'ms'
import validate from 'aproba'
import nanoid from 'nanoid'

/* ITEM */

// const rootItemId = 'i_root_item_id'
// const initialRootItem = {
//   id: rootItemId,
//   rev: null,
//   title: 'Root Item',
//   pid: null,
//   childIds: [],
// }
//
// function createNewItem() {
//   return {
//     id: 'i_' + nanoid(),
//     rev: null,
//     title: faker.lorem.words(),
//     pid: rootItemId,
//     childIds: [],
//   }
// }

/* PRE ELM APP INIT */

// function focusOffset(event, offset) {
//   const focusable = Array.from(
//     document.querySelectorAll('[data-is-focusable=true]'),
//   )
//   const idx = focusable.indexOf(event.target)
//   R.compose(
//     R.unless(R.isNil)(R.invoker(0, 'focus')),
//     R.defaultTo(focusable[0]),
//     R.nth(idx + offset),
//   )(focusable)
// }
//
// window.addEventListener('keydown', function(event) {
//   const isFocusable = !!event.target.dataset.isFocusable
//   console.log('isFocusable', isFocusable)
//   if (isFocusable) {
//     // console.debug(idx, focusable)
//     if (isHotKey('up')(event)) {
//       focusOffset(event, -1)
//     } else if (isHotkey('down')(event)) {
//       focusOffset(event, 1)
//     }
//   }
// })

window.addEventListener('keydown', function(event) {
  // console.log(`event`, event)
  // event.preventDefault()
  // if (isHotKey('tab')(event) || isHotKey('shift+tab')(event)) {
  //   event.preventDefault()
  // }
  // const inputEl = document.getElementById('item-input-dom-id-')
  // if (inputEl && !inputEl.hasFocus) {
  //   console.log(`'preventingDefault'`, 'preventingDefault')
  //   event.preventDefault()
  //   inputEl.focus()
  // }
})

/* ELM APP */

const mainCache = getMainCache()
const app = Elm.Main.init({
  node:
    document.querySelector('#main') || document.querySelector('body > *'),
  flags: { now: Date.now(), cache: mainCache },
})

const db = new PouchDB('http://127.0.0.1:5984/elm-ingrid-backup')
const historyDb = new PouchDB('http://127.0.0.1:5984/elm-ingrid-history')

db.info().catch(sendErrorWithTitle('PouchDB info failed'))

function canSendToPort(portName) {
  validate('S', arguments)
  return R.hasPath(['ports', portName, 'send'])(app)
}

function send(value, portName) {
  validate('*S', arguments)

  const port = app.ports[portName]
  if (canSendToPort(portName)) {
    port.send(value)
  } else {
    console.error(`Port Not Found '${portName}', msg=`, value)
  }
}

function onJsError(title, desc) {
  validate('SS', arguments)
  send([title, desc], 'onJsError')
}

// db.allDocs({ include_docs: true }).then(({ rows }) => {
//   if (rows.length === 0) {
//     db.put(itemToPouchDoc(initialRootItem))
//   } else {
//     app.ports.pouchItemsLoaded.send(rows.map(r => pouchDocToItem(r.doc)))
//   }
// })
//
// db.changes({ include_docs: true, live: true, since: 'now' })
//   .on('change', change => {
//     console.log(change)
//     if (change.deleted) {
//     } else {
//       app.ports.pouchItemChanged.send(pouchDocToItem(change.doc))
//     }
//   })
//   .on('error', error => sendErrorWithTitle('item changes error', error))
//
// app.ports.newItemDoc.subscribe(function([parent, idx]) {
//   validate('A', arguments)
//   validate('ON', [parent, idx])
//
//   const newItem = createNewItem()
//   db.put(
//     itemToPouchDoc({
//       ...parent,
//       childIds: insert(idx)(newItem.id)(parent.childIds),
//     }),
//   )
//     .then(() => db.put(itemToPouchDoc(newItem)))
//     .catch(sendErrorWithTitle)
// })

function sendErrorWithTitle(title) {
  return function(error) {
    console.error(title, error)
    onJsError(title, error.message)
  }
}

function backup(model) {
  const cAt = Date.now()
  db.put({ _id: `${cAt}`, model, cAt })
    .then(console.log)
    .catch(sendErrorWithTitle('Pouch Backup Failed'))
}

const debouncedBackup = debounce(backup, ms('5s'), {
  leading: false,
  trailing: true,
})

app.ports.toJsCache.subscribe(model => {
  setCache('elm-main', model)
  debouncedBackup(model)
  const cAt = Date.now()
  historyDb
    .put({
      _id: `${cAt}_${nanoid()}`,
      cursor: model.cursor,
      pid: model.historyId,
      cAt,
    })
    .then(console.log)
    .catch(sendErrorWithTitle('HistoryDb Error'))
})

app.ports.toJsError.subscribe(errorArgs => {
  console.error(...errorArgs)
})

// app.ports.bulkItemDocs.subscribe(bulkItemDocs)

// const debouncedBulkItemDocs = debounce(bulkItemDocs, 1000, {
//   leading: false,
//   trailing: true,
// })
//
// app.ports.debouncedBulkItemDocs.subscribe(debouncedBulkItemDocs)

/* HELPERS */

// function bulkItemDocs(items) {
//   validate('A', arguments)
//
//   console.log('bulkItemDocs: items', items)
//   if (!isEmpty(items)) {
//     const docs = items.map(itemToPouchDoc)
//
//     console.log('bulkItemDocs: docs', docs)
//     db.bulkDocs(docs)
//       .then(res => console.log('ports.bulkItemDocs res', res))
//       .catch(sendErrorWithTitle)
//   }
// }
//
// function pouchDocToItem(doc) {
//   const id = doc._id
//   const rev = doc._rev
//   return { id, rev, ...omit(['_id', '_rev'])(doc) }
// }
// function itemToPouchDoc(item) {
//   return {
//     _id: item.id,
//     _rev: item.rev,
//     ...omit(['id', 'rev'])(item),
//   }
// }

function getMainCache() {
  return R.compose(
    R.mergeDeepRight({ cursor: null, historyId: '' }),
    R.defaultTo({}),
    // R.always(null),
    getCached,
  )('elm-main')
}

if (module.hot) {
  module.hot.accept(() => window.location.reload(true))
}
