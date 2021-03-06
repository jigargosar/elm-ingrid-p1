import { getCached, setCache } from './cache-helpers'
import './main.scss'
import { Elm } from './Main.elm'
import * as R from 'ramda'
import PouchDB from 'pouchdb-browser'
import debounce from 'lodash.debounce'
import ms from 'ms'
import validate from 'aproba'
import nanoid from 'nanoid'

import pouchdbFind from 'pouchdb-find'

PouchDB.plugin(pouchdbFind)

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

function dbGet(id, db) {
  validate('SO', arguments)
  if (!id) {
    throw new Error(`dbGet: InvalidId ${id}`)
  }
  return db.get(id)
}

const tapLog = msg => R.tap(R.partial(console.log, [msg]))

const checkCouchDbAvailability = R.pipe(
  fetch,
  R.then(R.invoker(0, 'json')),
  R.then(tapLog('CouchDb Fetch:')),
  R.otherwise(logAndsendErrorWithTitle('CouchDB Fetch Error')),
)

function cachedRedoHistoryIds() {
  return R.pipe(
    getCached,
    R.defaultTo([]),
  )('redoHistoryIds')
}

const mainCache = getMainCache()
const app = Elm.Main.init({
  node:
    document.querySelector('#main') || document.querySelector('body > *'),
  flags: { now: Date.now(), cache: mainCache },
})

const couchDbServerUrl = `http://127.0.0.1:5984`
const db = new PouchDB(`${couchDbServerUrl}/elm-ingrid-backup`)
const historyDb = new PouchDB(`${couchDbServerUrl}/elm-ingrid-history`)

checkCouchDbAvailability(couchDbServerUrl)

function initHistory() {
  const redoHistoryIds = cachedRedoHistoryIds()

  if (redoHistoryIds.length > 0) {
    dbGet(R.last(redoHistoryIds), historyDb)
      .then(tapLog('fetched last history doc'))
      .then(sendHistoryDocMsg)
      .catch(logAndsendErrorWithTitle('Init History Error'))
  }
}

function sendHistoryDocMsg(doc) {
  validate('O', arguments)
  sendMsg('history', doc.cursor)
}

initHistory()

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

function sendMsg(msg, payload) {
  validate('S*', arguments)
  send({ msg, payload }, 'fromJs')
}

function sendErrorWithTitleAndDesc(title, desc) {
  validate('SS', arguments)
  // send([title, desc], 'onJsError')
  sendMsg('error', [title, desc])
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
//   .on('error', error => logAndsendErrorWithTitle('item changes error', error))
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
//     .catch(logAndsendErrorWithTitle)
// })

function logAndsendErrorWithTitle(title) {
  return function(error) {
    console.error(title, error)
    sendErrorWithTitleAndDesc(title, error.message)
  }
}

function backup(model) {
  const cAt = Date.now()
  db.put({ _id: `${cAt}`, model, cAt })
    .then(R.tap(R.partial(console.log, ['backup put res'])))
    .catch(logAndsendErrorWithTitle('Pouch Backup Failed'))
}

const debouncedBackup = debounce(backup, ms('5s'), {
  leading: false,
  trailing: true,
})

app.ports.toJs.subscribe(({ msg, payload }) => {
  console.log(`msg,payload`, msg, payload)
  switch (msg) {
    case 'cache':
      setCache('elm-main', payload)
      debouncedBackup(payload)
      break
    case 'undo':
      undo()
      break
    case 'redo':
      redo()
      break
    case 'persistHistory':
      persistHistory(payload)
      break
    default:
      console.error('Invalid msg', msg, payload)
      sendErrorWithTitleAndDesc('Invalid msg', msg)
      break
  }
})

function undo() {
  const ids = cachedRedoHistoryIds()
  if (ids.length === 0) return

  dbGet(R.last(ids), historyDb)
    .then(R.tap(console.log))
    .then(({ pid }) => {
      if (pid) {
        dbGet(pid, historyDb)
          .then(R.tap(console.log))
          .then(R.tap(sendHistoryDocMsg))
          .then(doc => {
            setCache(
              'redoHistoryIds',
              R.append(doc._id)(cachedRedoHistoryIds()),
            )
          })
          .catch(logAndsendErrorWithTitle('HistoryDb Undo Error'))
      } else {
        console.log('reachedRootUndoState')
      }
    })
}

function redo() {
  const ids = cachedRedoHistoryIds()
  if (ids.length <= 1) return
  const newRedoHistoryIds = R.compose(R.init)(ids)

  const historyId = R.compose(
    R.defaultTo(null),
    R.last,
  )(newRedoHistoryIds)

  dbGet(historyId, historyDb)
    // historyDb
    //   .find({
    //     selector: {
    //       pid: R.compose(
    //         R.defaultTo(''),
    //         R.last,
    //       )(redoHistoryIds),
    //       limit: 1,
    //       sort: [{ _id: 'desc' }],
    //     },
    //   })
    .then(R.tap(console.log))
    .then(sendHistoryDocMsg)
    .then(() => setCache('redoHistoryIds', newRedoHistoryIds))
    .catch(logAndsendErrorWithTitle('HistoryDb Undo Error'))
}

function persistHistory(cursor) {
  const cAt = Date.now()
  const newId = `${cAt}_${nanoid()}`
  const pid = R.compose(
    R.defaultTo(null),
    R.last,
  )(cachedRedoHistoryIds())

  const newHistoryItem = {
    _id: newId,
    cursor,
    pid,
    cAt,
  }

  if (pid) {
    historyDb.get(pid).then(doc => {
      if (!R.equals(doc.cursor, cursor)) {
        return historyDb
          .put(newHistoryItem)
          .then(() => setCache('redoHistoryIds', [newId]))
          .catch(logAndsendErrorWithTitle('HistoryDb Error'))
      } else {
        sendErrorWithTitleAndDesc(
          'HistoryDb Persist',
          'Ignoring duplicate cursor history ',
        )
      }
    })
  } else {
    console.log('Creating first history item')
    historyDb
      .put(newHistoryItem)
      .then(R.tap(console.log))
      .then(() => setCache('redoHistoryIds', [newId]))
      .catch(logAndsendErrorWithTitle('HistoryDb Error'))
  }
}

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
//       .catch(logAndsendErrorWithTitle)
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
    R.mergeDeepRight({ cursor: null }),
    R.defaultTo({}),
    // R.always(null),
    getCached,
  )('elm-main')
}

if (module.hot) {
  module.hot.accept(() => window.location.reload(true))
}
