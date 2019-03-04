// noinspection JSUnresolvedVariable
import { getCached, setCache } from './cache-helpers'
import * as R from 'ramda'
import {
  compose,
  defaultTo,
  insert,
  isEmpty,
  mergeDeepRight,
  omit,
} from 'ramda'
import './main.scss'
import { Elm } from './Main.elm'
import PouchDb from 'pouchdb-browser'
import validate from 'aproba'
import nanoid from 'nanoid'
import faker from 'faker'
import isHotKey from 'is-hotkey'

/* ITEM */
const rootItemId = 'i_root_item_id'
const initialRootItem = {
  id: rootItemId,
  rev: null,
  title: 'Root Item',
  pid: null,
  childIds: [],
}

function createNewItem() {
  return {
    id: 'i_' + nanoid(),
    rev: null,
    title: faker.lorem.words(),
    pid: rootItemId,
    childIds: [],
  }
}

/* PRE ELM APP INIT */

function focusOffset(event, offset) {
  const focusable = Array.from(
    document.querySelectorAll('[data-is-focusable=true]'),
  )
  const idx = focusable.indexOf(event.target)
  R.compose(
    R.unless(R.isNil)(R.invoker(0, 'focus')),
    R.defaultTo(focusable[0]),
    R.nth(idx + offset),
  )(focusable)
}

window.addEventListener('keydown', function(event) {
  const isFocusable = !!event.target.dataset.isFocusable
  console.log('isFocusable', isFocusable)
  if (isFocusable) {
    // console.debug(idx, focusable)
    if (isHotKey('up')(event)) {
      focusOffset(event, -1)
    } else if (isHotKey('down')(event)) {
      focusOffset(event, 1)
    }
  }
})

/* ELM APP */

const app = Elm.Main.init({
  node:
    document.querySelector('#main') || document.querySelector('body > *'),
  flags: getMainCache(),
})

const db = new PouchDb('items')

db.allDocs({ include_docs: true }).then(({ rows }) => {
  if (rows.length === 0) {
    db.put(itemToPouchDoc(initialRootItem))
  } else {
    app.ports.pouchItemsLoaded.send(rows.map(r => pouchDocToItem(r.doc)))
  }
})

db.changes({ include_docs: true, live: true, since: 'now' })
  .on('change', change => {
    console.log(change)
    if (change.deleted) {
    } else {
      app.ports.pouchItemChanged.send(pouchDocToItem(change.doc))
    }
  })
  .on('error', error => console.error('item changes error', error))

app.ports.newItemDoc.subscribe(function([parent, idx]) {
  validate('A', arguments)
  validate('ON', [parent, idx])

  const newItem = createNewItem()
  db.put(
    itemToPouchDoc({
      ...parent,
      childIds: insert(idx)(newItem.id)(parent.childIds),
    }),
  )
    .then(() => db.put(itemToPouchDoc(newItem)))
    .catch(console.error)
})

app.ports.toJsCache.subscribe(model => {
  setCache('elm-main', model)
})

app.ports.bulkItemDocs.subscribe(bulkItemDocs)

// const debouncedBulkItemDocs = debounce(bulkItemDocs, 1000, {
//   leading: false,
//   trailing: true,
// })
//
// app.ports.debouncedBulkItemDocs.subscribe(debouncedBulkItemDocs)

/* HELPERS */

function bulkItemDocs(items) {
  validate('A', arguments)

  console.log('bulkItemDocs: items', items)
  if (!isEmpty(items)) {
    const docs = items.map(itemToPouchDoc)

    console.log('bulkItemDocs: docs', docs)
    db.bulkDocs(docs)
      .then(res => console.log('ports.bulkItemDocs res', res))
      .catch(console.error)
  }
}

function pouchDocToItem(doc) {
  const id = doc._id
  const rev = doc._rev
  return { id, rev, ...omit(['_id', '_rev'])(doc) }
}
function itemToPouchDoc(item) {
  return {
    _id: item.id,
    _rev: item.rev,
    ...omit(['id', 'rev'])(item),
  }
}

function getMainCache() {
  return compose(
    mergeDeepRight({ items: [], maybeFocusedItemId: null }),
    defaultTo({}),
    // always(null),
    getCached,
  )('elm-main')
}

if (module.hot) {
  module.hot.accept(() => window.location.reload(true))
}
