import * as R from 'ramda'
import validate from 'aproba'

export function getCachedOr_(thunk, key) {
  return R.when(R.isNil)(thunk)(
    JSON.parse(localStorage.getItem(key) || null),
  )
}

export function getCached(key) {
  validate('S', arguments)

  try {
    return JSON.parse(localStorage.getItem(key) || null)
  } catch (e) {
    console.error(e)
    return null
  }
}

export const setCache = R.curry(function setCache(key, jsonValue) {
  validate('S*', arguments)
  localStorage.setItem(key, JSON.stringify(jsonValue))
})

export function removeCached(cacheKey) {
  localStorage.removeItem(cacheKey)
}
