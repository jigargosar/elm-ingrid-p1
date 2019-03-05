import './main.scss'
import { Elm } from './StoryBook.elm'

/* StoryBook APP */

Elm.StoryBook.init({
  node:
    document.querySelector('#main') || document.querySelector('body > *'),
  flags: {},
})
