import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

import elmWebComponents from './elm-web-components'

let cssBase = require('!!to-string-loader!css-loader!./main.css')
let cssFlat = require('!!to-string-loader!css-loader!./flat.css')
let cssColorfull = require('!!to-string-loader!css-loader!./colorfull.css')
let cssDefault = require('!!to-string-loader!css-loader!./default.css')

function registerVariant(name, css) {
  elmWebComponents.register('elm-period-picker-'+name, Elm.Main, {
    shadowCss: cssBase + css,
    useShadowDom: false,
    setupPorts: ports => {
      ports.emitSelected.subscribe(function(data) {
        console.dir('demo', data);
      });
    },
    mapFlags: flags => {
      const maybeToday = (date) => {
        if (!date) return new Date();
        return new Date(date);
      };
      const from = maybeToday(flags.from).getTime();
      const to = maybeToday(flags.to).getTime();
      console.log(flags.config)
      let config;
      if (!!flags.config) {
        config = JSON.parse(flags.config)
        return Object.assign({}, flags, { from, to, config });
      } else {
        return Object.assign({}, flags, { from, to });
      }
    ;
    },
  })
}

registerVariant('flat', cssFlat);
registerVariant('coluerfull', cssColorfull);
registerVariant('default', cssDefault);

window.picker = document
  .getElementById('w420')
  .getElementsByTagName('elm-period-picker-default')[0]

picker.addEventListener("emitSelected", function(portData) {
  console.log("fucking awsome ", portData.detail)
})

// window.cals = picker.shadowRoot.querySelector('.calenders')
window.cals = picker.querySelector('.calenders')


// document
//   .getElementById('w1420')
//   .getElementsByTagName('elm-period-picker-default')[0]
//   .addEventListener("emitSelected", function(portData) {
//     console.log("fucking awsome ", portData.detail)
//   }
// )

// document
//   .getElementById('today')
//   .getElementsByTagName('elm-period-picker-default')[0]
//   .addEventListener("emitSelected", function(portData) {
//     console.log("fucking awsome ", portData.detail)
//   }
// )

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
