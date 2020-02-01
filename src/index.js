import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

import elmWebComponents from './elm-web-components'

elmWebComponents.register('elm-period-picker', Elm.Main, {
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
    return Object.assign({}, flags, { from, to });
  },
})

document
  .getElementById('w420')
  .getElementsByTagName('elm-period-picker')[0]
  .addEventListener("emitSelected", function(portData, ev) {
    console.log("fucking awsome ", portData.detail)
  }
)

document
  .getElementById('w1420')
  .getElementsByTagName('elm-period-picker')[0]
  .addEventListener("emitSelected", function(portData, ev) {
    console.log("fucking awsome ", portData.detail)
  }
)
document
  .getElementById('today')
  .getElementsByTagName('elm-period-picker')[0]
  .addEventListener("emitSelected", function(portData, ev) {
    console.log("fucking awsome ", portData.detail)
  }
)

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
