import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: { from: new Date('2018-01-01').getTime(), to: Date.now()}
});

Elm.Main.init({
  node: document.getElementById('root2'),
  // flags: { from: new Date('2018-01-01').getTime(), to: new Date('2018-12-31').getTime()}
  flags: { from: new Date('2018-01-01').getTime(), to: Date.now()}
});

Elm.Main.init({
  node: document.getElementById('root-current-year'),
  flags: { from: Date.now(), to: Date.now()}
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
