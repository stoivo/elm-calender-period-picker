// taken from https://github.com/thread/elm-web-components 2020-02-01 20:00
// adapted from https://github.com/PixelsCommander/ReactiveElements

const camelize = str => {
  // adapted from https://stackoverflow.com/questions/2970525/converting-any-string-into-camel-case#2970667
  return str
    .toLowerCase()
    .replace(/[-_]+/g, ' ')
    .replace(/[^\w\s]/g, '')
    .replace(/ (.)/g, firstChar => firstChar.toUpperCase())
    .replace(/ /g, '')
}

const getProps = el => {
  const props = {}

  for (let i = 0; i < el.attributes.length; i++) {
    const attribute = el.attributes[i]
    const name = camelize(attribute.name)
    props[name] = attribute.value
  }
  return props
}

const elmWebComponents = {
  register(
    name,
    ElmComponent,
    {
      setupPorts = () => {},
      staticFlags = {},
      onDetached = () => {},
      mapFlags = flags => flags,
      onSetupError,
      useShadowDom = false,
    } = {}
  ) {
    class ElmElement extends HTMLElement {
      connectedCallback() {
        const context = {}
        try {
          let props = Object.assign({}, getProps(this), staticFlags)
          if (Object.keys(props).length === 0) props = undefined

          if (props === undefined) props = {};
          const flags = mapFlags(props)
          context.flags = flags

          const parentDiv = useShadowDom ? this.attachShadow({mode: 'open'}) : this;

          /* a change in Elm 0.19 means that ElmComponent.init now replaces the node you give it
           * whereas in 0.18 it rendered into it. To avoid Elm therefore destroying our custom element
           * we create a div that we let Elm render into, and manually clear any pre-rendered contents.
           */
          const elmDiv = document.createElement('div')
          parentDiv.innerHTML = ''
          parentDiv.appendChild(elmDiv)

          const elmElement = ElmComponent.init({
            flags,
            node: elmDiv,
          })
          setupPorts(elmElement.ports)

          for (const port in elmElement.ports){
            console.log("autosetup of event " + port)
            elmElement.ports[port].subscribe(data => {
              console.log("auto dispatch " + port, data)
              this.dispatchEvent(new CustomEvent(port, {detail: data}))
            })
          }
          console.log(elmElement.ports);
        } catch (error) {
          if (onSetupError) {
            onSetupError(error, context)
          } else {
            console.error(
              `Error from elm-web-components registering ${name}`,
              'You can pass an `onSetupError` to handle these.',
              error
            )
          }
        }
      }

      disconnectedCallback() {
        onDetached()
      }
    }

    customElements.define(name, ElmElement)
  },
}

module.exports = elmWebComponents
