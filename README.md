# purescript-marionette

`marionette` is a small PureScript only MVC library.

## Documentation

- Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-marionette).


## Installation

```
spago install marionette
```

## About

By itself `marionette` is agnostic of how the "Control" part and the "View" part is handled. Thus for each `marionette` program you have to chose a "controller" (1) and a "renderer" (2).

1. This packages includes a couple of controllers:

    - `PureController` A simplistic controller that is completely pure and does not perform any side effects.
    - `ControlAPI` Control handler that runs in `Aff` and provides a beginner friendly API to handle state updates with effects.
    - `Monadic` The most flexible way to handle state control via a `MarionetteT` monad transformer.

2. Renderers are very environment specific and are defined in other packages:

    - [marionette-commander](https://github.com/thought2/purescript-marionette-commander) A renderer for the command line
    - [marionette-react-basic](https://github.com/thought2/purescript-marionette-react-basic) A react-basic-hooks renderer that provides a simple `useMarionette` hook.

With those options at hand you can for instance run the same state machine as CLI and in the browser. The renderer packages contain complete examples.

## FAQ

- Q: Does it have "components" with local state?

  A: No, there's only one global state. (Comparable to Elm)

- Q: How are side effects handled in state updates?

  A: If you chose the "monadic" or the "controlAPI" controller you can perform side effects mixed with asynchronous state updates in the control function. (Comparable to Halogen event handlers)

- Q: How about subscriptions for recurring effects?

  A: For now there's no special subscription mechanism implemented. However, besides state updates you can also trigger new messages from within asynchronous control handlers. As the examples show, with this you can implement subscriptions manually.

- Q: If many control handlers can run asynchronously and can even trigger new ones recursively. How about memory leaks?

  A: For the time the state machine is running you have to take care about this manually. However, all running control handlers are canceled automatically once the state machine exits.

