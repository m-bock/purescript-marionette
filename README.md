# purescript-marionette

`marionette` is a small PureScript only MVC library for writing CLI programs. 

## Installation

```
spago install marionette
```

## Getting started

In the simplest form a `marionette` program can look like this:

```hs
type State = Int

data Msg = CountUp | CountDown

update :: Msg -> State -> State
update msg state = case msg of
  CountUp -> state + 1
  CountDown -> state - 1

view :: State -> CliSurface Msg
view count = CliSurface
  ( TextOutput $
      "Current count: " <> show count
  )
  ( KeyInput (KeyPrompt "Use up/down keys") case _ of
      { name: "up" } -> Just CountUp
      { name: "down" } -> Just CountDown
      _ -> Nothing
  )

initialState :: State
initialState = 0
```

It's a counter that runs in the terminal, the user can count up and down by using the arrow keys. Check out the full code in the `examples` folder.



## Examples

### PureCounter

[Source](https://github.com/thought2/purescript-marionette/blob/main/test/Examples/PureCounter.purs)

```
spago run --main Test.Examples.PureCounter
```

<img src="assets/PureCounter.gif"/>

### CountDown

[Source](https://github.com/thought2/purescript-marionette/blob/main/test/Examples/CountDown.purs)

```
spago run --main Test.Examples.CountDown
```

<img src="assets/CountDown.gif"/>

### WordTicker

[Source](https://github.com/thought2/purescript-marionette/blob/main/test/Examples/WordTicker.purs)

```
spago run --main Test.Examples.WordTicker
```

<img src="assets/WordTicker.gif"/>


### Snake

The snake implementation can be found in [this repo](https://github.com/thought2/purescript-marionette.snake-demo).

<img src="https://raw.githubusercontent.com/thought2/purescript-marionette.snake-demo/main/assets/demo.gif"/>

## FAQ

- Q: Does it have "components" with local state?

  A: No, there's only one global state. (Comparable to Elm)

- Q: How are side effects handled in state updates?

  A: If you chose the "monadic" or the "controlAPI" controller you can perform side effects mixed with asynchronous state updates in the control function. (Comparable to Halogen event handlers)

- Q: How about subscriptions for recurring effects?

  A: For now there's no special subscription mechanism implemented. However, besides state updates you can also trigger new messages from within asynchronous control handlers. As the examples show, with this you can implement subscriptions manually.

- Q: Does it only run in Node or can I use it in the browser, too?

  A: Currently it only runs in Node. But there are plans to provide a renderer for ReactBasic, too.

- Q: If many control handlers can run asynchronously and can even trigger new ones recursively. How about memory leaks?

  A: For the time the state machine is running you have to take care about this manually. However, all running control handlers are canceled automatically once the state machine exits.

## Inspired By

- [brick](https://hackage.haskell.org/package/brick)
  A declarative Unix terminal UI library written in Haskell
- [ink](https://github.com/vadimdemedes/ink)
  React for interactive command-line apps
  
## Documentation

- Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-marionette).