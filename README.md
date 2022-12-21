# purescript-marionette

`marionette` is a small PureScript only MVC library for writing CLI programs. 


## Installation

```
spago install marionette (TODO: publish to pursuit to make this work)
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
```

It's a counter that runs in the terminal, the user can count up and down by using the arrow keys. Check out the full code in the `examples` folder.



## Examples

### PureCounter

```
spago run --main Test.Examples.PureCounter
```

<img src="assets/PureCounter.gif"/>

### CountDown

```
spago run --main Test.Examples.CountDown
```

<img src="assets/CountDown.gif"/>

