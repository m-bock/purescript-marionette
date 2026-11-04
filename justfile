build:
    spago build

build-ide:
    spago build --json-errors

format:
    purs-tidy format-in-place 'src/**/*.purs'
