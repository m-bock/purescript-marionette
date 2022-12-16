build:
    spago build --purs-args "--stash"

build-ide:
    spago build --purs-args "--stash --json-errors"

format:
    purs-tidy format-in-place 'src/**/*.purs'