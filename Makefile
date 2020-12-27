build:
	elm make src/Main.elm --output=../chess-web/app/index.html

watch:
	elm reactor

test:
	elm-test

# CHECK OUT
# https://github.com/rhyskeepence/elm-websocket/blob/master/Makefile
browser-sync:
	browser-sync start --proxy "localhost:8000" --ws --files "example/assets"
