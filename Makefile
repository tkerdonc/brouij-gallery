OUT_FILE?=main.js

all: js

js:
	elm make src/Main.elm --output ${OUT_FILE}
