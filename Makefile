BIN=logo
gen: build
	stack exec $(BIN)
build: Main.hs Logo.hs
	stack build
