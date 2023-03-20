.PHONY: build clean-cargo clean-wasm clean run-wasm run

build: data/* knight.wat
	cargo run -p wat-datagen -- knight.wat elems.dat data/*
	wat2wasm --debug-names knight.wat -o knight.wasm

clean-cargo:
	cargo clean

clean-wasm:
	rm -f knight.wasm

clean: clean-cargo clean-wasm

run: build
	wasmtime run knight.wasm
