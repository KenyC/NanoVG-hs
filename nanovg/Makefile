
nanovg: lib/libnanovg.a


lib/libnanovg.a: build/nanovg.o build/nanovg_gl3.o build/nanovg_hs_wrapper.o
	ar rcs $@ $^

build/%.o: src/%.c
	ghc -fPIC -c $<  -o $@ -Iinclude/ -lGL -lGLEW -lSDL2

