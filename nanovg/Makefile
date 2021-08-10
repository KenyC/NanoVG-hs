
nanovg: lib/libnanovg.a


lib/libnanovg.a: build/nanovg.o build/nanovg_gl3.o build/nanovg_hs_wrapper.o
	ar rcs $@ $^

build/%.o: src/%.c
	ghc -fPIC -c $<  -o $@ -Iinclude/ -lGL -lGLEW -lSDL2

# build/libnanovg.o: src/nanovg.c src/nanovg_gl3.c 
# 	ghc -fPIC -c src/nanovg.c  -o $@ -Iinclude/ -lGL -lGLEW -lSDL2
# 	ghc -fPIC -c src/nanovg.c -c src/nanovg_gl3.c  -o $@ -Iinclude/ -lGL -lGLEW -lSDL2
# 	ghc -fPIC -c $^ -o $@ -Iinclude/ -lGL -lGLEW -lSDL2

testlib: lib/libtesty.a

lib/libtesty.a: build/libtesty.o
	ar rcs $@ $<

build/libtesty.o: src/testy.c
	ghc -fPIC -c $^ -o $@ -Iinclude/ -lGL -lGLEW -lSDL2