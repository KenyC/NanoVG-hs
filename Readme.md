NanoVG-hs
==================================

NanoVG-hs is small vector graphics rendering library for OpenGL. Its API is modeled after the HTML5 canvas API. It is a Hakell binding to the corresponding [NanoVG](https://github.com/memononen/nanovg) library.

## Example

A (partial) replication of a demo from the original C library.

![Screenshot](resources/screenshot.png)

## Example

Set up an OpenGL context (using e.g. [OpenGLRaw](https://hackage.haskell.org/package/OpenGLRaw) and [sdl2](https://hackage.haskell.org/package/sdl2)). Then write:

```haskell
withPath Open $ do
	roundedRect
		(V2 20 30)
		(V2 40 50)
		5
strokeColor $ fromRGB 127 25 0
stroke 
```