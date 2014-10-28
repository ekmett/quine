Quine
=====

This is just me waxing nostalgic and throwing together some code for playing with graphics.

I reserve the right to get bored and go do something else that actually makes money.

A couple of shader toys:

![screenshot of a dodecahedron](https://raw.githubusercontent.com/ekmett/quine/master/data/dodecahedron.png)

![screenshot of chains](https://raw.githubusercontent.com/ekmett/quine/master/data/chains.png)

Build Instructions
==================

To build this you'll need my modified version of @polarina's `SDL2` bindings:

http://github.com/ekmett/sdl2.git

Usage Instructions
==================

You can start up the process with just

```
$ quine
```

Resolution Settings
-------------------

Full-screen can be accessed on a mac with `Cmd-Enter`, or you can start there with `-f`

```
$ quine -f
```

Due to a bug in SDL 2.0.3 on OSX when rendering through Cocoa, this currently has to use the "fullscreen via desktop" method
otherwise I can't let you return to the desktop.

If you want to use native fullscreen then you can get coarser resolutions that run much faster (due to the lack of upsampling) with

```
$ quine -f -n -x 1024 -y 768
```

Retina rendering supported via `-r`, but figuring out the device scaling factor is currently not, I'll guess 2.0 as it is the default, but
you can play with the ratio with `-s`.

```
$ quine -r -s 2.0
```

Profiling
---------

By default `ekg` will start on port 5616, and you can connect to monitor the rendering process.

You can run `quine -M` to also have the browser pop open to the page on startup. (On a mac)

Opening the `quine.frame` chart will give you a handy FPS meter.


Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell and #haskell-game IRC channels on irc.freenode.net.

-Edward Kmett
