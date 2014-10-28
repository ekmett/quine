Shader quirks
-------------

To get portable GLSL with `#include` support, to avoid cut and paste coding, I'm using `cpphs` to do things manually.

`cpphs` won't pass through directives it doesn't understand other than `#pragma`, so use 

```c
#pragma version 410 core
```

to define the version at the top of the header. `__VERSION__` is fixed to `410` as well, so if you want another you'll need to
`#undef` and `#define` it yourself

Core profile is also assumed.

Similarly use:

```c
#pragma extension extension_name : behavior
```
