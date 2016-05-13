# chicken-sdl2-image

[CHICKEN Scheme](http://call-cc.org/) bindings to
[SDL_image](http://www.libsdl.org/projects/SDL_image/) 2.

- Version:     0.1.0 (2015-12-19)
- Project:     https://gitlab.com/chicken-sdl2/chicken-sdl2-image
- Issues:      https://gitlab.com/chicken-sdl2/chicken-sdl2-image/issues
- API Docs:    http://api.call-cc.org/doc/sdl2-image
- License:     [BSD 2-Clause](LICENSE-BSD.txt)
- Maintainer:  John Croisant (john+chicken at croisant dot net)


## Synopsis

chicken-sdl2-image provides bindings to the SDL_image image loading
library version 2. chicken-sdl2-image is built to be compatible with
[chicken-sdl2](https://gitlab.com/chicken-sdl2/chicken-sdl2), which
provides bindings to Simple DirectMedia Layer (SDL) version 2, a
popular library used in games and other media-rich software.


## Installation

This project is young, so these instructions are still quite sparse.
If you run into trouble installing chicken-sdl2-image, please
[file a support request on the chicken-sdl2 project](https://gitlab.com/chicken-sdl2/chicken-sdl2/blob/master/CONTRIBUTING.md#filing-support-requests) so
we can help you, and so we can improve the install process and
instructions for future users.

### Dependencies

- [chicken-sdl2](https://gitlab.com/chicken-sdl2/chicken-sdl2)
  (the `sdl2` egg)
- [SDL_image](http://www.libsdl.org/projects/SDL_image/)
  2.0 or higher

chicken-sdl2-image requires CHICKEN Scheme 4.8 or higher. Please file
an issue or contact the maintainer if you need to use this library
with an earlier version of CHICKEN Scheme.

### Installing from egg repository

chicken-sdl2-image has not yet been submitted to the egg repository.
For now, you must install from source.

### Installing from source

To install chicken-sdl2-image, clone the repository or download the
source from the project page, then run this command from within the
project's directory (notice these are back ticks, not quotes):

```
SDL2_FLAGS=`sdl2-config --cflags --libs` chicken-install
```

If you do not have the sdl2-config helper program installed on your
computer, you may manually specify SDL-related compiler flags (notice
these are double quotes, not back ticks):

```
SDL2_FLAGS="-I/usr/local/include/SDL2 -L/usr/local/lib -lSDL2" chicken-install
```

By default, chicken-sdl2-image will be linked against SDL_image using
the compiler flag `-lSDL2_image`. You can override this by setting the
`SDL2_IMAGE_FLAGS` environment variable, if needed. You can also use
that environment variable in case you have installed SDL_image in a
different location than SDL.


## Examples

After you have installed chicken-sdl2-image, you can try compiling and
running chicken-sdl2 examples.

The [chicken-sdl2-examples repository](https://gitlab.com/chicken-sdl2/chicken-sdl2-examples)
contains complete example games and programs made with chicken-sdl2
and related libraries.


## Contributing

chicken-sdl2-image is part of the
[chicken-sdl2 project](https://gitlab.com/chicken-sdl2/chicken-sdl2)

chicken-sdl2 is a volunteer effort, and your help is appreciated.
There are many ways to get involved in the project, whether you are an
experienced programmer or not. For more information about how you can
help, please see the chicken-sdl2
[contribution guide](https://gitlab.com/chicken-sdl2/chicken-sdl2/blob/master/CONTRIBUTING.md).

Please be aware that all project participants are expected to abide by
the [Contributor Code of Conduct](https://gitlab.com/chicken-sdl2/chicken-sdl2/blob/master/CODE_OF_CONDUCT.md).
We are committed to making participation in this project a welcoming
and harassment-free experience.
