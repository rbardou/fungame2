# Fungame2

This library is incomplete, and I don't know if I'll resume working on it soon,
so you should probably not use it.

This library is a wrapper around `tsdl` (thin bindings for SDL2) and `tsdl-image`.
It is higher-level than `tsdl`:
- its interface is closer to what OCaml developers are used to
  (e.g. flags are optional labeled arguments of functions that use them,
  events are records);
- it adds a few type constraints (e.g. to prevent mis-using pixel formats,
  or to prevent trying to modify a surface without locking it);
- it automatically frees surfaces and destroys textures when the OCaml value is
  garbage collected (but you can of course trigger that yourself sooner);
- it automatically initializes and closes SDL for you;
- it provides some useful helpers like a module to cache textures to avoid recomputing
  them every frames.

It is however still low-level enough to give control over important details,
in particular those that impact performance. Most of the abstractions that can be done
have already been done by SDL itself. Abstracting more would mean making opinionated
choices that would prevent some use cases. For instance, one could imagine merging
surfaces and textures into a single concept. But textures are hardware-accelerated,
while surfaces are not, and surfaces can be accessed directly, while textures do not
(at least not as efficiently). It is thus important to keep the two concepts separated.
Having only textures may be enough for some simple games, but not for more complex
applications.

Another important distinction with `tsdl` is that this library attempts to be
self-sufficient in terms of documentation. In other words, you should be able to
use it and avoid performance programming errors that would cause performance issues
without having to read the SDL documentation.

Finally, there is an attempt at a JavaScript backend.
The interface abstracts SDL and `tsdl` completely, which means that one can switch
to a different backend. The JavaScript version is very incomplete though.
For now you can read mouse, keyboard and window events, and draw rectangles.
Transformations are supported. But surfaces and textures are not.

Still missing from the SDL backend:
- sound
- joysticks (including gamepads)
