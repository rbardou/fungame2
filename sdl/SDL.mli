(** SDL2 backend of Fungame2. *)

(** All functions of this module can raise this exception.

    The string argument is an error message. *)
exception Error of string

(** Close everything.

    Call this before exiting to shut down the library.
    Your program continues running after. *)
val quit: unit -> unit

module Hint:
sig
  (** Miscellaneous behavior settings. *)

  (** Enable or disable vertical synchronization.

      If set to [true], wait for vertical synchronization with the monitor
      before displaying contents to the screen. This prevents tearing
      but means that some parts of the screen may be drawn a bit later
      than they could have. Note that this does not appear to cause the program
      to do nothing while waiting.

      Return [true] in case of success, [false] if vsync could not be activated. *)
  val render_vsync: bool -> bool
end

module Pixel:
sig
  (** Pixel formats. *)

  module type FORMAT =
  sig
    (** Functions supported by all pixel formats. *)

    (** Pixel colors (non-abstract, e.g. [int32]). *)
    type repr

    (** Pixel colors (abstract, to prevent mixing formats). *)
    type t

    (** Make a pixel color from RGBA components. *)
    val make: r: int -> g: int -> b: int -> a: int -> t

    (** Get the red component of a pixel color. *)
    val r: t -> int

    (** Get the green component of a pixel color. *)
    val g: t -> int

    (** Get the blue component of a pixel color. *)
    val b: t -> int

    (** Get the alpha component of a pixel color. *)
    val a: t -> int

    (** Get the non-abstract representation of a pixel color. *)
    val encode: t -> repr

    (** Get the abstract representation of a pixel color. *)
    val decode: repr -> t
  end

  module ARGB8888: FORMAT with type repr = int32
  module RGBA8888: FORMAT with type repr = int32

  (** Pixel formats.

      - [ARGB8888]: 0xAARRGGBB
      - [RGBA8888]: 0xRRGGBBAA *)
  type _ format =
    | ARGB8888: ARGB8888.t format
    | RGBA8888: RGBA8888.t format

  (** Pixel formats with the type parameter hidden under an existential variable. *)
  type packed_format = Format: _ format -> packed_format [@@unboxed]
end

module Surface:
sig
  (** Images stored in regular memory (not hardware-accelerated).

      Surfaces can be easily read and modified, but as they are not stored in the GPU,
      they take longer to display. Also their pixel format may not match the display,
      meaning that a conversion may need to occur before they are displayed.
      For these reasons you should convert surfaces to a texture before displaying them.
      Only convert them once, not everytime you display them;
      unless you want to modify them, but if you need to modify them and display them
      often, you should probably use a renderer to render pixels directly into a texture. *)

  (** Surfaces.

      ['a] is the pixel format, e.g. [Pixel.RGBA8888.t]. *)
  type 'a t

  (** Surfaces with the pixel format hidden under an existential variable. *)
  type packed = Surface: _ t -> packed

  (** Create a new surface.

      The surface will have width [w] and height [h].

      If you choose a pixel format which is different from what the video
      driver, the format will have to be converted when you convert the surface
      to a texture. If you don't know, use [ARGB8888] (apparently the best for
      my laptop, and according to some random guy on the internet, also the best
      for most modern GPUs). Or use [Window.best_format]. *)
  val create: 'a Pixel.format -> w: int -> h: int -> 'a t

  (** Create a new surface and initialize its pixels.

      The surface will have width [w] and height [h].

      The function will be called for each pixel.
      It is given the pixel coordinates [x, y] and shall return the color of the pixel. *)
  val init: 'a Pixel.format -> w: int -> h: int -> (x: int -> y: int -> 'a) -> 'a t

  (** Create a new surface by loading an image file.

      Various file formats are supported, including PNG and SVG. *)
  val load: string -> packed

  (** Make a copy of a surface with a possibly different pixel format. *)
  val convert: 'a t -> 'b Pixel.format -> 'b t

  (** Convert the format of a surface if different.

      If the requested new format differs from the original surface format,
      convert, free the original surface, and return the copy.

      If the requested new format is the same as the original surface format,
      return the original surface. *)
  val convert_if_needed: 'a t -> 'b Pixel.format -> 'b t

  (** Free a surface.

      This is automatically called if the value is garbage collected,
      but you can call it manually if you want to reclaim memory faster.

      Calling [free] a second time has no effect. *)
  val free: _ t -> unit

  (** Get the width of a surface. *)
  val w: _ t -> int

  (** Get the height of a surface. *)
  val h: _ t -> int

  (** Surface locks, that allow to read and write pixels. *)
  type 'a lock

  (** Lock a surface, to allow reading and writing its pixels.

      If you call [lock] multiple times, you need to call [unlock] as many times
      for the surface to actually be unlocked. This allows nested locks. *)
  val lock: 'a t -> 'a lock

  (** Unlock a surface.

      The pixels can no longer be read or written: you should no longer use
      the [lock] value. *)
  val unlock: 'a t -> unit

  (** Lock a surface, call a function on the lock, and unlock the surface.

      The surface is unlocked even if the function raises an exception. *)
  val with_lock: 'a t -> ('a lock -> 'b) -> 'b

  (** Get a pixel of a locked surface. *)
  val get: 'a lock -> x: int -> y: int -> 'a

  (** Set a pixel of a locked surface. *)
  val set: 'a lock -> x: int -> y: int -> 'a -> unit

  (** Iterate on all pixel coordinates of a surface. *)
  val iter: 'a t -> (x: int -> y: int -> unit) -> unit

  (** Save a surface into a file using the BMP format. *)
  val save_bmp: 'a t -> string -> unit

  (** Save a surface into a file using the PNG format. *)
  val save_png: 'a t -> string -> unit
end

module Window:
sig
  (** Window management. *)

  (** Windows. *)
  type t

  (** Display modes.

      - [Windowed]: the window is a regular window.

      - [Fullscreen]: the window is actually the whole screen, whose
        resolution is changed to match the window size.  Switching to
        another program needs a resolution change, and if your program
        does not exit properly the resolution may stay to your program's
        settings. So this setting can make the user experience worse. But
        this can also be more efficient as it can mean a more direct
        access to the video driver.

      - [Fullscreen_desktop]: the window takes the whole screen, but
        the window size is set to the screen's resolution.
        Also known as windowed fullscreen.

      If you use [Fullscreen] mode, make sure you [free] the window
      to restore resolution. *)
  type mode = Windowed | Fullscreen | Fullscreen_desktop

  (** Window size statuses.

      The exact semantics depends on the window manager, but usually:
      - [Normal] means the window is neither minimized nor maximized;
      - [Minimized] means the window is not visible except in the taskbar;
      - [Maximized] means the window takes as much space as possible. *)
  type status = Normal | Minimized | Maximized

  (** Create a new window.

      The window must be freed with [free] before the program exits.

      - If [borderless] is [true], the window is created without borders,
        to allow you to draw them yourself.
        Default is [false].

      - [icon] can be used to set the icon of the window, usually displayed
        in the title bar and in the taskbar.

      - If [input_grabbed] is [true], the mouse is confined to the window.
        Default is [false].

      - [mode] sets whether the window is windowed, fullscreen, or windowed fullscreen.
        Default is [Windowed].

      - If [resizable] is [true], the user can resize the window.
        In some window managers this is a hint that the window can be tiled.
        Default is [false].

      - [status] sets whether the window should be created normally, minimized or maximized.
        Default is [Normal].

      - [x] and [y] define the position of the window on screen.

      - [w] and [h] define the width and height of the window.

      - [title] defines the title of the window, usually displayed in the title bar
        and the taskbar.

      Note that some window managers may ignore some parameters. *)
  val create:
    ?borderless: bool ->
    ?icon: _ Surface.t ->
    ?input_grabbed: bool ->
    ?mode: mode ->
    ?resizable: bool ->
    ?status: status ->
    ?x: int -> ?y: int ->
    w: int -> h: int ->
    title: string ->
    unit -> t

  (** Close and free a window.

      If the window mode is [Fullscreen], this restores the screen's resolution. *)
  val free: t -> unit

  (** Get the unique identifier of a window. *)
  val id: t -> int

  (** Get the dimentions (width, height) of a window. *)
  val size: t -> int * int

  (** Get the width of a window. *)
  val w: t -> int

  (** Get the height of a window. *)
  val h: t -> int

  (** Get the best supported pixel format for a window.

      Using this format for textures should result in the best performance.
      So using this format for surfaces should also help to avoid conversions. *)
  val best_format: t -> Pixel.packed_format
end

module Texture:
sig
  (** Images stored in GPU memory (hardware-accelerated). *)

  (** Static access textures cannot be locked nor rendered to. *)
  type access_static

  (** Streaming access textures can be locked but not rendered to. *)
  type access_streaming

  (** Target access textures cannot be locked but can be rendered to. *)
  type access_target

  (** Textures.

      ['a] is the access type, e.g. [access_static]. *)
  type 'a t

  (** Static access textures. *)
  type static = access_static t

  (** Streaming access textures. *)
  type streaming = access_streaming t

  (** Target access textures. *)
  type target = access_target t

  (** Coerce a streaming texture into a static access texture.

      This allows you to put the texture in a list with heterogenous access types. *)
  val streaming_as_static: streaming -> static

  (** Coerce a target texture into a static access texture.

      This allows you to put the texture in a list with heterogenous access types. *)
  val target_as_static: target -> static

  (** Note that textures are created for a given window
      and cannot be rendered on other windows. *)

  (** Create a texture that can be locked to write pixels directly. *)
  val create_streaming: Window.t -> w: int -> h: int -> streaming

  (** Create a texture that can be rendered into. *)
  val create_target: Window.t -> w: int -> h: int -> target

  (** Create a static texture from a surface.

      Note that if the pixel format of the surface is not the best format,
      a conversion will be done on the fly (the original surface is untouched).
      This means that there is an initial conversion cost to create the texture
      but that after that the texture is optimized for fast rendering. *)
  val create_from_surface: Window.t -> _ Surface.t -> static

  (** Create a static texture from a function. *)
  val init: Window.t -> 'a Pixel.format ->
    w: int -> h: int -> (x: int -> y: int -> 'a) -> static

  (** Free a texture.

      This is automatically called if the value is garbage collected,
      but you can call it manually if you want to reclaim GPU memory faster.

      Calling [free] a second time has no effect. *)
  val free: _ t -> unit

  (** Get the width of a texture. *)
  val w: _ t -> int

  (** Get the height of a texture. *)
  val h: _ t -> int
end

module Render:
sig
  (** Rendering. *)

  (** Rendering targets.

      - [Nowhere]: do not actually render anything.
        All rendering operations are ignored.
        This is the initial state; call [set_target] to change it.

      - [Window]: render into a window.
        Note that you must call [present] to actually see the results.

      - [Texture]: render into a texture.
        The texture must have the target access type. *)
  type target =
    | Nowhere
    | Window of Window.t
    | Texture of Texture.target

  (** Get the current rendering target.

      A typical use is to get the current target,
      set it to another (usually a texture),
      then switch it back to the original target.
      To do that you can also use [with_target]. *)
  val get_target: unit -> target

  (** Set the rendering target for future operations.

      This is automatically called by the first call to [Window.create]
      to set the created window as the rendering target.

      Note that this can change the current scaling factor, viewport clipping
      rectangle, and logical size. More precisely, when the target changes to
      become a texture, all of those are reset. When the target changes to
      become a window, those properties are set back to the value they had
      while this window was a target. In other words, windows store those
      properties, but textures do not. *)
  val set_target: target -> unit

  (** Set the rendering target, do something, then restore the previous target.

      Previous target is restored even if the function raises an exception. *)
  val with_target: target -> (unit -> 'a) -> 'a

  (** Get the current window.

      If the current target is [Nowhere], return [None].
      If the current target is [Window window], return [Some window].
      If the current target is [Texture texture], return [Some window]
      where [window] is the window that was used to create [texture]. *)
  val get_window: unit -> Window.t option

  (** Same as [get_window] but fail if the current target is [Nowhere].

      @raise [Error] if the current target is [Nowhere]. *)
  val get_window_exn: unit -> Window.t

  (** Create a streaming texture for the current window.

      Same as [Texture.create_streaming (get_window_exn ())]. *)
  val create_streaming_texture: w: int -> h: int -> Texture.streaming

  (** Create a target texture for the current window.

      Same as [Texture.create_target (get_window_exn ())]. *)
  val create_target_texture: w: int -> h: int -> Texture.target

  (** Create a static texture from a surface for the current window.

      Same as [Texture.create_from_surface (get_window_exn ())]. *)
  val create_texture_from_surface: _ Surface.t -> Texture.static

  (** Create a static texture from a function.

      Same as [Texture.init (get_window_exn ())]. *)
  val init_texture: 'a Pixel.format -> w: int -> h: int -> (x: int -> y: int -> 'a) ->
    Texture.static

  (** Create a static texture from an image file for the current window.

      Same as [Surface.load] followed by [create_texture_from_surface]
      followed by [Surface.free]. *)
  val load_texture: string -> Texture.static

  (** Present what you have rendered to the user.

      Rendering functions operate on a backbuffer, which is not actually displayed
      until you call [present].

      After you call [present], the contents of the backbuffer is unspecified:
      you should not assume that it contains what you previously rendered.
      It is strongly encouraged to [clear] after [present], even if you intend
      to overwrite every pixel.

      When rendering into textures, you do not need to call [present] for the target
      texture to be modified. You only need to call [present] to see modifications
      on the window. *)
  val present: unit -> unit

  (** Set the current draw color. *)
  val set_draw_color: r: int -> g: int -> b: int -> unit

  (** Clear the target by setting all pixels to the current draw color.

      This ignores current clip rect and viewport. *)
  val clear: unit -> unit

  (** Draw a pixel using the current draw color. *)
  val draw_point: x: int -> y: int -> unit

  (** Draw a line using the current draw color. *)
  val draw_line: x1: int -> y1: int -> x2: int -> y2: int -> unit

  (** Draw a rectangle using the current draw color (not filled). *)
  val draw_rect: x: int -> y: int -> w: int -> h: int -> unit

  (** Draw a rectangle using the current draw color (filled). *)
  val fill_rect: x: int -> y: int -> w: int -> h: int -> unit

  (** Render a texture.

      Textures can only be rendered on the window that were used to create them,
      or into textures that where created for the same window.

      [src_x], [src_y], [src_w] and [src_h] define the part of the texture to copy.
      Default values for [src_x] and [src_y] are [0],
      and defalut values for [src_w] and [src_h] are the remaining width and height
      of the texture after removing [src_x] and [src_y].
      In other words, by default all the texture is copied.

      [x] and [y] define where to draw the texture.
      Default values for [x] and [y] are [0] (top-left corner).

      [w] and [h] define the size of the rectangle where to draw the texture.
      Default value for [w] is [src_w] and default value for [h] is [src_h].
      In other words, by default the texture is not scaled.

      If [angle] is specified, the texture is rotated by [angle] (in degrees).
      The center of rotation is defined by [center_x] and [center_y],
      which are relative to [src_x] and [src_y].
      This center of rotation will be put at the position it would have
      if the texture was not rotated.
      Note that with a rotation, the rectangle denoted by [x], [y], [w] and [h]
      is not the exact set of pixels that are modified. The rotation of this rectangle is.
      Default value for [center_x] is [w / 2] and
      default value for [center_y] is [h / 2].

      If [vflip] is [true], the texture is flipped vertically.
      If [hflip] is [true], the texture is flipped horizontally.
      Both apply to the part of the texture defined by [src_x], [src_y], [src_w] and [src_h];
      i.e. the flip occurs on this sub-rectangle, not on the whole texture,
      and the source rectangle refers to the coordinates before the flip, not after.
      Default value for [vflip] and [hflip] is [false]. *)
  (* TODO: is [angle] clockwise or counterclockwise? *)
  val copy:
    ?src_x: int -> ?src_y: int -> ?src_w: int -> ?src_h: int ->
    ?x: int -> ?y: int -> ?w: int -> ?h: int ->
    ?angle: float -> ?center_x: int -> ?center_y: int ->
    ?vflip: bool -> ?hflip: bool ->
    _ Texture.t -> unit

  (** {2 Scaling and Clipping} *)

  (** Set the scaling factor.

      This applies a scaling factor to coordinates of all future operations
      (except [set_scale] itself and [set_logical_size]).
      This includes operations such as [set_clip_rect] and [set_viewport]. *)
  val set_scale: x: float -> y: float -> unit

  (** Set the clipping rectangle.

      This prevents drawing outside of a rectangular area.

      Note that [clear] ignores the clipping rectangle. *)
  val set_clip_rect: x: int -> y: int -> w: int -> h: int -> unit

  (** Remove the clipping rectangle. *)
  val unset_clip_rect: unit -> unit

  (** Set the viewport.

      This makes future operations operate as if rendering in the given subrectangle.
      This is similar to [set_clip_rect] except that coordinates of all future operations
      are now translated by [x, y].

      You can use both a clipping rectangle and a viewport.
      Pixels will only be modified in the intersection of the two.
      Coordinates given to [set_clip_rect] will also be relative to the viewport
      if you call [set_clip_rect] after [set_viewport], so the order in which you
      set them matters.

      Note that [clear] ignores the viewport.

      The viewport of a window is reset when the window is resized.
      This reset does not affect texture targets. *)
  val set_viewport: x: int -> y: int -> w: int -> h: int -> unit

  (** Remove the viewport. *)
  val unset_viewport: unit -> unit

  (** Set the logical size.

      This sets the scaling factor and the viewport so that rendering is done
      as if the window had size [w, h] and that the result was then scaled to fit
      the window as much as possible, adding margins to center it inside the window
      to keep aspect ratio.

      Because this sets the scaling factor and the viewport, you should probably
      avoid calling [set_scale], [set_viewport] and [unset_viewport] if you use
      [set_logical_size].

      If the window is resized, the scaling factor and the viewport are updated
      to keep the requested logical size. *)
  val set_logical_size: w: int -> h: int -> unit

  (** Read the contents of the current target.

      Can be used to read any target (window or texture), but there must be
      a current target.

      This is a rather slow process. For instance, on a rather good 2019 device it took
      about 5 milliseconds to read a 640 by 480 window.

      Do not call [present] before [read]: [read] would return unspecified pixels. *)
  val read: ?x: int -> ?y: int -> w: int -> h: int -> unit -> Surface.packed
end

module Timer:
sig
  (** Time-related functions. *)

  (** Wait for a given number of milliseconds. *)
  val delay: int -> unit

  (** Get the number of milliseconds since the program started.

      The exact starting time usually corresponds to the creation of the first window. *)
  val get_ticks: unit -> int
end

module Key_code:
sig
  (** Layout-dependent key codes. *)

  (** Key codes. *)
  type t =
    | A
    | Ac_back
    | Ac_bookmarks
    | Ac_forward
    | Ac_home
    | Ac_refresh
    | Ac_search
    | Ac_stop
    | Again
    | Alterase
    | Ampersand
    | Application
    | Asterisk
    | At
    | Audiomute
    | Audionext
    | Audioplay
    | Audioprev
    | Audiostop
    | B
    | Backquote
    | Backslash
    | Backspace
    | Brightnessdown
    | Brightnessup
    | C
    | Calculator
    | Cancel
    | Capslock
    | Caret
    | Clear
    | Clearagain
    | Colon
    | Comma
    | Computer
    | Copy
    | Crsel
    | Currencysubunit
    | Currencyunit
    | Cut
    | D
    | Decimalseparator
    | Delete
    | Displayswitch
    | Dollar
    | Down
    | E
    | Eject
    | End
    | Equals
    | Escape
    | Exclaim
    | Execute
    | Exsel
    | F
    | F1
    | F10
    | F11
    | F12
    | F13
    | F14
    | F15
    | F16
    | F17
    | F18
    | F19
    | F2
    | F20
    | F21
    | F22
    | F23
    | F24
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | Find
    | G
    | Greater
    | H
    | Hash
    | Help
    | Home
    | I
    | Insert
    | J
    | K
    | K0
    | K1
    | K2
    | K3
    | K4
    | K5
    | K6
    | K7
    | K8
    | K9
    | Kbdillumdown
    | Kbdillumtoggle
    | Kbdillumup
    | Kp_0
    | Kp_00
    | Kp_000
    | Kp_1
    | Kp_2
    | Kp_3
    | Kp_4
    | Kp_5
    | Kp_6
    | Kp_7
    | Kp_8
    | Kp_9
    | Kp_a
    | Kp_ampersand
    | Kp_at
    | Kp_b
    | Kp_backspace
    | Kp_binary
    | Kp_c
    | Kp_clear
    | Kp_clearentry
    | Kp_colon
    | Kp_comma
    | Kp_d
    | Kp_dblampersand
    | Kp_dblverticalbar
    | Kp_decimal
    | Kp_divide
    | Kp_e
    | Kp_enter
    | Kp_equals
    | Kp_equalsas400
    | Kp_exclam
    | Kp_f
    | Kp_greater
    | Kp_hash
    | Kp_hexadecimal
    | Kp_leftbrace
    | Kp_leftparen
    | Kp_less
    | Kp_memadd
    | Kp_memclear
    | Kp_memdivide
    | Kp_memmultiply
    | Kp_memrecall
    | Kp_memstore
    | Kp_memsubtract
    | Kp_minus
    | Kp_multiply
    | Kp_octal
    | Kp_percent
    | Kp_period
    | Kp_plus
    | Kp_plusminus
    | Kp_power
    | Kp_rightbrace
    | Kp_rightparen
    | Kp_space
    | Kp_tab
    | Kp_verticalbar
    | Kp_xor
    | L
    | Lalt
    | Lctrl
    | Left
    | Leftbracket
    | Leftparen
    | Less
    | Lgui
    | Lshift
    | M
    | Mail
    | Mediaselect
    | Menu
    | Minus
    | Mode
    | Mute
    | N
    | Numlockclear
    | O
    | Oper
    | Out
    | P
    | Pagedown
    | Pageup
    | Paste
    | Pause
    | Percent
    | Period
    | Plus
    | Power
    | Printscreen
    | Prior
    | Q
    | Question
    | Quote
    | Quotedbl
    | R
    | Ralt
    | Rctrl
    | Return
    | Return2
    | Rgui
    | Right
    | Rightbracket
    | Rightparen
    | Rshift
    | S
    | Scrolllock
    | Select
    | Semicolon
    | Separator
    | Slash
    | Sleep
    | Space
    | Stop
    | Sysreq
    | T
    | Tab
    | Thousandsseparator
    | U
    | Underscore
    | Undo
    | Up
    | V
    | Volumedown
    | Volumeup
    | W
    | Www
    | X
    | Y
    | Z
    | Unsupported of string

  (** Convert a key code to a string.

      The string is the OCaml representation of the value,
      For instance, [show Kp_plus] is ["Kp_plus"]. *)
  val show: t -> string
end

module Scan_code:
sig
  (** Layout-independent key codes. *)

  (** Scan codes. *)
  type t =
    | A
    | Ac_back
    | Ac_bookmarks
    | Ac_forward
    | Ac_home
    | Ac_refresh
    | Ac_search
    | Ac_stop
    | Again
    | Alterase
    | Apostrophe
    | App1
    | App2
    | Application
    | Audiomute
    | Audionext
    | Audioplay
    | Audioprev
    | Audiostop
    | B
    | Backslash
    | Backspace
    | Brightnessdown
    | Brightnessup
    | C
    | Calculator
    | Cancel
    | Capslock
    | Clear
    | Clearagain
    | Comma
    | Computer
    | Copy
    | Crsel
    | Currencysubunit
    | Currencyunit
    | Cut
    | D
    | Decimalseparator
    | Delete
    | Displayswitch
    | Down
    | E
    | Eject
    | End
    | Equals
    | Escape
    | Execute
    | Exsel
    | F
    | F1
    | F10
    | F11
    | F12
    | F13
    | F14
    | F15
    | F16
    | F17
    | F18
    | F19
    | F2
    | F20
    | F21
    | F22
    | F23
    | F24
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | Find
    | G
    | Grave
    | H
    | Help
    | Home
    | I
    | Insert
    | International1
    | International2
    | International3
    | International4
    | International5
    | International6
    | International7
    | International8
    | International9
    | J
    | K
    | K0
    | K1
    | K2
    | K3
    | K4
    | K5
    | K6
    | K7
    | K8
    | K9
    | Kbdillumdown
    | Kbdillumtoggle
    | Kbdillumup
    | Kp_0
    | Kp_00
    | Kp_000
    | Kp_1
    | Kp_2
    | Kp_3
    | Kp_4
    | Kp_5
    | Kp_6
    | Kp_7
    | Kp_8
    | Kp_9
    | Kp_a
    | Kp_ampersand
    | Kp_at
    | Kp_b
    | Kp_backspace
    | Kp_binary
    | Kp_c
    | Kp_clear
    | Kp_clearentry
    | Kp_colon
    | Kp_comma
    | Kp_d
    | Kp_dblampersand
    | Kp_dblverticalbar
    | Kp_decimal
    | Kp_divide
    | Kp_e
    | Kp_enter
    | Kp_equals
    | Kp_equalsas400
    | Kp_exclam
    | Kp_f
    | Kp_greater
    | Kp_hash
    | Kp_hexadecimal
    | Kp_leftbrace
    | Kp_leftparen
    | Kp_less
    | Kp_memadd
    | Kp_memclear
    | Kp_memdivide
    | Kp_memmultiply
    | Kp_memrecall
    | Kp_memstore
    | Kp_memsubtract
    | Kp_minus
    | Kp_multiply
    | Kp_octal
    | Kp_percent
    | Kp_period
    | Kp_plus
    | Kp_plusminus
    | Kp_power
    | Kp_rightbrace
    | Kp_rightparen
    | Kp_space
    | Kp_tab
    | Kp_verticalbar
    | Kp_xor
    | L
    | Lalt
    | Lang1
    | Lang2
    | Lang3
    | Lang4
    | Lang5
    | Lang6
    | Lang7
    | Lang8
    | Lang9
    | Lctrl
    | Left
    | Leftbracket
    | Lgui
    | Lshift
    | M
    | Mail
    | Mediaselect
    | Menu
    | Minus
    | Mode
    | Mute
    | N
    | Nonusbackslash
    | Nonushash
    | Numlockclear
    | O
    | Oper
    | Out
    | P
    | Pagedown
    | Pageup
    | Paste
    | Pause
    | Period
    | Printscreen
    | Prior
    | Q
    | R
    | Ralt
    | Rctrl
    | Return
    | Return2
    | Rgui
    | Right
    | Rightbracket
    | Rshift
    | S
    | Scrolllock
    | Select
    | Semicolon
    | Separator
    | Slash
    | Sleep
    | Space
    | Stop
    | Sysreq
    | T
    | Tab
    | Thousandsseparator
    | U
    | Undo
    | Up
    | V
    | Volumedown
    | Volumeup
    | W
    | Www
    | X
    | Y
    | Z
    | Unsupported of string

  (** Convert a scan code to a string.

      The string is the OCaml representation of the value,
      For instance, [show Kp_plus] is ["Kp_plus"]. *)
  val show: t -> string
end

module Mouse_button:
sig
  (** Mouse buttons. *)

  (** Mouse buttons. *)
  type t =
    | Left
    | Middle
    | Right
    | Button of int

  (** Convert a mouse button to a string.

      The string is the OCaml representation of the value,
      For instance, [show Left] is ["Left"]. *)
  val show: t -> string
end

module Event:
sig
  (** Events. *)

  (** Button states (pressed is down, released is up). *)
  type button_state = Pressed | Released

  (** Information about a quit event. *)
  type quit =
    {
      timestamp: int; (** When the event occurred (same unit as [Timer.get_ticks]). *)
    }

  (** Information about a keyboard event. *)
  type keyboard =
    {
      timestamp: int; (** When the event occurred (same unit as [Timer.get_ticks]). *)
      window_id: int; (** Unique identifier of the window which received the event. *)
      state: button_state; (** Whether the key was pressed or released. *)
      repeat: bool; (** Whether this is a repeat due to the key being held down. *)
      scan_code: Scan_code.t; (** Layout-agnostic key code. *)
      key_code: Key_code.t; (** Layout-dependent key code. *)
      mod_lshift: bool; (** Whether left shift was also down. *)
      mod_rshift: bool; (** Whether right shift was also down. *)
      mod_lctrl: bool; (** Whether left ctrl was also down. *)
      mod_rctrl: bool; (** Whether right ctrl was also down. *)
      mod_lalt: bool; (** Whether left alt was also down. *)
      mod_ralt: bool; (** Whether right alt was also down. *)
      mod_lgui: bool; (** Whether left GUI (usually the Windows key) was also down. *)
      mod_rgui: bool; (** Whether right GUI (usually the Windows key) was also down. *)
      mod_num: bool; (** Whether numlock was on. *)
      mod_caps: bool; (** Whether capslock was on. *)
      mod_mode: bool; (** Supposed to be AltGr, in practice AltGr is [mod_ralt]. *)
      mod_ctrl: bool; (** [mod_lctrl || mod_rctrl] *)
      mod_shift: bool; (** [mod_lshift || mod_rshift] *)
      mod_alt: bool; (** [mod_lalt || mod_ralt] *)
      mod_gui: bool; (** [mod_lgui || mod_rgui] *)
    }

  (** Information about a mouse button event. *)
  type mouse_button =
    {
      timestamp: int; (** When the event occurred (same unit as [Timer.get_ticks]). *)
      window_id: int; (** Unique identifier of the window which received the event. *)
      which: int; (** Identifier of the mouse which triggered the event. *)
      button: Mouse_button.t; (** Which button was pressed or released. *)
      state: button_state; (** Whether the button was pressed or released. *)
      clicks: int; (** 1 for single click, 2 for double-click, etc. *)
      x: int; (** X coordinate of the cursor location at the time of the event. *)
      y: int; (** Y coordinate of the cursor location at the time of the event. *)
    }

  (** Information about a mouse motion event. *)
  (* TODO: xrel and yrel are related to SDL_SetRelativeMouseMode *)
  type mouse_motion =
    {
      timestamp: int; (** When the event occurred (same unit as [Timer.get_ticks]). *)
      window_id: int; (** Unique identifier of the window which received the event. *)
      which: int; (** Identifier of the mouse which triggered the event. *)
      button_left: bool; (** Whether the left button was pressed. *)
      button_middle: bool; (** Whether the middle button was pressed. *)
      button_right: bool; (** Whether the right button was pressed. *)
      button_x1: bool; (** Whether the X1 button was pressed. *)
      button_x2: bool; (** Whether the X2 button was pressed. *)
      x: int; (** X coordinate of the cursor location at the time of the event. *)
      y: int; (** Y coordinate of the cursor location at the time of the event. *)
      xrel: int; (** Amount of movement in the X direction since the last motion event. *)
      yrel: int; (** Amount of movement in the Y direction since the last motion event. *)
    }

  (** Information about a mouse wheel event.

      In theory:
      - a movement to the left generates a negative X value;
      - a movement to the right generates a positive X value;
      - a movement down generates a negative Y value;
      - a movement up generates a positive Y value.
      In practice, it depends... *)
  type mouse_wheel =
    {
      timestamp: int; (** When the event occurred (same unit as [Timer.get_ticks]). *)
      window_id: int; (** Unique identifier of the window which received the event. *)
      which: int; (** Identifier of the mouse which triggered the event. *)
      x: int; (** Amount of movement in the X direction. *)
      y: int; (** Amount of movement in the Y direction. *)
    }

  (** Information about a window event. *)
  type window =
    {
      timestamp: int; (** When the event occurred (same unit as [Timer.get_ticks]). *)
      window_id: int; (** Unique identifier of the window which received the event. *)
    }

  (** Events.

      The [Quit] event is received in particular if the user tries to close the
      last window.

      Note: [Window_resized] is always preceded by [Window_size_changed]. *)
  type t =
    | Quit of quit (** User is trying to close the program. *)
    | Key_down of keyboard (** A key was pressed (or held long enough for a repeat). *)
    | Key_up of keyboard (** A key was released. *)
    | Mouse_button_down of mouse_button (** A mouse button was pressed. *)
    | Mouse_button_up of mouse_button (** A mouse button was released. *)
    | Mouse_motion of mouse_motion (** A mouse was moved. *)
    | Mouse_wheel of mouse_wheel (** A mouse wheel was turned. *)
    | Window_shown of window (** A window was shown. *)
    | Window_hidden of window (** A window was hidden. *)
    | Window_exposed of window (** A window was exposed and needs to be redrawn. *)
    | Window_moved of window (** A window was moved. *)
    | Window_resized of window (** A window was resized by the user. *)
    | Window_size_changed of window (** A window was resized by the user or function call. *)
    | Window_minimized of window (** A window was minimized into the taskbar. *)
    | Window_maximized of window (** A window was maximized to take the whole screen. *)
    | Window_restored of window (** A window was set to be neither minimized nor maximized. *)
    | Window_enter of window (** A mouse cursor entered a window. *)
    | Window_leave of window (** A mouse cursor leaved a window. *)
    | Window_focus_gained of window (** A window gained focus. *)
    | Window_focus_lost of window (** A window lost focus. *)
    | Window_close of window (** The window manager wants to close a window. *)
    | Window_take_focus of window (** A window is being offered a focus. *)
    | Unsupported (** An unknown event occurred. *)

  (** Convert an event to a string for debugging. *)
  val show: t -> string

  (** Get the next event.

      The event is removed from the queue.
      If there is no event yet, this returns [None]. *)
  val poll: unit -> t option

  (** Wait until the next event.

      The event is removed from the queue. *)
  val wait: unit -> t

  (** Wait until the next event (with a timeout).

      The event is removed from the queue.

      The timeout is in milliseconds.
      Return [None] in case of timeout. *)
  val wait_timeout: int -> t option
end

module Font:
sig
  (** Text rendering using fonts. *)

  (** Fonts. *)
  type t

  (** Font rendering hints. *)
  type hinting =
    | Normal (** Default. *)
    | Light
    | Mono (** Optimize for monochrome. *)
    | None

  (** Load a font from a file.

      Supported file formats are TTF and FON.

      Style attributes:
      - [bold]: whether to render the font in bold (default [false]);
      - [italic]: whether to render the font in italic (default [false]);
      - [underline]: whether to render the font underlined (default [false]);
      - [strikethrough]: whether to render the font striked through (default [false]).

      Other attributes:
      - [outline]: width of the outline to draw around glyphs, in pixels (default [0]);
      - [kerning]: whether to use kerning (adjusting space between glyphs) (default [true]);
      - [hinting]: hints to give to the font renderer;
      - [size]: font size in points, based on 72 DPI.

      If [outline] is not [0], only an outline is drawn: the inside is transparent.
      Combining [bold] and a non-zero [outline] give weird results. *)
  val load:
    ?bold: bool ->
    ?italic: bool ->
    ?underline: bool ->
    ?strikethrough: bool ->
    ?outline: int ->
    ?kerning: bool ->
    ?hinting: hinting ->
    size: int ->
    string -> t

  (** Free a font.

      This is automatically called if the value is garbage collected,
      but you can call it manually if you want to reclaim memory faster.

      Calling [free] a second time has no effect. *)
  val free: t -> unit

  (** Render text into a surface.

      Usage: [render font ~r ~g ~b ~a text]

      [text] is assumed to be UTF8-encoded.
      [r], [g], [b], [a] define the color of the text.
      If [wrap] is specified, wrap words to fit a result with width [wrap].
      Alpha-blending is used to make the background transparent
      and to anti-alias the text. *)
  val render: ?wrap: int -> t ->
    r: int -> g: int -> b: int -> a: int ->
    string -> Pixel.ARGB8888.t Surface.t

  (** Same as [render] but render into a texture.

      If [window] is not specified, the texture is created for the current
      rendering target. *)
  val render_texture: ?window: Window.t -> ?wrap: int -> t ->
    r: int -> g: int -> b: int -> a: int ->
    string -> Texture.static

  (** Same as [render_texture] but uses [Texture_cache] to cache the result. *)
  val render_cached: ?window: Window.t -> ?wrap: int -> t ->
    r: int -> g: int -> b: int -> a: int ->
    string -> Texture.static

  (** Get the width and height of some text for a given font.

      Usage: [size_text font text]

      The result [(w, h)] is the size of the surface you would obtain with [render].
      [h] is supposed to be equal to [height font], with the caveat that
      sometimes it can be one pixel larger. *)
  val size_text: t -> string -> int * int

  (** Get the maximum height of a font, in pixels.

      Note that this seems to ignore the outline width, so this is only
      completely reliable with an [outline] of 0. The outline is not ignored
      by [ascent] and [descent] however. Without an outline, [height] is supposed
      to be equal to [ascent - descent]. *)
  val height: t -> int

  (** Get the maximum height above the baseline of a font, in pixels. *)
  val ascent: t -> int

  (** Get the maximum height below the baseline of a font, in pixels.

      This is usually a negative value. It should be equal to baseline - [height]
      (see remark about outlines in the documentation of [height] though).  *)
  val descent: t -> int

  (** Get the suggested distance between two lines for a font, in pixels. *)
  val line_skip: t -> int
end

(** {2 Helper Modules} *)

(** The following modules can be implemented with the other modules of this library.
    Nevertheless they provide features that prove to be convenient when using this library. *)

module Texture_cache:
sig
  (** Cache to reuse textures between frames. *)

  (** It is often the case that one wants to render a texture and use it
      in several consecutive frames without re-rendering it.
      Once the texture is not used in a frame, it is likely not going
      to be useful for a while and freeing it is thus a good idea.
      This module helps to manage such textures.

      GUIs are a good example where this module is useful, for textures
      such as texts (which usually stay on screen for a while)
      and widget backgrounds that depend on the widget size
      (which usually does not change very often). *)

  module type S =
  sig
    (** Texture cache instances. *)

    (** Cache keys. *)
    type key

    (** Get the texture associated to a key.

        Usage: [get key make]

        If [key] is in the cache, returns the texture which is associated to it.
        Else, calls [make key] to build the texture that is to be associated to [key]
        and return this texture. Next calls to [get] on keys which are considered
        to be equal to [key] will return this texture until the key is removed
        from the cache. *)
    val get: key -> (unit -> Texture.static) -> Texture.static

    (** Remove a key from the cache and free the associated texture. *)
    val free: key -> unit

    (** Remove all keys from the cache and free all associated textures. *)
    val clear: unit -> unit

    (** Move on to the next frame.

        All keys that were not used (i.e. not returned by a call to [get])
        since the last call to [next_frame] are [free]d. *)
    val next_frame: unit -> unit
  end

  (** Make a texture cache for a given key type. *)
  module Make (Key: Map.OrderedType): S with type key = Key.t

  (** Remove all keys from all caches and free all associated textures.

      [Main_loop.run] calls this automatically at the end of the loop. *)
  val clear_all: unit -> unit

  (** Move on to the next frame for all caches.

      [Main_loop.run] calls this automatically after [draw] is called. *)
  val next_frame_all: unit -> unit

  (** {2 Main Cache} *)

  (** This module includes an instance of [Make] for an extensible type [k].
      So you can easily use this module without applying [Make] by simply
      extending [k] and using [get]. Example:
      {[
        type Texture_cache.k += Main_background

        let draw () =
          (* The main background will only be created once. *)
          let main_background =
            Texture_cache.get Main_background @@ fun _ ->
            Render.init_texture ARGB8888 ~w: 640 ~h: 480 @@ fun ~x: _ ~y: _ ->
            Pixel.ARGB8888.make ~r: (Random.int 256) ~g: 0 ~b: 0 ~a: 255
          in
          Render.copy main_background;
          ...
      ]} *)

  (** Extensible type for the main cache.

      Comparison function is [Stdlib.compare], so be careful about only
      extending this type with constructors that [Stdlib.compare] can compare. *)
  type k = ..

  include S with type key = k
end

module Main_loop:
sig
  (** Main loops. *)

  (** Run a main loop that processes events and manages time.

      [handle_event] is called when an event occurs.
      Its return value is used to decide whether to [draw].

      If [logical_delay] is specified, [logical_frame] is called every
      [logical_delay] milliseconds. The return value of [logical_frame]
      is used to decide whether to [draw].
      If [logical_delay] is not specified, [logical_frame] is never called.

      Note that the delay between two [logical_frame]s may be higher than expected.
      This can happen in particular if [logical_frame] or [draw] are not fast enough.
      This is mitigated partly by a catch-up mechanism that causes processing
      of the next logical frames to be done sooner, possibly immediately,
      until the delay is absorbed. This mechanism, however, can cause the program
      to appear to accelerate after freezes. [max_logical_catch_up_delay] defines the maximum
      delay, in milliseconds, that can be caught up with this mechanism.
      Its default value is 200. Lower values can reduce the duration of potential
      accelerations after freezes, but can cause the program to appear slower than
      it should. Note that while catching up, [draw] will not be called unless
      [max_freeze] is used.

      [draw] is called when:
      - there is something to draw, i.e.:
        - [handle_event] was called since the last [draw], and returned [true];
        - or [logical_frame] was called since the last [draw], and returned [true];
        - or [draw_delay] is specified and at least [draw_delay] milliseconds
          passed since the last [draw];
        - or the window was resized or exposed;
      - and:
        - there is no event and no logical frame to process;
        - or [max_freeze] is specified and at least [max_freeze] milliseconds
          passed since the last [draw].
      Note that [max_freeze] should be at least equal to [draw_delay] if both
      are specified. Otherwise [max_freeze] will have no effect.

      If [continue] is specified, it is called before each iteration,
      and if it returns [false], the loop stops.
      If it is not specified, the loop automatically stops when [Event.Quit] is received.

      If [window] is specified, the loop takes care of some housekeeping:
      - it calls [Render.set_target (Window window)],
        [Render.set_draw_color] (with color black),
        and [Render.clear] before [draw];
      - it calls [Render.set_target (Window window)] and [Render.present] after [draw];
      - it calls [Window.free window] and [quit] at the end.
      This is intended for cases that involve only a single window.

      Typical use cases are:
      - a GUI which only reacts to events
        (don't use [logical_delay] nor [draw_delay]);
      - a game where the state self-actualizes every [logical_delay] milliseconds
        and the screen only depends on this state (don't use [draw_delay]);
      - a game where there are animations that benefit from being drawn as often as possible
        (set [draw_delay] to a small number, such as [0]). *)
  val run:
    ?handle_event: (Event.t -> bool) ->
    ?logical_delay: int ->
    ?logical_frame: (unit -> bool) ->
    ?max_logical_catch_up_delay: int ->
    ?draw_delay: int ->
    ?draw: (unit -> unit) ->
    ?max_freeze: int ->
    ?continue: (unit -> bool) ->
    ?window: Window.t ->
    unit -> unit
end

(* TODO: sound, gamepads, widgets (?), nice demos *)
