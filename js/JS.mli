(** JavaScript backend of Fungame2. *)

(** The documentation of this module only details the differences
    from the SDL implementation. So it is advised that you read
    the documentation of the SDL version first. *)

exception Error of string

(* (\** Does nothing (provided for compatibility). *\) *)
(* val quit: unit -> unit *)

(* module Hint: *)
(* sig *)
(*   (\** Miscellaneous behavior settings. *\) *)

(*   (\** Always return false (provided for compatibility). *\) *)
(*   val render_vsync: bool -> bool *)
(* end *)

(* module Pixel: *)
(* sig *)
(*   module type FORMAT = *)
(*   sig *)
(*     type repr *)
(*     type t *)

(*     val make: r: int -> g: int -> b: int -> a: int -> t *)

(*     val r: t -> int *)
(*     val g: t -> int *)
(*     val b: t -> int *)
(*     val a: t -> int *)

(*     val encode: t -> repr *)
(*     val decode: repr -> t *)
(*   end *)

(*   module ARGB8888: FORMAT with type repr = int32 *)
(*   module RGBA8888: FORMAT with type repr = int32 *)

(*   type _ format = *)
(*     | ARGB8888: ARGB8888.t format *)
(*     | RGBA8888: RGBA8888.t format *)

(*   type packed_format = Format: _ format -> packed_format [@@unboxed] *)
(* end *)

(* module Surface: *)
(* sig *)
(*   type 'a t *)
(*   type packed = Surface: _ t -> packed *)

(*   val create: 'a Pixel.format -> w: int -> h: int -> 'a t *)
(*   val init: 'a Pixel.format -> w: int -> h: int -> (x: int -> y: int -> 'a) -> 'a t *)
(*   val load: string -> packed *)
(*   val convert: 'a t -> 'b Pixel.format -> 'b t *)
(*   val convert_if_needed: 'a t -> 'b Pixel.format -> 'b t *)
(*   val free: _ t -> unit *)
(*   val w: _ t -> int *)
(*   val h: _ t -> int *)

(*   type 'a lock *)

(*   val lock: 'a t -> 'a lock *)
(*   val unlock: 'a t -> unit *)
(*   val with_lock: 'a t -> ('a lock -> 'b) -> 'b *)
(*   val get: 'a lock -> x: int -> y: int -> 'a *)
(*   val set: 'a lock -> x: int -> y: int -> 'a -> unit *)
(*   val iter: 'a t -> (x: int -> y: int -> unit) -> unit *)
(*   val save_bmp: 'a t -> string -> unit *)
(*   val save_png: 'a t -> string -> unit *)
(* end *)

module Window:
sig
  type t

  (** Create an HTML canvas.

      This searches for a DOM node with id [fungame_window_X], where X
      is the index of the window, and appends the canvas to it
      (the first created [Window.t] is appended
      as a child of [fungame_window_0], the second one is appended
      as a child of [fungame_window_1], etc.);

      Here is a typical example of an HTML page that can host such a window:
      {[
        <html>
          <head><script type="text/javascript" src="main.js"></script></head>
          <body style="margin: 0"><div id="fungame_window_0"/></body>
        </html>
      ]}

      [resizable] defines whether the size of the canvas should be
      redimensioned automatically to take all available space,
      both on startup and when the user resizes the browser window
      (this means [w] and [h] are merely default values in case this
      mechanism does not work). *)
  val create:
    (* ?borderless: bool -> *) (* TODO whatever *)
    (* ?icon: _ Surface.t -> *) (* TODO (favicon) *)
    (* ?input_grabbed: bool -> *) (* TODO but I'm not sure if possible *)
    (* ?mode: mode -> *) (* TODO could auto-resize the canvas *)
    ?resizable: bool ->
    (* ?status: status -> *) (* TODO could auto-resize the canvas *)
    (* ?x: int -> ?y: int -> *) (* TODO could set the style *)
    w: int -> h: int ->
    (* title: string -> *) (* TODO could set the document title *)
    unit -> t

  (** Remove a canvas from its parent. *)
  val free: t -> unit

  val id: t -> int
  val size: t -> int * int
  val w: t -> int
  val h: t -> int

(*   val best_format: t -> Pixel.packed_format *)
end

(* module Texture: *)
(* sig *)
(*   type access_static *)
(*   type access_streaming *)
(*   type access_target *)

(*   type 'a t *)

(*   type static = access_static t *)
(*   type streaming = access_streaming t *)
(*   type target = access_target t *)

(*   val streaming_as_static: streaming -> static *)
(*   val target_as_static: target -> static *)
(*   val create_streaming: Window.t -> w: int -> h: int -> streaming *)
(*   val create_target: Window.t -> w: int -> h: int -> target *)
(*   val create_from_surface: Window.t -> _ Surface.t -> static *)
(*   val init: Window.t -> 'a Pixel.format -> *)
(*     w: int -> h: int -> (x: int -> y: int -> 'a) -> static *)
(*   val free: _ t -> unit *)
(*   val w: _ t -> int *)
(*   val h: _ t -> int *)
(* end *)

module Render:
sig
  type target =
    | Nowhere
    | Window of Window.t
    (* | Texture of Texture.target *) (* TODO *)

  val get_target: unit -> target
  val set_target: target -> unit
  val with_target: target -> (unit -> 'a) -> 'a
  val get_window: unit -> Window.t option
  val get_window_exn: unit -> Window.t
(*   val create_streaming_texture: w: int -> h: int -> Texture.streaming *)
(*   val create_target_texture: w: int -> h: int -> Texture.target *)
(*   val create_texture_from_surface: _ Surface.t -> Texture.static *)
(*   val init_texture: 'a Pixel.format -> w: int -> h: int -> (x: int -> y: int -> 'a) -> *)
(*     Texture.static *)
(*   val load_texture: string -> Texture.static *)

  (** Do nothing: rendering is always presented automatically. *)
  val present: unit -> unit

  val set_draw_color: r: int -> g: int -> b: int -> unit
  val clear: unit -> unit
  val draw_point: x: int -> y: int -> unit
  val draw_line: x1: int -> y1: int -> x2: int -> y2: int -> unit
  val draw_rect: x: int -> y: int -> w: int -> h: int -> unit
  val fill_rect: x: int -> y: int -> w: int -> h: int -> unit
(*   val copy: *)
(*     ?src_x: int -> ?src_y: int -> ?src_w: int -> ?src_h: int -> *)
(*     ?x: int -> ?y: int -> ?w: int -> ?h: int -> *)
(*     ?angle: float -> ?center_x: int -> ?center_y: int -> *)
(*     ?vflip: bool -> ?hflip: bool -> *)
(*     _ Texture.t -> unit *)
  val set_scale: x: float -> y: float -> unit
  val set_clip_rect: x: int -> y: int -> w: int -> h: int -> unit
  val unset_clip_rect: unit -> unit
  val set_viewport: x: int -> y: int -> w: int -> h: int -> unit
  val unset_viewport: unit -> unit
(*   val set_logical_size: w: int -> h: int -> unit *)
(*   val read: ?x: int -> ?y: int -> w: int -> h: int -> unit -> Surface.packed *)
end

module Timer:
sig
(*   val delay: int -> unit *)
  val get_ticks: unit -> int
end

module Key_code:
sig
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

  val show: t -> string
end

module Scan_code:
sig
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

  val show: t -> string
end

module Mouse_button:
sig
  type t =
    | Left
    | Middle
    | Right
    | Button of int

  val show: t -> string
end

module Event:
sig
  type button_state = Pressed | Released

(*   (\** Information about a quit event. *\) *)
(*   type quit = *)
(*     { *)
(*       timestamp: int; (\** When the event occurred (same unit as [Timer.get_ticks]). *\) *)
(*     } *)

  type keyboard =
    {
      timestamp: int;
      window_id: int;
      state: button_state;
      repeat: bool;
      scan_code: Scan_code.t;
      key_code: Key_code.t;
      mod_lshift: bool;
      mod_rshift: bool;
      mod_lctrl: bool;
      mod_rctrl: bool;
      mod_lalt: bool;
      mod_ralt: bool;
      mod_lgui: bool;
      mod_rgui: bool;
      mod_num: bool;
      mod_caps: bool;
      mod_mode: bool;
      mod_ctrl: bool;
      mod_shift: bool;
      mod_alt: bool;
      mod_gui: bool;
    }

  type mouse_button =
    {
      timestamp: int;
      window_id: int;
      which: int;
      button: Mouse_button.t;
      state: button_state;
      clicks: int;
      x: int;
      y: int;
    }

  type mouse_motion =
    {
      timestamp: int;
      window_id: int;
      which: int;
      button_left: bool;
      button_middle: bool;
      button_right: bool;
      button_x1: bool;
      button_x2: bool;
      x: int;
      y: int;
      xrel: int;
      yrel: int;
    }

  type mouse_wheel =
    {
      timestamp: int;
      window_id: int;
      which: int;
      x: int;
      y: int;
    }

(*   type window = *)
(*     { *)
(*       timestamp: int; *)
(*       window_id: int; *)
(*     } *)

  type t =
(*     | Quit of quit *)
    | Key_down of keyboard
    | Key_up of keyboard
    | Mouse_button_down of mouse_button
    | Mouse_button_up of mouse_button
    | Mouse_motion of mouse_motion
    | Mouse_wheel of mouse_wheel
(*     | Window_shown of window *)
(*     | Window_hidden of window *)
(*     | Window_exposed of window *)
(*     | Window_moved of window *)
(*     | Window_resized of window *)
(*     | Window_size_changed of window *)
(*     | Window_minimized of window *)
(*     | Window_maximized of window *)
(*     | Window_restored of window *)
(*     | Window_enter of window *)
(*     | Window_leave of window *)
(*     | Window_focus_gained of window *)
(*     | Window_focus_lost of window *)
(*     | Window_close of window *)
(*     | Window_take_focus of window *)
(*     | Unsupported *)

  val show: t -> string
  val poll: unit -> t option

(*   val wait: unit -> t *)
(*   val wait_timeout: int -> t option *)
end

(* module Texture_cache: *)
(* sig *)
(*   module type S = *)
(*   sig *)
(*     type key *)

(*     val get: key -> (unit -> Texture.static) -> Texture.static *)
(*     val free: key -> unit *)
(*     val clear: unit -> unit *)
(*     val next_frame: unit -> unit *)
(*   end *)

(*   module Make (Key: Map.OrderedType): S with type key = Key.t *)

(*   val clear_all: unit -> unit *)
(*   val next_frame_all: unit -> unit *)

(*   type k = .. *)

(*   include S with type key = k *)
(* end *)

module Main_loop:
sig
  val run:
    ?handle_event: (Event.t -> bool) ->
    ?logical_delay: int ->
    ?logical_frame: (unit -> bool) ->
    ?max_logical_catch_up_delay: int ->
    ?draw_delay: int ->
    ?draw: (unit -> unit) ->
(*     ?max_freeze: int -> *)
(*     ?continue: (unit -> bool) -> *)
(*     ?window: Window.t -> *)
    unit -> unit
end

(* module Font: *)
(* sig *)
(*   type t *)

(*   type hinting = *)
(*     | Normal *)
(*     | Light *)
(*     | Mono *)
(*     | None *)

(*   val load: *)
(*     ?bold: bool -> *)
(*     ?italic: bool -> *)
(*     ?underline: bool -> *)
(*     ?strikethrough: bool -> *)
(*     ?outline: int -> *)
(*     ?kerning: bool -> *)
(*     ?hinting: hinting -> *)
(*     size: int -> *)
(*     string -> t *)

(*   val free: t -> unit *)

(*   val render: ?wrap: int -> t -> *)
(*     r: int -> g: int -> b: int -> a: int -> *)
(*     string -> Pixel.ARGB8888.t Surface.t *)

(*   val render_texture: ?window: Window.t -> ?wrap: int -> t -> *)
(*     r: int -> g: int -> b: int -> a: int -> *)
(*     string -> Texture.static *)

(*   val render_cached: ?window: Window.t -> ?wrap: int -> t -> *)
(*     r: int -> g: int -> b: int -> a: int -> *)
(*     string -> Texture.static *)

(*   val size_text: t -> string -> int * int *)
(*   val height: t -> int *)
(*   val ascent: t -> int *)
(*   val descent: t -> int *)
(*   val line_skip: t -> int *)
(* end *)
