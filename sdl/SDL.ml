open Tsdl

module Int_map = Map.Make (Int)

exception Error of string

let () =
  Printexc.register_printer @@ function
  | Error msg -> Some ("Fungame2.SDL error: " ^ msg)
  | _ -> None

let or_raise: 'a Sdl.result -> 'a = function
  | Ok x -> x
  | Error (`Msg msg) -> raise (Error msg)

module Hint =
struct
  let setb hint value =
    Sdl.set_hint hint (if value then "1" else "0")

  let render_vsync value =
    setb Sdl.Hint.render_vsync value
end

module Pixel =
struct
  module type FORMAT =
  sig
    type repr
    type t
    val make: r: int -> g: int -> b: int -> a: int -> t
    val r: t -> int
    val g: t -> int
    val b: t -> int
    val a: t -> int
    val encode: t -> repr
    val decode: repr -> t
  end

  let make_int32 a b c d =
    Int32.logor (Int32.shift_left (Int32.of_int a) 24) @@
    Int32.logor (Int32.shift_left (Int32.of_int b) 16) @@
    Int32.logor (Int32.shift_left (Int32.of_int c) 8) @@
    Int32.of_int d

  let get_int32_1 x = Int32.shift_right_logical x 24 |> Int32.to_int
  let get_int32_2 x = Int32.shift_right_logical x 16 |> Int32.logand 0xFFl |> Int32.to_int
  let get_int32_3 x = Int32.shift_right_logical x 8 |> Int32.logand 0xFFl |> Int32.to_int
  let get_int32_4 x = x |> Int32.logand 0xFFl |> Int32.to_int

  module ARGB8888 =
  struct
    type repr = int32
    type t = repr
    let make ~r ~g ~b ~a = make_int32 a r g b
    let r = get_int32_2
    let g = get_int32_3
    let b = get_int32_4
    let a = get_int32_1
    let encode = Fun.id
    let decode = Fun.id
  end

  module RGBA8888 =
  struct
    type repr = int32
    type t = repr
    let make ~r ~g ~b ~a = make_int32 r g b a
    let r = get_int32_1
    let g = get_int32_2
    let b = get_int32_3
    let a = get_int32_4
    let encode = Fun.id
    let decode = Fun.id
  end

  type _ format =
    | ARGB8888: ARGB8888.t format
    | RGBA8888: RGBA8888.t format

  type packed_format = Format: _ format -> packed_format [@@unboxed]

  let sdl_format_of_format (type a) (format: a format) =
    match format with
      | ARGB8888 -> Sdl.Pixel.format_argb8888
      | RGBA8888 -> Sdl.Pixel.format_rgba8888

  let format_of_sdl_format format =
    if Sdl.Pixel.(eq format_argb8888) format then Some (Format ARGB8888) else
    if Sdl.Pixel.(eq format_rgba8888) format then Some (Format RGBA8888) else
      None

  type _ size =
    | Int32: int32 size

  let size_of_format (type a): a format -> a size = function
    | ARGB8888 -> Int32
    | RGBA8888 -> Int32
end

module Surface =
struct
  type 'a t =
    {
      format: 'a Pixel.format;
      w: int;
      h: int;
      pitch: int;
      sdl_surface: Sdl.surface;
      mutable alive: bool;
    }

  type packed = Surface: _ t -> packed

  let free surface =
    if surface.alive then (
      Sdl.free_surface surface.sdl_surface;
      surface.alive <- false;
    )

  let w x = x.w
  let h x = x.h

  let create_from_sdl_surface format sdl_surface =
    let w, h = Sdl.get_surface_size sdl_surface in
    let pitch = Sdl.get_surface_pitch sdl_surface in
    let surface =
      {
        format;
        w;
        h;
        pitch;
        alive = true;
        sdl_surface;
      }
    in
    Gc.finalise free surface;
    surface

  let create (type a) (format: a Pixel.format) ~w ~h =
    Sdl.create_rgb_surface_with_format ~w ~h ~depth: 32 (Pixel.sdl_format_of_format format)
    |> or_raise
    |> create_from_sdl_surface format

  let load filename =
    let surface = Tsdl_image.Image.load filename |> or_raise in
    let format =
      Pixel.format_of_sdl_format (Sdl.get_surface_format_enum surface)
    in
    match format with
      | Some (Format format) ->
          Surface (create_from_sdl_surface format surface)
      | None ->
          (* Unsupported format, convert to a supported format. *)
          let converted_surface =
            Sdl.convert_surface_format surface Sdl.Pixel.format_argb8888 |> or_raise
          in
          Sdl.free_surface surface;
          Surface (create_from_sdl_surface Pixel.ARGB8888 converted_surface)

  let convert (type a) (type b) (surface: a t) (format: b Pixel.format): b t =
    Sdl.convert_surface_format surface.sdl_surface (Pixel.sdl_format_of_format format)
    |> or_raise
    |> create_from_sdl_surface format

  let convert_if_needed (type a) (type b) (surface: a t) (format: b Pixel.format): b t =
    match surface.format, format with
      | ARGB8888, ARGB8888 -> surface
      | RGBA8888, RGBA8888 -> surface
      | ARGB8888, _
      | RGBA8888, _ ->
          let converted_surface = convert surface format in
          free surface;
          converted_surface

  type 'a lock =
    {
      get: x: int -> y: int -> 'a;
      set: x: int -> y: int -> 'a -> unit;
    }

  let lock (type a) (surface: a t): a lock =
    Sdl.lock_surface surface.sdl_surface |> or_raise;
    match Pixel.size_of_format surface.format with
      | Int32 ->
          let pitch = surface.pitch / 4 in
          let pixels = Sdl.get_surface_pixels surface.sdl_surface Bigarray.Int32 in
          {
            get = (fun ~x ~y -> Bigarray.Array1.get pixels (x + y * pitch));
            set = (fun ~x ~y p -> Bigarray.Array1.set pixels (x + y * pitch) p);
          }

  let unlock surface =
    Sdl.unlock_surface surface.sdl_surface

  let with_lock surface f =
    let lock = lock surface in
    match f lock with
      | exception exn ->
          unlock surface;
          raise exn
      | x ->
          unlock surface;
          x

  let get lock = lock.get
  let set lock = lock.set

  let iter { w; h; _ } f =
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        f ~x ~y
      done
    done

  let init format ~w ~h f =
    let surface = create format ~w ~h in
    with_lock surface @@ fun lock ->
    iter surface (fun ~x ~y -> set lock ~x ~y (f ~x ~y));
    surface

  (* TODO: blit and blit_scaled, but according to the doc, it's tricky *)

  let save_bmp surface filename =
    Sdl.save_bmp surface.sdl_surface filename |> or_raise

  let save_png surface filename =
    let result = Tsdl_image.Image.save_png surface.sdl_surface filename in
    if result <> 0 then raise (Error ("failed to write " ^ filename));
    ()
end

module Window =
struct
  type t =
    {
      window: Sdl.window;
      renderer: Sdl.renderer;
      best_format: Pixel.packed_format;
    }

  type mode =
    | Windowed
    | Fullscreen
    | Fullscreen_desktop

  type status =
    | Normal
    | Minimized
    | Maximized

  let first_window = ref true
  let on_create_first_window = ref (fun _ -> ())

  let create
      ?(borderless = false)
      ?icon
      ?(input_grabbed = false)
      ?(mode = Windowed)
      ?(resizable = false)
      ?(status = Normal)
      ?x ?y ~w ~h ~title
      () =
    (* Note: if we create several windows, the system will be initialized several times,
       which is supported by SDL. Also, initializing video also initializes events. *)
    Sdl.(init_sub_system Init.video) |> or_raise;
    let flags =
      match mode with
        | Windowed ->
            (* The [windowed] flag is actually not a flag, it is [0],
               so we use it to start from somewhere. *)
            Sdl.Window.windowed
        | Fullscreen ->
            Sdl.Window.fullscreen
        | Fullscreen_desktop ->
            Sdl.Window.fullscreen_desktop
    in
    let flags = if borderless then Sdl.Window.((+) borderless) flags else flags in
    let flags = if input_grabbed then Sdl.Window.((+) input_grabbed) flags else flags in
    let flags = if resizable then Sdl.Window.((+) resizable) flags else flags in
    let flags =
      match status with
        | Normal -> flags
        | Minimized -> Sdl.Window.((+) minimized) flags
        | Maximized -> Sdl.Window.((+) maximized) flags
    in
    (* Note: adding the [opengl] flag does not seem to make a difference when
       drawing 100000 times the same texture. It looks like this flag is used
       not to activate hardware acceleration but to create an OpenGL surface,
       to allow using OpenGL directly (e.g. to render triangles and use shaders). *)
    let window = Sdl.create_window title ?x ?y ~w ~h flags |> or_raise in
    (
      match icon with
        | None -> ()
        | Some icon -> Sdl.set_window_icon window icon.Surface.sdl_surface
    );
    (* Apparently there should be at most 1 renderer per window,
       and having 0 is only used when used with OpenGL etc.
       For now we don't support OpenGL etc. so we always make a renderer.
       When we add OpenGL etc. we can have a new argument ?render_mode
       of type Default | OpenGL | Vulkan | ..., for instance. *)
    let renderer = Sdl.create_renderer window |> or_raise in
    let best_format =
      let info = Sdl.get_renderer_info renderer |> or_raise in
      let rec find_supported = function
        | [] ->
            (* We can't find a format we support, use a default value
               and hope for the best. *)
            Pixel.Format ARGB8888
        | head :: tail ->
            match Pixel.format_of_sdl_format head with
              | Some format ->
                  format
              | None ->
                  find_supported tail
      in
      (* Usually the best format is apparently the first value in the list. *)
      find_supported info.ri_texture_formats
    in
    let window =
      {
        window;
        renderer;
        best_format;
      }
    in
    if !first_window then (
      first_window := false;
      !on_create_first_window window;
    );
    window

  let free { window; renderer; _ } =
    (* Note: if there are other windows which have not been destroyed yet,
       since we called [init_sub_system] for all of them, SDL will not actually
       quit the subsystem, because it counts the number of initializations. *)
    Sdl.(quit_sub_system Init.video);
    Sdl.destroy_renderer renderer;
    Sdl.destroy_window window

  let id { window; _ } =
    Sdl.get_window_id window

  let size { window; _ } =
    Sdl.get_window_size window

  let w window =
    size window |> fst

  let h window =
    size window |> snd

  let best_format x = x.best_format
end

module Texture =
struct
  type access_static = unit
  type access_streaming = unit
  type access_target = unit

  type _ t =
    {
      w: int;
      h: int;
      window: Window.t;
      sdl_texture: Sdl.texture;
      mutable alive: bool;
    }

  type static = access_static t
  type streaming = access_streaming t
  type target = access_target t

  let streaming_as_static (texture: streaming) = (texture: static)
  let target_as_static (texture: target) = (texture: static)

  let free texture =
    if texture.alive then (
      Sdl.destroy_texture texture.sdl_texture;
      texture.alive <- false;
    )

  let create_from_sdl_texture window sdl_texture =
    let _, _, (w, h) = Sdl.query_texture sdl_texture |> or_raise in
    let texture =
      {
        w;
        h;
        window;
        sdl_texture;
        alive = true;
      }
    in
    Gc.finalise free texture;
    texture

  let create access (window: Window.t) ~w ~h =
    let Format format = window.best_format in
    Sdl.create_texture window.renderer (Pixel.sdl_format_of_format format) access ~w ~h
    |> or_raise
    |> create_from_sdl_texture window

  let create_streaming = create Sdl.Texture.access_streaming
  let create_target = create Sdl.Texture.access_target

  let create_from_surface (window: Window.t) (surface: _ Surface.t) =
    (* A quick test showed that the texture format was the best format
       of the renderer, not the surface format, so the format appears to be
       converted on the fly and we don't need to convert the surface ourself.
       Looking at the SDL source code confirms this. *)
    Sdl.create_texture_from_surface window.renderer surface.sdl_surface
    |> or_raise
    |> create_from_sdl_texture window

  (* TODO: would it be more efficient to render the texture with draw_point?
     In which case the return type would be target.
     And we could probably get rid of the format argument.
     Or lock_texture would probably be the best. *)
  let init window format ~w ~h f =
    let surface = Surface.init format ~w ~h f in
    let texture = create_from_surface window surface in
    Surface.free surface;
    texture

  let w x = x.w
  let h x = x.h
end

module Render =
struct
  type target =
    | Nowhere
    | Window of Window.t
    | Texture of Texture.target

  let target = ref Nowhere

  (* The renderer of [target]. *)
  let renderer = ref None

  let get_target () = !target

  let set_target new_target =
    target := new_target;
    match new_target with
      | Nowhere ->
          renderer := None
      | Window window ->
          let window_renderer = window.renderer in
          renderer := Some window_renderer;
          Sdl.set_render_target window_renderer None |> or_raise
      | Texture texture ->
          let texture_renderer = texture.window.renderer in
          renderer := Some texture_renderer;
          Sdl.set_render_target texture_renderer (Some texture.sdl_texture) |> or_raise

  let () =
    Window.on_create_first_window := (fun window -> set_target (Window window))

  let with_target new_target f =
    let old_target = get_target () in
    set_target new_target;
    match f () with
      | exception exn ->
          set_target old_target;
          raise exn
      | x ->
          set_target old_target;
          x

  let with_renderer f =
    match !renderer with
      | None -> ()
      | Some renderer -> f renderer

  let get_window () =
    match !target with
      | Nowhere ->
          None
      | Window window ->
          Some window
      | Texture texture ->
          Some texture.window

  let get_window_exn () =
    match get_window () with
      | None ->
          raise (Error "no current rendering target (tip: create a window first)" )
      | Some window ->
          window

  let create_streaming_texture ~w ~h =
    Texture.create_streaming (get_window_exn ()) ~w ~h

  let create_target_texture ~w ~h =
    Texture.create_target (get_window_exn ()) ~w ~h

  let create_texture_from_surface surface =
    Texture.create_from_surface (get_window_exn ()) surface

  let init_texture format ~w ~h f =
    Texture.init (get_window_exn ()) format ~w ~h f

  let load_texture filename =
    let Surface surface = Surface.load filename in
    let texture = create_texture_from_surface surface in
    Surface.free surface;
    texture

  let present () =
    with_renderer Sdl.render_present

  (* TODO: test if we can set blend to use alpha channel.
     If not, have ~r ~g ~b arguments.
     If yes, have ~r ~g ~b ~a arguments. *)
  let set_draw_color ~r ~g ~b =
    with_renderer @@ fun renderer ->
    Sdl.set_render_draw_color renderer r g b 0xFF |> or_raise

  let clear () =
    with_renderer @@ fun renderer ->
    Sdl.render_clear renderer |> or_raise

  let draw_point ~x ~y =
    with_renderer @@ fun renderer ->
    Sdl.render_draw_point renderer x y |> or_raise

  let draw_line ~x1 ~y1 ~x2 ~y2 =
    with_renderer @@ fun renderer ->
    Sdl.render_draw_line renderer x1 y1 x2 y2 |> or_raise

  let draw_rect ~x ~y ~w ~h =
    with_renderer @@ fun renderer ->
    let rect = Sdl.Rect.create ~x ~y ~w ~h in
    Sdl.render_draw_rect renderer (Some rect) |> or_raise

  let fill_rect ~x ~y ~w ~h =
    with_renderer @@ fun renderer ->
    let rect = Sdl.Rect.create ~x ~y ~w ~h in
    Sdl.render_fill_rect renderer (Some rect) |> or_raise

  let make_flip ~vflip ~hflip =
    match vflip, hflip with
      | false, false -> Sdl.Flip.none
      | true, false -> Sdl.Flip.vertical
      | false, true -> Sdl.Flip.horizontal
      | true, true -> Sdl.Flip.(vertical + horizontal)

  (* Out of curiosity, I micro-benched the cost that it took to have
     so many optional labels instead of taking rects as input.
     It appears that for 100_000 images we have an overhead of 1ms
     on my machine. I think the convenience is worth it, although the
     micro-bench maybe was completely wrong, as all micro-benchs.
     The cost of testing for ?angle, ?vflip and ?hflip was low compared to
     the cost of giving default values to the rects.

     Interestingly, it also looks like the time to [copy] N times the same
     texture is not linear in N. In my benchmarks, it was linear until about
     N = 12500, for which it took about 40ms (about 300k copy per second).
     After that, it quickly ramped up, taking 1.45s for N = 100000
     (about 70k copy per second). *)
  let copy
      ?(src_x = 0) ?(src_y = 0) ?src_w ?src_h
      ?(x = 0) ?(y = 0) ?w ?h
      ?angle ?center_x ?center_y ?(vflip = false) ?(hflip = false)
      (texture: _ Texture.t) =
    with_renderer @@ fun renderer ->
    let src_w = match src_w with Some w -> w | None -> texture.w - src_x in
    let src_h = match src_h with Some h -> h | None -> texture.h - src_y in
    let w = match w with Some w -> w | None -> src_w in
    let h = match h with Some h -> h | None -> src_h in
    let src = Sdl.Rect.create ~x: src_x ~y: src_y ~w: src_w ~h: src_h in
    let dst = Sdl.Rect.create ~x ~y ~w ~h in
    match angle, vflip, hflip with
      | None, false, false ->
          Sdl.render_copy ~src ~dst renderer texture.sdl_texture
          |> or_raise
      | None, _, _ ->
          let flip = make_flip ~vflip ~hflip in
          Sdl.render_copy_ex ~src ~dst renderer texture.sdl_texture
            0. None flip
          |> or_raise
      | Some angle, _, _ ->
          let center =
            let x = match center_x with Some x -> x | None -> src_w / 2 in
            let y = match center_y with Some y -> y | None -> src_h / 2 in
            Sdl.Point.create ~x ~y
          in
          let flip = make_flip ~vflip ~hflip in
          Sdl.render_copy_ex ~src ~dst renderer texture.sdl_texture
            angle (Some center) flip
          |> or_raise

  let set_scale ~x ~y =
    with_renderer @@ fun renderer ->
    Sdl.render_set_scale renderer x y |> or_raise

  let set_clip_rect ~x ~y ~w ~h =
    with_renderer @@ fun renderer ->
    Sdl.render_set_clip_rect renderer (Some (Sdl.Rect.create ~x ~y ~w ~h)) |> or_raise

  let unset_clip_rect () =
    with_renderer @@ fun renderer ->
    Sdl.render_set_clip_rect renderer None |> or_raise

  let set_viewport ~x ~y ~w ~h =
    with_renderer @@ fun renderer ->
    Sdl.render_set_viewport renderer (Some (Sdl.Rect.create ~x ~y ~w ~h)) |> or_raise

  let unset_viewport () =
    with_renderer @@ fun renderer ->
    Sdl.render_set_viewport renderer None |> or_raise

  let set_logical_size ~w ~h =
    with_renderer @@ fun renderer ->
    Sdl.render_set_logical_size renderer w h |> or_raise

  let read ?(x = 0) ?(y = 0) ~w ~h () =
    let window = get_window_exn () in
    let Format format = window.best_format in
    let surface = Surface.create format ~w ~h in
    Sdl.lock_surface surface.sdl_surface |> or_raise;
    (
      match Pixel.size_of_format format with
        | Int32 ->
            let pixels = Sdl.get_surface_pixels surface.sdl_surface Bigarray.Int32 in
            Sdl.render_read_pixels window.renderer
              (Some (Sdl.Rect.create ~x ~y ~w ~h))
              (Some (Pixel.sdl_format_of_format format))
              pixels surface.pitch |> or_raise
    );
    Sdl.unlock_surface surface.sdl_surface;
    Surface.Surface surface
end

module Timer =
struct
  let delay x = Sdl.delay (Int32.of_int x)

  let get_ticks () =
    match Sdl.get_ticks () |> Int32.unsigned_to_int with
      | None -> -1
      | Some x -> x
end

module Key_code =
struct
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

  let show = function
    | A -> "A"
    | Ac_back -> "Ac_back"
    | Ac_bookmarks -> "Ac_bookmarks"
    | Ac_forward -> "Ac_forward"
    | Ac_home -> "Ac_home"
    | Ac_refresh -> "Ac_refresh"
    | Ac_search -> "Ac_search"
    | Ac_stop -> "Ac_stop"
    | Again -> "Again"
    | Alterase -> "Alterase"
    | Ampersand -> "Ampersand"
    | Application -> "Application"
    | Asterisk -> "Asterisk"
    | At -> "At"
    | Audiomute -> "Audiomute"
    | Audionext -> "Audionext"
    | Audioplay -> "Audioplay"
    | Audioprev -> "Audioprev"
    | Audiostop -> "Audiostop"
    | B -> "B"
    | Backquote -> "Backquote"
    | Backslash -> "Backslash"
    | Backspace -> "Backspace"
    | Brightnessdown -> "Brightnessdown"
    | Brightnessup -> "Brightnessup"
    | C -> "C"
    | Calculator -> "Calculator"
    | Cancel -> "Cancel"
    | Capslock -> "Capslock"
    | Caret -> "Caret"
    | Clear -> "Clear"
    | Clearagain -> "Clearagain"
    | Colon -> "Colon"
    | Comma -> "Comma"
    | Computer -> "Computer"
    | Copy -> "Copy"
    | Crsel -> "Crsel"
    | Currencysubunit -> "Currencysubunit"
    | Currencyunit -> "Currencyunit"
    | Cut -> "Cut"
    | D -> "D"
    | Decimalseparator -> "Decimalseparator"
    | Delete -> "Delete"
    | Displayswitch -> "Displayswitch"
    | Dollar -> "Dollar"
    | Down -> "Down"
    | E -> "E"
    | Eject -> "Eject"
    | End -> "End"
    | Equals -> "Equals"
    | Escape -> "Escape"
    | Exclaim -> "Exclaim"
    | Execute -> "Execute"
    | Exsel -> "Exsel"
    | F -> "F"
    | F1 -> "F1"
    | F10 -> "F10"
    | F11 -> "F11"
    | F12 -> "F12"
    | F13 -> "F13"
    | F14 -> "F14"
    | F15 -> "F15"
    | F16 -> "F16"
    | F17 -> "F17"
    | F18 -> "F18"
    | F19 -> "F19"
    | F2 -> "F2"
    | F20 -> "F20"
    | F21 -> "F21"
    | F22 -> "F22"
    | F23 -> "F23"
    | F24 -> "F24"
    | F3 -> "F3"
    | F4 -> "F4"
    | F5 -> "F5"
    | F6 -> "F6"
    | F7 -> "F7"
    | F8 -> "F8"
    | F9 -> "F9"
    | Find -> "Find"
    | G -> "G"
    | Greater -> "Greater"
    | H -> "H"
    | Hash -> "Hash"
    | Help -> "Help"
    | Home -> "Home"
    | I -> "I"
    | Insert -> "Insert"
    | J -> "J"
    | K -> "K"
    | K0 -> "K0"
    | K1 -> "K1"
    | K2 -> "K2"
    | K3 -> "K3"
    | K4 -> "K4"
    | K5 -> "K5"
    | K6 -> "K6"
    | K7 -> "K7"
    | K8 -> "K8"
    | K9 -> "K9"
    | Kbdillumdown -> "Kbdillumdown"
    | Kbdillumtoggle -> "Kbdillumtoggle"
    | Kbdillumup -> "Kbdillumup"
    | Kp_0 -> "Kp_0"
    | Kp_00 -> "Kp_00"
    | Kp_000 -> "Kp_000"
    | Kp_1 -> "Kp_1"
    | Kp_2 -> "Kp_2"
    | Kp_3 -> "Kp_3"
    | Kp_4 -> "Kp_4"
    | Kp_5 -> "Kp_5"
    | Kp_6 -> "Kp_6"
    | Kp_7 -> "Kp_7"
    | Kp_8 -> "Kp_8"
    | Kp_9 -> "Kp_9"
    | Kp_a -> "Kp_a"
    | Kp_ampersand -> "Kp_ampersand"
    | Kp_at -> "Kp_at"
    | Kp_b -> "Kp_b"
    | Kp_backspace -> "Kp_backspace"
    | Kp_binary -> "Kp_binary"
    | Kp_c -> "Kp_c"
    | Kp_clear -> "Kp_clear"
    | Kp_clearentry -> "Kp_clearentry"
    | Kp_colon -> "Kp_colon"
    | Kp_comma -> "Kp_comma"
    | Kp_d -> "Kp_d"
    | Kp_dblampersand -> "Kp_dblampersand"
    | Kp_dblverticalbar -> "Kp_dblverticalbar"
    | Kp_decimal -> "Kp_decimal"
    | Kp_divide -> "Kp_divide"
    | Kp_e -> "Kp_e"
    | Kp_enter -> "Kp_enter"
    | Kp_equals -> "Kp_equals"
    | Kp_equalsas400 -> "Kp_equalsas400"
    | Kp_exclam -> "Kp_exclam"
    | Kp_f -> "Kp_f"
    | Kp_greater -> "Kp_greater"
    | Kp_hash -> "Kp_hash"
    | Kp_hexadecimal -> "Kp_hexadecimal"
    | Kp_leftbrace -> "Kp_leftbrace"
    | Kp_leftparen -> "Kp_leftparen"
    | Kp_less -> "Kp_less"
    | Kp_memadd -> "Kp_memadd"
    | Kp_memclear -> "Kp_memclear"
    | Kp_memdivide -> "Kp_memdivide"
    | Kp_memmultiply -> "Kp_memmultiply"
    | Kp_memrecall -> "Kp_memrecall"
    | Kp_memstore -> "Kp_memstore"
    | Kp_memsubtract -> "Kp_memsubtract"
    | Kp_minus -> "Kp_minus"
    | Kp_multiply -> "Kp_multiply"
    | Kp_octal -> "Kp_octal"
    | Kp_percent -> "Kp_percent"
    | Kp_period -> "Kp_period"
    | Kp_plus -> "Kp_plus"
    | Kp_plusminus -> "Kp_plusminus"
    | Kp_power -> "Kp_power"
    | Kp_rightbrace -> "Kp_rightbrace"
    | Kp_rightparen -> "Kp_rightparen"
    | Kp_space -> "Kp_space"
    | Kp_tab -> "Kp_tab"
    | Kp_verticalbar -> "Kp_verticalbar"
    | Kp_xor -> "Kp_xor"
    | L -> "L"
    | Lalt -> "Lalt"
    | Lctrl -> "Lctrl"
    | Left -> "Left"
    | Leftbracket -> "Leftbracket"
    | Leftparen -> "Leftparen"
    | Less -> "Less"
    | Lgui -> "Lgui"
    | Lshift -> "Lshift"
    | M -> "M"
    | Mail -> "Mail"
    | Mediaselect -> "Mediaselect"
    | Menu -> "Menu"
    | Minus -> "Minus"
    | Mode -> "Mode"
    | Mute -> "Mute"
    | N -> "N"
    | Numlockclear -> "Numlockclear"
    | O -> "O"
    | Oper -> "Oper"
    | Out -> "Out"
    | P -> "P"
    | Pagedown -> "Pagedown"
    | Pageup -> "Pageup"
    | Paste -> "Paste"
    | Pause -> "Pause"
    | Percent -> "Percent"
    | Period -> "Period"
    | Plus -> "Plus"
    | Power -> "Power"
    | Printscreen -> "Printscreen"
    | Prior -> "Prior"
    | Q -> "Q"
    | Question -> "Question"
    | Quote -> "Quote"
    | Quotedbl -> "Quotedbl"
    | R -> "R"
    | Ralt -> "Ralt"
    | Rctrl -> "Rctrl"
    | Return -> "Return"
    | Return2 -> "Return2"
    | Rgui -> "Rgui"
    | Right -> "Right"
    | Rightbracket -> "Rightbracket"
    | Rightparen -> "Rightparen"
    | Rshift -> "Rshift"
    | S -> "S"
    | Scrolllock -> "Scrolllock"
    | Select -> "Select"
    | Semicolon -> "Semicolon"
    | Separator -> "Separator"
    | Slash -> "Slash"
    | Sleep -> "Sleep"
    | Space -> "Space"
    | Stop -> "Stop"
    | Sysreq -> "Sysreq"
    | T -> "T"
    | Tab -> "Tab"
    | Thousandsseparator -> "Thousandsseparator"
    | U -> "U"
    | Underscore -> "Underscore"
    | Undo -> "Undo"
    | Up -> "Up"
    | V -> "V"
    | Volumedown -> "Volumedown"
    | Volumeup -> "Volumeup"
    | W -> "W"
    | Www -> "Www"
    | X -> "X"
    | Y -> "Y"
    | Z -> "Z"
    | Unsupported s -> "Unsupported \"" ^ String.escaped s ^ "\""

  let map =
    let add = Int_map.add in
    Int_map.empty
    |> add Sdl.K.a A
    |> add Sdl.K.ac_back Ac_back
    |> add Sdl.K.ac_bookmarks Ac_bookmarks
    |> add Sdl.K.ac_forward Ac_forward
    |> add Sdl.K.ac_home Ac_home
    |> add Sdl.K.ac_refresh Ac_refresh
    |> add Sdl.K.ac_search Ac_search
    |> add Sdl.K.ac_stop Ac_stop
    |> add Sdl.K.again Again
    |> add Sdl.K.alterase Alterase
    |> add Sdl.K.ampersand Ampersand
    |> add Sdl.K.application Application
    |> add Sdl.K.asterisk Asterisk
    |> add Sdl.K.at At
    |> add Sdl.K.audiomute Audiomute
    |> add Sdl.K.audionext Audionext
    |> add Sdl.K.audioplay Audioplay
    |> add Sdl.K.audioprev Audioprev
    |> add Sdl.K.audiostop Audiostop
    |> add Sdl.K.b B
    |> add Sdl.K.backquote Backquote
    |> add Sdl.K.backslash Backslash
    |> add Sdl.K.backspace Backspace
    |> add Sdl.K.brightnessdown Brightnessdown
    |> add Sdl.K.brightnessup Brightnessup
    |> add Sdl.K.c C
    |> add Sdl.K.calculator Calculator
    |> add Sdl.K.cancel Cancel
    |> add Sdl.K.capslock Capslock
    |> add Sdl.K.caret Caret
    |> add Sdl.K.clear Clear
    |> add Sdl.K.clearagain Clearagain
    |> add Sdl.K.colon Colon
    |> add Sdl.K.comma Comma
    |> add Sdl.K.computer Computer
    |> add Sdl.K.copy Copy
    |> add Sdl.K.crsel Crsel
    |> add Sdl.K.currencysubunit Currencysubunit
    |> add Sdl.K.currencyunit Currencyunit
    |> add Sdl.K.cut Cut
    |> add Sdl.K.d D
    |> add Sdl.K.decimalseparator Decimalseparator
    |> add Sdl.K.delete Delete
    |> add Sdl.K.displayswitch Displayswitch
    |> add Sdl.K.dollar Dollar
    |> add Sdl.K.down Down
    |> add Sdl.K.e E
    |> add Sdl.K.eject Eject
    |> add Sdl.K.kend End
    |> add Sdl.K.equals Equals
    |> add Sdl.K.escape Escape
    |> add Sdl.K.exclaim Exclaim
    |> add Sdl.K.execute Execute
    |> add Sdl.K.exsel Exsel
    |> add Sdl.K.f F
    |> add Sdl.K.f1 F1
    |> add Sdl.K.f10 F10
    |> add Sdl.K.f11 F11
    |> add Sdl.K.f12 F12
    |> add Sdl.K.f13 F13
    |> add Sdl.K.f14 F14
    |> add Sdl.K.f15 F15
    |> add Sdl.K.f16 F16
    |> add Sdl.K.f17 F17
    |> add Sdl.K.f18 F18
    |> add Sdl.K.f19 F19
    |> add Sdl.K.f2 F2
    |> add Sdl.K.f20 F20
    |> add Sdl.K.f21 F21
    |> add Sdl.K.f22 F22
    |> add Sdl.K.f23 F23
    |> add Sdl.K.f24 F24
    |> add Sdl.K.f3 F3
    |> add Sdl.K.f4 F4
    |> add Sdl.K.f5 F5
    |> add Sdl.K.f6 F6
    |> add Sdl.K.f7 F7
    |> add Sdl.K.f8 F8
    |> add Sdl.K.f9 F9
    |> add Sdl.K.find Find
    |> add Sdl.K.g G
    |> add Sdl.K.greater Greater
    |> add Sdl.K.h H
    |> add Sdl.K.hash Hash
    |> add Sdl.K.help Help
    |> add Sdl.K.home Home
    |> add Sdl.K.i I
    |> add Sdl.K.insert Insert
    |> add Sdl.K.j J
    |> add Sdl.K.k K
    |> add Sdl.K.k0 K0
    |> add Sdl.K.k1 K1
    |> add Sdl.K.k2 K2
    |> add Sdl.K.k3 K3
    |> add Sdl.K.k4 K4
    |> add Sdl.K.k5 K5
    |> add Sdl.K.k6 K6
    |> add Sdl.K.k7 K7
    |> add Sdl.K.k8 K8
    |> add Sdl.K.k9 K9
    |> add Sdl.K.kbdillumdown Kbdillumdown
    |> add Sdl.K.kbdillumtoggle Kbdillumtoggle
    |> add Sdl.K.kbdillumup Kbdillumup
    |> add Sdl.K.kp_0 Kp_0
    |> add Sdl.K.kp_00 Kp_00
    |> add Sdl.K.kp_000 Kp_000
    |> add Sdl.K.kp_1 Kp_1
    |> add Sdl.K.kp_2 Kp_2
    |> add Sdl.K.kp_3 Kp_3
    |> add Sdl.K.kp_4 Kp_4
    |> add Sdl.K.kp_5 Kp_5
    |> add Sdl.K.kp_6 Kp_6
    |> add Sdl.K.kp_7 Kp_7
    |> add Sdl.K.kp_8 Kp_8
    |> add Sdl.K.kp_9 Kp_9
    |> add Sdl.K.kp_a Kp_a
    |> add Sdl.K.kp_ampersand Kp_ampersand
    |> add Sdl.K.kp_at Kp_at
    |> add Sdl.K.kp_b Kp_b
    |> add Sdl.K.kp_backspace Kp_backspace
    |> add Sdl.K.kp_binary Kp_binary
    |> add Sdl.K.kp_c Kp_c
    |> add Sdl.K.kp_clear Kp_clear
    |> add Sdl.K.kp_clearentry Kp_clearentry
    |> add Sdl.K.kp_colon Kp_colon
    |> add Sdl.K.kp_comma Kp_comma
    |> add Sdl.K.kp_d Kp_d
    |> add Sdl.K.kp_dblampersand Kp_dblampersand
    |> add Sdl.K.kp_dblverticalbar Kp_dblverticalbar
    |> add Sdl.K.kp_decimal Kp_decimal
    |> add Sdl.K.kp_divide Kp_divide
    |> add Sdl.K.kp_e Kp_e
    |> add Sdl.K.kp_enter Kp_enter
    |> add Sdl.K.kp_equals Kp_equals
    |> add Sdl.K.kp_equalsas400 Kp_equalsas400
    |> add Sdl.K.kp_exclam Kp_exclam
    |> add Sdl.K.kp_f Kp_f
    |> add Sdl.K.kp_greater Kp_greater
    |> add Sdl.K.kp_hash Kp_hash
    |> add Sdl.K.kp_hexadecimal Kp_hexadecimal
    |> add Sdl.K.kp_leftbrace Kp_leftbrace
    |> add Sdl.K.kp_leftparen Kp_leftparen
    |> add Sdl.K.kp_less Kp_less
    |> add Sdl.K.kp_memadd Kp_memadd
    |> add Sdl.K.kp_memclear Kp_memclear
    |> add Sdl.K.kp_memdivide Kp_memdivide
    |> add Sdl.K.kp_memmultiply Kp_memmultiply
    |> add Sdl.K.kp_memrecall Kp_memrecall
    |> add Sdl.K.kp_memstore Kp_memstore
    |> add Sdl.K.kp_memsubtract Kp_memsubtract
    |> add Sdl.K.kp_minus Kp_minus
    |> add Sdl.K.kp_multiply Kp_multiply
    |> add Sdl.K.kp_octal Kp_octal
    |> add Sdl.K.kp_percent Kp_percent
    |> add Sdl.K.kp_period Kp_period
    |> add Sdl.K.kp_plus Kp_plus
    |> add Sdl.K.kp_plusminus Kp_plusminus
    |> add Sdl.K.kp_power Kp_power
    |> add Sdl.K.kp_rightbrace Kp_rightbrace
    |> add Sdl.K.kp_rightparen Kp_rightparen
    |> add Sdl.K.kp_space Kp_space
    |> add Sdl.K.kp_tab Kp_tab
    |> add Sdl.K.kp_verticalbar Kp_verticalbar
    |> add Sdl.K.kp_xor Kp_xor
    |> add Sdl.K.l L
    |> add Sdl.K.lalt Lalt
    |> add Sdl.K.lctrl Lctrl
    |> add Sdl.K.left Left
    |> add Sdl.K.leftbracket Leftbracket
    |> add Sdl.K.leftparen Leftparen
    |> add Sdl.K.less Less
    |> add Sdl.K.lgui Lgui
    |> add Sdl.K.lshift Lshift
    |> add Sdl.K.m M
    |> add Sdl.K.mail Mail
    |> add Sdl.K.mediaselect Mediaselect
    |> add Sdl.K.menu Menu
    |> add Sdl.K.minus Minus
    |> add Sdl.K.mode Mode
    |> add Sdl.K.mute Mute
    |> add Sdl.K.n N
    |> add Sdl.K.numlockclear Numlockclear
    |> add Sdl.K.o O
    |> add Sdl.K.oper Oper
    |> add Sdl.K.out Out
    |> add Sdl.K.p P
    |> add Sdl.K.pagedown Pagedown
    |> add Sdl.K.pageup Pageup
    |> add Sdl.K.paste Paste
    |> add Sdl.K.pause Pause
    |> add Sdl.K.percent Percent
    |> add Sdl.K.period Period
    |> add Sdl.K.plus Plus
    |> add Sdl.K.power Power
    |> add Sdl.K.printscreen Printscreen
    |> add Sdl.K.prior Prior
    |> add Sdl.K.q Q
    |> add Sdl.K.question Question
    |> add Sdl.K.quote Quote
    |> add Sdl.K.quotedbl Quotedbl
    |> add Sdl.K.r R
    |> add Sdl.K.ralt Ralt
    |> add Sdl.K.rctrl Rctrl
    |> add Sdl.K.return Return
    |> add Sdl.K.return2 Return2
    |> add Sdl.K.rgui Rgui
    |> add Sdl.K.right Right
    |> add Sdl.K.rightbracket Rightbracket
    |> add Sdl.K.rightparen Rightparen
    |> add Sdl.K.rshift Rshift
    |> add Sdl.K.s S
    |> add Sdl.K.scrolllock Scrolllock
    |> add Sdl.K.select Select
    |> add Sdl.K.semicolon Semicolon
    |> add Sdl.K.separator Separator
    |> add Sdl.K.slash Slash
    |> add Sdl.K.sleep Sleep
    |> add Sdl.K.space Space
    |> add Sdl.K.stop Stop
    |> add Sdl.K.sysreq Sysreq
    |> add Sdl.K.t T
    |> add Sdl.K.tab Tab
    |> add Sdl.K.thousandsseparator Thousandsseparator
    |> add Sdl.K.u U
    |> add Sdl.K.underscore Underscore
    |> add Sdl.K.undo Undo
    |> add Sdl.K.up Up
    |> add Sdl.K.v V
    |> add Sdl.K.volumedown Volumedown
    |> add Sdl.K.volumeup Volumeup
    |> add Sdl.K.w W
    |> add Sdl.K.www Www
    |> add Sdl.K.x X
    |> add Sdl.K.y Y
    |> add Sdl.K.z Z

  let of_sdl code =
    match Int_map.find_opt code map with
      | None -> Unsupported (string_of_int code)
      | Some code -> code
end

module Scan_code =
struct
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

  let show = function
    | A -> "A"
    | Ac_back -> "Ac_back"
    | Ac_bookmarks -> "Ac_bookmarks"
    | Ac_forward -> "Ac_forward"
    | Ac_home -> "Ac_home"
    | Ac_refresh -> "Ac_refresh"
    | Ac_search -> "Ac_search"
    | Ac_stop -> "Ac_stop"
    | Again -> "Again"
    | Alterase -> "Alterase"
    | Apostrophe -> "Apostrophe"
    | App1 -> "App1"
    | App2 -> "App2"
    | Application -> "Application"
    | Audiomute -> "Audiomute"
    | Audionext -> "Audionext"
    | Audioplay -> "Audioplay"
    | Audioprev -> "Audioprev"
    | Audiostop -> "Audiostop"
    | B -> "B"
    | Backslash -> "Backslash"
    | Backspace -> "Backspace"
    | Brightnessdown -> "Brightnessdown"
    | Brightnessup -> "Brightnessup"
    | C -> "C"
    | Calculator -> "Calculator"
    | Cancel -> "Cancel"
    | Capslock -> "Capslock"
    | Clear -> "Clear"
    | Clearagain -> "Clearagain"
    | Comma -> "Comma"
    | Computer -> "Computer"
    | Copy -> "Copy"
    | Crsel -> "Crsel"
    | Currencysubunit -> "Currencysubunit"
    | Currencyunit -> "Currencyunit"
    | Cut -> "Cut"
    | D -> "D"
    | Decimalseparator -> "Decimalseparator"
    | Delete -> "Delete"
    | Displayswitch -> "Displayswitch"
    | Down -> "Down"
    | E -> "E"
    | Eject -> "Eject"
    | End -> "End"
    | Equals -> "Equals"
    | Escape -> "Escape"
    | Execute -> "Execute"
    | Exsel -> "Exsel"
    | F -> "F"
    | F1 -> "F1"
    | F10 -> "F10"
    | F11 -> "F11"
    | F12 -> "F12"
    | F13 -> "F13"
    | F14 -> "F14"
    | F15 -> "F15"
    | F16 -> "F16"
    | F17 -> "F17"
    | F18 -> "F18"
    | F19 -> "F19"
    | F2 -> "F2"
    | F20 -> "F20"
    | F21 -> "F21"
    | F22 -> "F22"
    | F23 -> "F23"
    | F24 -> "F24"
    | F3 -> "F3"
    | F4 -> "F4"
    | F5 -> "F5"
    | F6 -> "F6"
    | F7 -> "F7"
    | F8 -> "F8"
    | F9 -> "F9"
    | Find -> "Find"
    | G -> "G"
    | Grave -> "Grave"
    | H -> "H"
    | Help -> "Help"
    | Home -> "Home"
    | I -> "I"
    | Insert -> "Insert"
    | International1 -> "International1"
    | International2 -> "International2"
    | International3 -> "International3"
    | International4 -> "International4"
    | International5 -> "International5"
    | International6 -> "International6"
    | International7 -> "International7"
    | International8 -> "International8"
    | International9 -> "International9"
    | J -> "J"
    | K -> "K"
    | K0 -> "K0"
    | K1 -> "K1"
    | K2 -> "K2"
    | K3 -> "K3"
    | K4 -> "K4"
    | K5 -> "K5"
    | K6 -> "K6"
    | K7 -> "K7"
    | K8 -> "K8"
    | K9 -> "K9"
    | Kbdillumdown -> "Kbdillumdown"
    | Kbdillumtoggle -> "Kbdillumtoggle"
    | Kbdillumup -> "Kbdillumup"
    | Kp_0 -> "Kp_0"
    | Kp_00 -> "Kp_00"
    | Kp_000 -> "Kp_000"
    | Kp_1 -> "Kp_1"
    | Kp_2 -> "Kp_2"
    | Kp_3 -> "Kp_3"
    | Kp_4 -> "Kp_4"
    | Kp_5 -> "Kp_5"
    | Kp_6 -> "Kp_6"
    | Kp_7 -> "Kp_7"
    | Kp_8 -> "Kp_8"
    | Kp_9 -> "Kp_9"
    | Kp_a -> "Kp_a"
    | Kp_ampersand -> "Kp_ampersand"
    | Kp_at -> "Kp_at"
    | Kp_b -> "Kp_b"
    | Kp_backspace -> "Kp_backspace"
    | Kp_binary -> "Kp_binary"
    | Kp_c -> "Kp_c"
    | Kp_clear -> "Kp_clear"
    | Kp_clearentry -> "Kp_clearentry"
    | Kp_colon -> "Kp_colon"
    | Kp_comma -> "Kp_comma"
    | Kp_d -> "Kp_d"
    | Kp_dblampersand -> "Kp_dblampersand"
    | Kp_dblverticalbar -> "Kp_dblverticalbar"
    | Kp_decimal -> "Kp_decimal"
    | Kp_divide -> "Kp_divide"
    | Kp_e -> "Kp_e"
    | Kp_enter -> "Kp_enter"
    | Kp_equals -> "Kp_equals"
    | Kp_equalsas400 -> "Kp_equalsas400"
    | Kp_exclam -> "Kp_exclam"
    | Kp_f -> "Kp_f"
    | Kp_greater -> "Kp_greater"
    | Kp_hash -> "Kp_hash"
    | Kp_hexadecimal -> "Kp_hexadecimal"
    | Kp_leftbrace -> "Kp_leftbrace"
    | Kp_leftparen -> "Kp_leftparen"
    | Kp_less -> "Kp_less"
    | Kp_memadd -> "Kp_memadd"
    | Kp_memclear -> "Kp_memclear"
    | Kp_memdivide -> "Kp_memdivide"
    | Kp_memmultiply -> "Kp_memmultiply"
    | Kp_memrecall -> "Kp_memrecall"
    | Kp_memstore -> "Kp_memstore"
    | Kp_memsubtract -> "Kp_memsubtract"
    | Kp_minus -> "Kp_minus"
    | Kp_multiply -> "Kp_multiply"
    | Kp_octal -> "Kp_octal"
    | Kp_percent -> "Kp_percent"
    | Kp_period -> "Kp_period"
    | Kp_plus -> "Kp_plus"
    | Kp_plusminus -> "Kp_plusminus"
    | Kp_power -> "Kp_power"
    | Kp_rightbrace -> "Kp_rightbrace"
    | Kp_rightparen -> "Kp_rightparen"
    | Kp_space -> "Kp_space"
    | Kp_tab -> "Kp_tab"
    | Kp_verticalbar -> "Kp_verticalbar"
    | Kp_xor -> "Kp_xor"
    | L -> "L"
    | Lalt -> "Lalt"
    | Lang1 -> "Lang1"
    | Lang2 -> "Lang2"
    | Lang3 -> "Lang3"
    | Lang4 -> "Lang4"
    | Lang5 -> "Lang5"
    | Lang6 -> "Lang6"
    | Lang7 -> "Lang7"
    | Lang8 -> "Lang8"
    | Lang9 -> "Lang9"
    | Lctrl -> "Lctrl"
    | Left -> "Left"
    | Leftbracket -> "Leftbracket"
    | Lgui -> "Lgui"
    | Lshift -> "Lshift"
    | M -> "M"
    | Mail -> "Mail"
    | Mediaselect -> "Mediaselect"
    | Menu -> "Menu"
    | Minus -> "Minus"
    | Mode -> "Mode"
    | Mute -> "Mute"
    | N -> "N"
    | Nonusbackslash -> "Nonusbackslash"
    | Nonushash -> "Nonushash"
    | Numlockclear -> "Numlockclear"
    | O -> "O"
    | Oper -> "Oper"
    | Out -> "Out"
    | P -> "P"
    | Pagedown -> "Pagedown"
    | Pageup -> "Pageup"
    | Paste -> "Paste"
    | Pause -> "Pause"
    | Period -> "Period"
    | Printscreen -> "Printscreen"
    | Prior -> "Prior"
    | Q -> "Q"
    | R -> "R"
    | Ralt -> "Ralt"
    | Rctrl -> "Rctrl"
    | Return -> "Return"
    | Return2 -> "Return2"
    | Rgui -> "Rgui"
    | Right -> "Right"
    | Rightbracket -> "Rightbracket"
    | Rshift -> "Rshift"
    | S -> "S"
    | Scrolllock -> "Scrolllock"
    | Select -> "Select"
    | Semicolon -> "Semicolon"
    | Separator -> "Separator"
    | Slash -> "Slash"
    | Sleep -> "Sleep"
    | Space -> "Space"
    | Stop -> "Stop"
    | Sysreq -> "Sysreq"
    | T -> "T"
    | Tab -> "Tab"
    | Thousandsseparator -> "Thousandsseparator"
    | U -> "U"
    | Undo -> "Undo"
    | Up -> "Up"
    | V -> "V"
    | Volumedown -> "Volumedown"
    | Volumeup -> "Volumeup"
    | W -> "W"
    | Www -> "Www"
    | X -> "X"
    | Y -> "Y"
    | Z -> "Z"
    | Unsupported s -> "Unsupported \"" ^ String.escaped s ^ "\""

  let map =
    let add = Int_map.add in
    Int_map.empty
    |> add Sdl.Scancode.a A
    |> add Sdl.Scancode.ac_back Ac_back
    |> add Sdl.Scancode.ac_bookmarks Ac_bookmarks
    |> add Sdl.Scancode.ac_forward Ac_forward
    |> add Sdl.Scancode.ac_home Ac_home
    |> add Sdl.Scancode.ac_refresh Ac_refresh
    |> add Sdl.Scancode.ac_search Ac_search
    |> add Sdl.Scancode.ac_stop Ac_stop
    |> add Sdl.Scancode.again Again
    |> add Sdl.Scancode.alterase Alterase
    |> add Sdl.Scancode.apostrophe Apostrophe
    |> add Sdl.Scancode.app1 App1
    |> add Sdl.Scancode.app2 App2
    |> add Sdl.Scancode.application Application
    |> add Sdl.Scancode.audiomute Audiomute
    |> add Sdl.Scancode.audionext Audionext
    |> add Sdl.Scancode.audioplay Audioplay
    |> add Sdl.Scancode.audioprev Audioprev
    |> add Sdl.Scancode.audiostop Audiostop
    |> add Sdl.Scancode.b B
    |> add Sdl.Scancode.backslash Backslash
    |> add Sdl.Scancode.backspace Backspace
    |> add Sdl.Scancode.brightnessdown Brightnessdown
    |> add Sdl.Scancode.brightnessup Brightnessup
    |> add Sdl.Scancode.c C
    |> add Sdl.Scancode.calculator Calculator
    |> add Sdl.Scancode.cancel Cancel
    |> add Sdl.Scancode.capslock Capslock
    |> add Sdl.Scancode.clear Clear
    |> add Sdl.Scancode.clearagain Clearagain
    |> add Sdl.Scancode.comma Comma
    |> add Sdl.Scancode.computer Computer
    |> add Sdl.Scancode.copy Copy
    |> add Sdl.Scancode.crsel Crsel
    |> add Sdl.Scancode.currencysubunit Currencysubunit
    |> add Sdl.Scancode.currencyunit Currencyunit
    |> add Sdl.Scancode.cut Cut
    |> add Sdl.Scancode.d D
    |> add Sdl.Scancode.decimalseparator Decimalseparator
    |> add Sdl.Scancode.delete Delete
    |> add Sdl.Scancode.displayswitch Displayswitch
    |> add Sdl.Scancode.down Down
    |> add Sdl.Scancode.e E
    |> add Sdl.Scancode.eject Eject
    |> add Sdl.Scancode.kend End
    |> add Sdl.Scancode.equals Equals
    |> add Sdl.Scancode.escape Escape
    |> add Sdl.Scancode.execute Execute
    |> add Sdl.Scancode.exsel Exsel
    |> add Sdl.Scancode.f F
    |> add Sdl.Scancode.f1 F1
    |> add Sdl.Scancode.f10 F10
    |> add Sdl.Scancode.f11 F11
    |> add Sdl.Scancode.f12 F12
    |> add Sdl.Scancode.f13 F13
    |> add Sdl.Scancode.f14 F14
    |> add Sdl.Scancode.f15 F15
    |> add Sdl.Scancode.f16 F16
    |> add Sdl.Scancode.f17 F17
    |> add Sdl.Scancode.f18 F18
    |> add Sdl.Scancode.f19 F19
    |> add Sdl.Scancode.f2 F2
    |> add Sdl.Scancode.f20 F20
    |> add Sdl.Scancode.f21 F21
    |> add Sdl.Scancode.f22 F22
    |> add Sdl.Scancode.f23 F23
    |> add Sdl.Scancode.f24 F24
    |> add Sdl.Scancode.f3 F3
    |> add Sdl.Scancode.f4 F4
    |> add Sdl.Scancode.f5 F5
    |> add Sdl.Scancode.f6 F6
    |> add Sdl.Scancode.f7 F7
    |> add Sdl.Scancode.f8 F8
    |> add Sdl.Scancode.f9 F9
    |> add Sdl.Scancode.find Find
    |> add Sdl.Scancode.g G
    |> add Sdl.Scancode.grave Grave
    |> add Sdl.Scancode.h H
    |> add Sdl.Scancode.help Help
    |> add Sdl.Scancode.home Home
    |> add Sdl.Scancode.i I
    |> add Sdl.Scancode.insert Insert
    |> add Sdl.Scancode.international1 International1
    |> add Sdl.Scancode.international2 International2
    |> add Sdl.Scancode.international3 International3
    |> add Sdl.Scancode.international4 International4
    |> add Sdl.Scancode.international5 International5
    |> add Sdl.Scancode.international6 International6
    |> add Sdl.Scancode.international7 International7
    |> add Sdl.Scancode.international8 International8
    |> add Sdl.Scancode.international9 International9
    |> add Sdl.Scancode.j J
    |> add Sdl.Scancode.k K
    |> add Sdl.Scancode.k0 K0
    |> add Sdl.Scancode.k1 K1
    |> add Sdl.Scancode.k2 K2
    |> add Sdl.Scancode.k3 K3
    |> add Sdl.Scancode.k4 K4
    |> add Sdl.Scancode.k5 K5
    |> add Sdl.Scancode.k6 K6
    |> add Sdl.Scancode.k7 K7
    |> add Sdl.Scancode.k8 K8
    |> add Sdl.Scancode.k9 K9
    |> add Sdl.Scancode.kbdillumdown Kbdillumdown
    |> add Sdl.Scancode.kbdillumtoggle Kbdillumtoggle
    |> add Sdl.Scancode.kbdillumup Kbdillumup
    |> add Sdl.Scancode.kp_0 Kp_0
    |> add Sdl.Scancode.kp_00 Kp_00
    |> add Sdl.Scancode.kp_000 Kp_000
    |> add Sdl.Scancode.kp_1 Kp_1
    |> add Sdl.Scancode.kp_2 Kp_2
    |> add Sdl.Scancode.kp_3 Kp_3
    |> add Sdl.Scancode.kp_4 Kp_4
    |> add Sdl.Scancode.kp_5 Kp_5
    |> add Sdl.Scancode.kp_6 Kp_6
    |> add Sdl.Scancode.kp_7 Kp_7
    |> add Sdl.Scancode.kp_8 Kp_8
    |> add Sdl.Scancode.kp_9 Kp_9
    |> add Sdl.Scancode.kp_a Kp_a
    |> add Sdl.Scancode.kp_ampersand Kp_ampersand
    |> add Sdl.Scancode.kp_at Kp_at
    |> add Sdl.Scancode.kp_b Kp_b
    |> add Sdl.Scancode.kp_backspace Kp_backspace
    |> add Sdl.Scancode.kp_binary Kp_binary
    |> add Sdl.Scancode.kp_c Kp_c
    |> add Sdl.Scancode.kp_clear Kp_clear
    |> add Sdl.Scancode.kp_clearentry Kp_clearentry
    |> add Sdl.Scancode.kp_colon Kp_colon
    |> add Sdl.Scancode.kp_comma Kp_comma
    |> add Sdl.Scancode.kp_d Kp_d
    |> add Sdl.Scancode.kp_dblampersand Kp_dblampersand
    |> add Sdl.Scancode.kp_dblverticalbar Kp_dblverticalbar
    |> add Sdl.Scancode.kp_decimal Kp_decimal
    |> add Sdl.Scancode.kp_divide Kp_divide
    |> add Sdl.Scancode.kp_e Kp_e
    |> add Sdl.Scancode.kp_enter Kp_enter
    |> add Sdl.Scancode.kp_equals Kp_equals
    |> add Sdl.Scancode.kp_equalsas400 Kp_equalsas400
    |> add Sdl.Scancode.kp_exclam Kp_exclam
    |> add Sdl.Scancode.kp_f Kp_f
    |> add Sdl.Scancode.kp_greater Kp_greater
    |> add Sdl.Scancode.kp_hash Kp_hash
    |> add Sdl.Scancode.kp_hexadecimal Kp_hexadecimal
    |> add Sdl.Scancode.kp_leftbrace Kp_leftbrace
    |> add Sdl.Scancode.kp_leftparen Kp_leftparen
    |> add Sdl.Scancode.kp_less Kp_less
    |> add Sdl.Scancode.kp_memadd Kp_memadd
    |> add Sdl.Scancode.kp_memclear Kp_memclear
    |> add Sdl.Scancode.kp_memdivide Kp_memdivide
    |> add Sdl.Scancode.kp_memmultiply Kp_memmultiply
    |> add Sdl.Scancode.kp_memrecall Kp_memrecall
    |> add Sdl.Scancode.kp_memstore Kp_memstore
    |> add Sdl.Scancode.kp_memsubtract Kp_memsubtract
    |> add Sdl.Scancode.kp_minus Kp_minus
    |> add Sdl.Scancode.kp_multiply Kp_multiply
    |> add Sdl.Scancode.kp_octal Kp_octal
    |> add Sdl.Scancode.kp_percent Kp_percent
    |> add Sdl.Scancode.kp_period Kp_period
    |> add Sdl.Scancode.kp_plus Kp_plus
    |> add Sdl.Scancode.kp_plusminus Kp_plusminus
    |> add Sdl.Scancode.kp_power Kp_power
    |> add Sdl.Scancode.kp_rightbrace Kp_rightbrace
    |> add Sdl.Scancode.kp_rightparen Kp_rightparen
    |> add Sdl.Scancode.kp_space Kp_space
    |> add Sdl.Scancode.kp_tab Kp_tab
    |> add Sdl.Scancode.kp_verticalbar Kp_verticalbar
    |> add Sdl.Scancode.kp_xor Kp_xor
    |> add Sdl.Scancode.l L
    |> add Sdl.Scancode.lalt Lalt
    |> add Sdl.Scancode.lang1 Lang1
    |> add Sdl.Scancode.lang2 Lang2
    |> add Sdl.Scancode.lang3 Lang3
    |> add Sdl.Scancode.lang4 Lang4
    |> add Sdl.Scancode.lang5 Lang5
    |> add Sdl.Scancode.lang6 Lang6
    |> add Sdl.Scancode.lang7 Lang7
    |> add Sdl.Scancode.lang8 Lang8
    |> add Sdl.Scancode.lang9 Lang9
    |> add Sdl.Scancode.lctrl Lctrl
    |> add Sdl.Scancode.left Left
    |> add Sdl.Scancode.leftbracket Leftbracket
    |> add Sdl.Scancode.lgui Lgui
    |> add Sdl.Scancode.lshift Lshift
    |> add Sdl.Scancode.m M
    |> add Sdl.Scancode.mail Mail
    |> add Sdl.Scancode.mediaselect Mediaselect
    |> add Sdl.Scancode.menu Menu
    |> add Sdl.Scancode.minus Minus
    |> add Sdl.Scancode.mode Mode
    |> add Sdl.Scancode.mute Mute
    |> add Sdl.Scancode.n N
    |> add Sdl.Scancode.nonusbackslash Nonusbackslash
    |> add Sdl.Scancode.nonushash Nonushash
    |> add Sdl.Scancode.numlockclear Numlockclear
    |> add Sdl.Scancode.o O
    |> add Sdl.Scancode.oper Oper
    |> add Sdl.Scancode.out Out
    |> add Sdl.Scancode.p P
    |> add Sdl.Scancode.pagedown Pagedown
    |> add Sdl.Scancode.pageup Pageup
    |> add Sdl.Scancode.paste Paste
    |> add Sdl.Scancode.pause Pause
    |> add Sdl.Scancode.period Period
    |> add Sdl.Scancode.printscreen Printscreen
    |> add Sdl.Scancode.prior Prior
    |> add Sdl.Scancode.q Q
    |> add Sdl.Scancode.r R
    |> add Sdl.Scancode.ralt Ralt
    |> add Sdl.Scancode.rctrl Rctrl
    |> add Sdl.Scancode.return Return
    |> add Sdl.Scancode.return2 Return2
    |> add Sdl.Scancode.rgui Rgui
    |> add Sdl.Scancode.right Right
    |> add Sdl.Scancode.rightbracket Rightbracket
    |> add Sdl.Scancode.rshift Rshift
    |> add Sdl.Scancode.s S
    |> add Sdl.Scancode.scrolllock Scrolllock
    |> add Sdl.Scancode.select Select
    |> add Sdl.Scancode.semicolon Semicolon
    |> add Sdl.Scancode.separator Separator
    |> add Sdl.Scancode.slash Slash
    |> add Sdl.Scancode.sleep Sleep
    |> add Sdl.Scancode.space Space
    |> add Sdl.Scancode.stop Stop
    |> add Sdl.Scancode.sysreq Sysreq
    |> add Sdl.Scancode.t T
    |> add Sdl.Scancode.tab Tab
    |> add Sdl.Scancode.thousandsseparator Thousandsseparator
    |> add Sdl.Scancode.u U
    |> add Sdl.Scancode.undo Undo
    |> add Sdl.Scancode.up Up
    |> add Sdl.Scancode.v V
    |> add Sdl.Scancode.volumedown Volumedown
    |> add Sdl.Scancode.volumeup Volumeup
    |> add Sdl.Scancode.w W
    |> add Sdl.Scancode.www Www
    |> add Sdl.Scancode.x X
    |> add Sdl.Scancode.y Y
    |> add Sdl.Scancode.z Z

  let of_sdl code =
    match Int_map.find_opt code map with
      | None -> Unsupported (string_of_int code)
      | Some code -> code
end

module Mouse_button =
struct
  type t =
    | Left
    | Middle
    | Right
    | Button of int

  let show = function
    | Left -> "Left"
    | Middle -> "Middle"
    | Right -> "Right"
    | Button i -> "Button " ^ string_of_int i

  let of_sdl = function
    | 1 -> Left
    | 2 -> Middle
    | 3 -> Right
    | i -> Button i
end

module Event =
struct
  type button_state = Pressed | Released

  type quit =
    {
      timestamp: int;
    }

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

  type window =
    {
      timestamp: int;
      window_id: int;
    }

  type t =
    | Quit of quit
    | Key_down of keyboard
    | Key_up of keyboard
    | Mouse_button_down of mouse_button
    | Mouse_button_up of mouse_button
    | Mouse_motion of mouse_motion
    | Mouse_wheel of mouse_wheel
    | Window_shown of window
    | Window_hidden of window
    | Window_exposed of window
    | Window_moved of window
    | Window_resized of window
    | Window_size_changed of window
    | Window_minimized of window
    | Window_maximized of window
    | Window_restored of window
    | Window_enter of window
    | Window_leave of window
    | Window_focus_gained of window
    | Window_focus_lost of window
    | Window_close of window
    | Window_take_focus of window
    | Unsupported

  let show_event typ fields =
    let buffer = Buffer.create 512 in
    Buffer.add_string buffer typ;
    Buffer.add_string buffer " {";
    let add_field (name, value) =
      Buffer.add_string buffer "\n  ";
      Buffer.add_string buffer name;
      Buffer.add_string buffer " = ";
      Buffer.add_string buffer value;
      Buffer.add_char buffer ';'
    in
    List.iter add_field fields;
    Buffer.add_string buffer "\n}";
    Buffer.contents buffer

  let show_button_state = function
    | Pressed -> "Pressed"
    | Released -> "Released"

  let show_keyboard_event typ
      {
        timestamp;
        window_id;
        state;
        repeat;
        scan_code;
        key_code;
        mod_lshift;
        mod_rshift;
        mod_lctrl;
        mod_rctrl;
        mod_lalt;
        mod_ralt;
        mod_lgui;
        mod_rgui;
        mod_num;
        mod_caps;
        mod_mode;
        mod_ctrl;
        mod_shift;
        mod_alt;
        mod_gui;
      } =
    show_event typ [
      "timestamp", string_of_int timestamp;
      "window_id", string_of_int window_id;
      "state", show_button_state state;
      "repeat", string_of_bool repeat;
      "scan_code", Scan_code.show scan_code;
      "key_code", Key_code.show key_code;
      "mod_lshift", string_of_bool mod_lshift;
      "mod_rshift", string_of_bool mod_rshift;
      "mod_lctrl", string_of_bool mod_lctrl;
      "mod_rctrl", string_of_bool mod_rctrl;
      "mod_lalt", string_of_bool mod_lalt;
      "mod_ralt", string_of_bool mod_ralt;
      "mod_lgui", string_of_bool mod_lgui;
      "mod_rgui", string_of_bool mod_rgui;
      "mod_num", string_of_bool mod_num;
      "mod_caps", string_of_bool mod_caps;
      "mod_mode", string_of_bool mod_mode;
      "mod_ctrl", string_of_bool mod_ctrl;
      "mod_shift", string_of_bool mod_shift;
      "mod_alt", string_of_bool mod_alt;
      "mod_gui", string_of_bool mod_gui;
    ]

  let show_mouse_button_event typ
      {
        timestamp;
        window_id;
        which;
        button;
        state;
        clicks;
        x;
        y;
      } =
    show_event typ [
      "timestamp", string_of_int timestamp;
      "window_id", string_of_int window_id;
      "which", string_of_int which;
      "button", Mouse_button.show button;
      "state", show_button_state state;
      "clicks", string_of_int clicks;
      "x", string_of_int x;
      "y", string_of_int y;
    ]

  let show_window_event typ { timestamp; window_id } =
    show_event typ [
      "timestamp", string_of_int timestamp;
      "window_id", string_of_int window_id;
    ]

  let show = function
    | Quit { timestamp } ->
        show_event "Quit" [ "timestamp", string_of_int timestamp ]
    | Key_down event ->
        show_keyboard_event "Key_down" event
    | Key_up event ->
        show_keyboard_event "Key_up" event
    | Mouse_button_down event ->
        show_mouse_button_event "Mouse_button_down" event
    | Mouse_button_up event ->
        show_mouse_button_event "Mouse_button_up" event
    | Mouse_motion {
        timestamp;
        window_id;
        which;
        button_left;
        button_middle;
        button_right;
        button_x1;
        button_x2;
        x;
        y;
        xrel;
        yrel;
      } ->
        show_event "Mouse_motion" [
          "timestamp", string_of_int timestamp;
          "window_id", string_of_int window_id;
          "which", string_of_int which;
          "button_left", string_of_bool button_left;
          "button_middle", string_of_bool button_middle;
          "button_right", string_of_bool button_right;
          "button_x1", string_of_bool button_x1;
          "button_x2", string_of_bool button_x2;
          "x", string_of_int x;
          "y", string_of_int y;
          "xrel", string_of_int xrel;
          "yrel", string_of_int yrel;
        ]
    | Mouse_wheel {
        timestamp;
        window_id;
        which;
        x;
        y;
      } ->
        show_event "Mouse_wheel" [
          "timestamp", string_of_int timestamp;
          "window_id", string_of_int window_id;
          "which", string_of_int which;
          "x", string_of_int x;
          "y", string_of_int y;
        ]
    | Window_shown event ->
        show_window_event "Window_shown" event
    | Window_hidden event ->
        show_window_event "Window_hidden" event
    | Window_exposed event ->
        show_window_event "Window_exposed" event
    | Window_moved event ->
        show_window_event "Window_moved" event
    | Window_resized event ->
        show_window_event "Window_resized" event
    | Window_size_changed event ->
        show_window_event "Window_size_changed" event
    | Window_minimized event ->
        show_window_event "Window_minimized" event
    | Window_maximized event ->
        show_window_event "Window_maximized" event
    | Window_restored event ->
        show_window_event "Window_restored" event
    | Window_enter event ->
        show_window_event "Window_enter" event
    | Window_leave event ->
        show_window_event "Window_leave" event
    | Window_focus_gained event ->
        show_window_event "Window_focus_gained" event
    | Window_focus_lost event ->
        show_window_event "Window_focus_lost" event
    | Window_close event ->
        show_window_event "Window_close" event
    | Window_take_focus event ->
        show_window_event "Window_take_focus" event
    | Unsupported ->
        "Unsupported"

  module E = Sdl.Event

  (* We create it once and for all, since we read all fields immediately.
     Not the first thing that is not thread-safe in this lib. *)
  let event = E.create ()

  let decode_timestamp () =
    match Int32.unsigned_to_int (E.get event E.timestamp) with
      | None ->
          -1
      | Some timestamp ->
          timestamp

  let decode_button_state state =
    if state = Sdl.pressed then
      Pressed
    else
      Released

  let decode_keyboard () =
    let keymod = E.get event E.keyboard_keymod in
    {
      timestamp = decode_timestamp ();
      window_id = E.get event E.keyboard_window_id;
      state = decode_button_state (E.get event E.keyboard_state);
      (* [keyboard_repeat] is an [int field], but its value is either 0 or 1. *)
      repeat = E.get event E.keyboard_repeat > 0;
      scan_code = Scan_code.of_sdl (E.get event E.keyboard_scancode);
      key_code = Key_code.of_sdl (E.get event E.keyboard_keycode);
      mod_lshift = keymod land Sdl.Kmod.lshift > 0;
      mod_rshift = keymod land Sdl.Kmod.rshift > 0;
      mod_lctrl = keymod land Sdl.Kmod.lctrl > 0;
      mod_rctrl = keymod land Sdl.Kmod.rctrl > 0;
      mod_lalt = keymod land Sdl.Kmod.lalt > 0;
      mod_ralt = keymod land Sdl.Kmod.ralt > 0;
      mod_lgui = keymod land Sdl.Kmod.lgui > 0;
      mod_rgui = keymod land Sdl.Kmod.rgui > 0;
      mod_num = keymod land Sdl.Kmod.num > 0;
      mod_caps = keymod land Sdl.Kmod.caps > 0;
      mod_mode = keymod land Sdl.Kmod.mode > 0;
      mod_ctrl = keymod land Sdl.Kmod.ctrl > 0;
      mod_shift = keymod land Sdl.Kmod.shift > 0;
      mod_alt = keymod land Sdl.Kmod.alt > 0;
      mod_gui = keymod land Sdl.Kmod.gui > 0;
    }

  let decode_mouse_button () =
    {
      timestamp = decode_timestamp ();
      window_id = E.get event E.mouse_button_window_id;
      which = E.get event E.mouse_button_which |> Int32.to_int;
      (* TODO: button enum type? *)
      button = Mouse_button.of_sdl (E.get event E.mouse_button_button);
      state = decode_button_state (E.get event E.mouse_button_state);
      clicks = E.get event E.mouse_button_clicks;
      x = E.get event E.mouse_button_x;
      y = E.get event E.mouse_button_y;
    }

  let decode_mouse_motion () =
    let state = E.get event E.mouse_motion_state in
    {
      timestamp = decode_timestamp ();
      window_id = E.get event E.mouse_motion_window_id;
      which = E.get event E.mouse_motion_which |> Int32.to_int;
      button_left = Int32.logand state Sdl.Button.lmask |> Int32.equal 0l |> not;
      button_middle = Int32.logand state Sdl.Button.mmask |> Int32.equal 0l |> not;
      button_right = Int32.logand state Sdl.Button.rmask |> Int32.equal 0l |> not;
      button_x1 = Int32.logand state Sdl.Button.x1mask |> Int32.equal 0l |> not;
      button_x2 = Int32.logand state Sdl.Button.x2mask |> Int32.equal 0l |> not;
      x = E.get event E.mouse_motion_x;
      y = E.get event E.mouse_motion_y;
      xrel = E.get event E.mouse_motion_xrel;
      yrel = E.get event E.mouse_motion_yrel;
    }

  let decode_mouse_wheel () =
    let direction =
      (* Abstract away the direction to simplify the use of this event. *)
      (* Using polymorphic equality here because type [mouse_wheel_direction]
         has no other way to be used. *)
      (* Note: the SDL documentation seems to be wrong here?
         It seems that on my laptop the X direction is reversed compared to the doc. *)
      if E.get event E.mouse_wheel_direction = E.mouse_wheel_flipped then -1 else 1
    in
    {
      timestamp = decode_timestamp ();
      window_id = E.get event E.mouse_wheel_window_id;
      which = E.get event E.mouse_wheel_which |> Int32.to_int;
      x = E.get event E.mouse_wheel_x * direction;
      y = E.get event E.mouse_wheel_y * direction;
    }

  let decode_window () =
    {
      timestamp = decode_timestamp ();
      window_id = E.get event E.window_window_id;
    }

  let decode () =
    let typ = E.get event E.typ in
    if typ = E.quit then
      Quit { timestamp = decode_timestamp () }
    else if typ = E.key_down then
      Key_down (decode_keyboard ())
    else if typ = E.key_up then
      Key_up (decode_keyboard ())
    else if typ = E.window_event then
      (
        let event_id = E.get event E.window_event_id in
        if event_id = E.window_event_shown then
          Window_shown (decode_window ())
        else if event_id = E.window_event_hidden then
          Window_hidden (decode_window ())
        else if event_id = E.window_event_exposed then
          Window_exposed (decode_window ())
        else if event_id = E.window_event_moved then
          Window_moved (decode_window ())
        else if event_id = E.window_event_resized then
          Window_resized (decode_window ())
        else if event_id = E.window_event_size_changed then
          Window_size_changed (decode_window ())
        else if event_id = E.window_event_minimized then
          Window_minimized (decode_window ())
        else if event_id = E.window_event_maximized then
          Window_maximized (decode_window ())
        else if event_id = E.window_event_restored then
          Window_restored (decode_window ())
        else if event_id = E.window_event_enter then
          Window_enter (decode_window ())
        else if event_id = E.window_event_leave then
          Window_leave (decode_window ())
        else if event_id = E.window_event_focus_gained then
          Window_focus_gained (decode_window ())
        else if event_id = E.window_event_focus_lost then
          Window_focus_lost (decode_window ())
        else if event_id = E.window_event_close then
          Window_close (decode_window ())
        else if event_id = E.window_event_take_focus then
          Window_take_focus (decode_window ())
        else
          (* Note: we don't yet support the hit test event because
             this would be useless without supporting SDL_SetWindowHitTest too. *)
          Unsupported
      )
    else if typ = E.mouse_button_down then
      Mouse_button_down (decode_mouse_button ())
    else if typ = E.mouse_button_up then
      Mouse_button_up (decode_mouse_button ())
    else if typ = E.mouse_motion then
      Mouse_motion (decode_mouse_motion ())
    else if typ = E.mouse_wheel then
      Mouse_wheel (decode_mouse_wheel ())
    else
      Unsupported

  let poll () =
    if Sdl.poll_event (Some event) then
      Some (decode ())
    else
      None

  let wait () =
    Sdl.wait_event (Some event) |> or_raise;
    decode ()

  let wait_timeout timeout =
    if Sdl.wait_event_timeout (Some event) timeout then
      Some (decode ())
    else
      None
end

module Texture_cache =
struct
  module type S =
  sig
    type key
    val get: key -> (unit -> Texture.static) -> Texture.static
    val free: key -> unit
    val clear: unit -> unit
    val next_frame: unit -> unit
  end

  let clear_functions = ref []
  let next_frame_functions = ref []

  let clear_all () =
    List.iter (fun f -> f ()) !clear_functions

  let next_frame_all () =
    List.iter (fun f -> f ()) !next_frame_functions

  module Make (Key: Map.OrderedType): S with type key = Key.t =
  struct
    type key = Key.t

    module Cache = Map.Make (Key)

    (* Invariant: a key can only be in one of [current_frame] or [previous_frame],
       not in both. *)
    let previous_frame = ref Cache.empty
    let current_frame = ref Cache.empty

    let get key (make: unit -> Texture.static) =
      (* Look into the [previous_frame] first, because we are more likely to find it
         there than in [current_frame]. Indeed, textures are often only used once
         per frame. *)
      match Cache.find_opt key !previous_frame with
        | Some texture ->
            current_frame := Cache.add key texture !current_frame;
            previous_frame := Cache.remove key !previous_frame;
            texture
        | None ->
            match Cache.find_opt key !current_frame with
              | Some texture ->
                  texture
              | None ->
                  let texture = make () in
                  current_frame := Cache.add key texture !current_frame;
                  texture

    let free key =
      match Cache.find_opt key !previous_frame with
        | Some texture ->
            previous_frame := Cache.remove key !previous_frame;
            Texture.free texture
        | None ->
            match Cache.find_opt key !current_frame with
              | Some texture ->
                  current_frame := Cache.remove key !current_frame;
                  Texture.free texture
              | None ->
                  ()

    let clear_cache cache =
      Cache.iter (fun _ texture -> Texture.free texture) !cache;
      cache := Cache.empty

    let clear () =
      clear_cache previous_frame;
      clear_cache current_frame

    let next_frame () =
      clear_cache previous_frame;
      previous_frame := !current_frame;
      current_frame := Cache.empty

    let () =
      clear_functions := clear :: !clear_functions;
      next_frame_functions := next_frame :: !next_frame_functions
  end

  type k = ..

  module K =
  struct
    type t = k
    let compare = (Stdlib.compare: t -> t -> int)
  end

  include Make (K)
end

module Font =
struct
  open Tsdl_ttf

  type t =
    {
      id: int;
      sdl_font: Ttf.font;
      mutable alive: bool;
    }

  type hinting =
    | Normal
    | Light
    | Mono
    | None

  let free font =
    if font.alive then (
      Ttf.close_font font.sdl_font;
      font.alive <- false;
    )

  let fresh_id =
    let next_id = ref 0 in
    fun () ->
      let id = !next_id in
      incr next_id;
      id

  let initialized = ref false

  let load
    ?(bold = false)
    ?(italic = false)
    ?(underline = false)
    ?(strikethrough = false)
    ?(outline = 0)
    ?(kerning = true)
    ?(hinting = Normal)
    ~size
    filename =
    if not !initialized then (
      Ttf.init () |> or_raise;
      initialized := true;
    );
    let font = Ttf.open_font filename size |> or_raise in
    let style = Ttf.Style.normal in
    let style = if bold then Ttf.Style.((+) bold) style else style in
    let style = if italic then Ttf.Style.((+) italic) style else style in
    let style = if underline then Ttf.Style.((+) underline) style else style in
    let style = if strikethrough then Ttf.Style.((+) strikethrough) style else style in
    Ttf.set_font_style font style;
    Ttf.set_font_outline font outline;
    Ttf.set_font_kerning font kerning;
    Ttf.set_font_hinting font (
      match hinting with
        | Normal -> Normal
        | Light -> Light
        | Mono -> Mono
        | None -> None
    );
    let font =
      {
        id = fresh_id ();
        sdl_font = font;
        alive = true;
      }
    in
    Gc.finalise free font;
    font

  let render ?wrap { sdl_font; _ } ~r ~g ~b ~a text =
    let color = Sdl.Color.create ~r ~g ~b ~a in
    let sdl_surface =
      match wrap with
        | None ->
            Ttf.render_utf8_blended sdl_font text color
            |> or_raise
        | Some wrap ->
            Ttf.render_utf8_blended_wrapped sdl_font text color (Int32.of_int wrap)
            |> or_raise
    in
    (* Format is ARGB8888 according to SDL_ttf's documentation. *)
    Surface.create_from_sdl_surface ARGB8888 sdl_surface

  let render_texture ?window ?wrap font ~r ~g ~b ~a text =
    let surface = render ?wrap font ~r ~g ~b ~a text in
    let texture =
      match window with
        | None ->
            Render.create_texture_from_surface surface
        | Some window ->
            Texture.create_from_surface window surface
    in
    Surface.free surface;
    texture

  type Texture_cache.k +=
    | Text of {
        window_id: int;
        wrap: int option;
        font_id: int;
        r: int; g: int; b: int; a: int;
        text: string;
      }

  let render_cached ?window ?wrap font ~r ~g ~b ~a text =
    let window =
      match window with
        | None ->
            Render.get_window_exn ()
        | Some window ->
            window
    in
    Texture_cache.get
      (
        Text {
          window_id = Window.id window;
          wrap;
          font_id = font.id;
          r; g; b; a;
          text;
        }
      )
    @@ fun () ->
    render_texture ~window ?wrap font ~r ~g ~b ~a text

  let size_text font text =
    Ttf.size_utf8 font.sdl_font text |> or_raise

  let height font = Ttf.font_height font.sdl_font
  let ascent font = Ttf.font_ascent font.sdl_font
  let descent font = Ttf.font_descent font.sdl_font
  let line_skip font = Ttf.font_line_skip font.sdl_font
end

(* Note: Sdl.quit only restores resolution if we initialized the video subsystem.
   If we don't, destroying the window does restore the resolution though.
   But we do initialize the video subsystem when we create a window. *)
let quit () =
  Sdl.quit ();
  if !Font.initialized then Tsdl_ttf.Ttf.quit ()

module Main_loop =
struct
  type state =
    {
      mutable modified: bool;
      (* [last_logical_frame] is the time at which the last logical frame
         *should* have been computed, not the actual time it was computed,
         so that the apparent logical frame rate is as consistent as possible. *)
      mutable last_logical_frame: int;
      (* [last_draw] is the time *after* the draw (closest to [present]). *)
      mutable last_draw: int;
      mutable continue: bool;
    }

  let run
      ?(handle_event = fun _ -> false)
      ?logical_delay
      ?(logical_frame = fun () -> false)
      ?(max_logical_catch_up_delay = 200)
      ?draw_delay
      ?(draw = fun () -> ())
      ?max_freeze
      ?continue
      ?window
      () =
    (* Make sure we start with something on the screen. *)
    let start = Timer.get_ticks () in
    draw ();
    let state =
      let now = Timer.get_ticks () in
      {
        modified = false;
        last_logical_frame = start;
        last_draw = now;
        continue = true;
      }
    in
    let should_continue =
      match continue with
        | None -> (fun () -> state.continue)
        | Some continue -> continue
    in
    let handle_event (event: Event.t) =
      match event with
        | Quit _ ->
            (
              match continue with
                | None ->
                    state.continue <- false
                | Some _ ->
                    if handle_event event then state.modified <- true
            )
        | Window_exposed _
        | Window_size_changed _ ->
            let _: bool = handle_event event in
            state.modified <- true
        | Unsupported ->
            ()
        | _ ->
            if handle_event event then state.modified <- true
    in
    let maybe_call_logical_frame () =
      match logical_delay with
        | None ->
            false
        | Some logical_delay ->
            let now = Timer.get_ticks () in
            let next_logical_frame = state.last_logical_frame + logical_delay in
            if now >= next_logical_frame then
              (
                if logical_frame () then state.modified <- true;
                let new_last_logical_frame =
                  max
                    (now - logical_delay - max_logical_catch_up_delay)
                    next_logical_frame
                in
                state.last_logical_frame <- new_last_logical_frame;
                true
              )
            else
              false
    in
    let draw_now () =
      state.modified <- false;
      (
        match window with
          | None ->
              ()
          | Some window ->
              Render.set_target (Window window);
              Render.set_draw_color ~r: 0 ~g: 0 ~b: 0;
              Render.clear ()
      );
      draw ();
      (
        match window with
          | None ->
              ()
          | Some window ->
              Render.set_target (Window window);
              Render.present ()
      );
      let now = Timer.get_ticks () in
      state.last_draw <- now;
      Texture_cache.next_frame_all ()
    in
    let maybe_call_draw ~idle =
      (* If we're past [draw_delay], consider that there is something to draw. *)
      (
        match draw_delay with
          | None ->
              ()
          | Some draw_delay ->
              let now = Timer.get_ticks () in
              if now >= state.last_draw + draw_delay then state.modified <- true
      );
      (* Only draw if there is something to draw. *)
      if state.modified then (
        if idle then
          (* There are things to draw and we have time: draw. *)
          draw_now ()
        else
          match max_freeze with
            | None ->
                (* We don't care about freezes: don't try to force a draw. *)
                ()
            | Some max_freeze ->
                (* Draw only if [max_freeze] was reached. *)
                let now = Timer.get_ticks () in
                if now >= state.last_draw + max_freeze then draw_now ()
      )
    in
    while should_continue () do
      (* Regularly check, in-between events and logical frames,
         if we're past [max_freeze], in which case we must draw now. *)
      maybe_call_draw ~idle: false;
      match Event.poll () with
        | Some event ->
            handle_event event
        | None ->
            if not (maybe_call_logical_frame ()) then (
              (* Idle: no event and no logical frame to process.
                 Use this opportunity to draw if there is something to draw. *)
              maybe_call_draw ~idle: true;
              (* Then passively wait for:
                 - events (if we happen to receive one we process it immediately);
                 - logical frame timeout or draw timeout (handled by next iteration). *)
              let now = Timer.get_ticks () in
              let timeout =
                match logical_delay, draw_delay with
                  | None, None ->
                      None
                  | Some logical_delay, None ->
                      Some (state.last_logical_frame + logical_delay - now)
                  | None, Some draw_delay ->
                      Some (state.last_draw + draw_delay - now)
                  | Some logical_delay, Some draw_delay ->
                      Some (
                        min
                          (state.last_logical_frame + logical_delay - now)
                          (state.last_draw + draw_delay - now)
                      )
              in
              match timeout with
                | None ->
                    let event = Event.wait () in
                    handle_event event
                | Some timeout ->
                    if timeout > 0 then
                      match Event.wait_timeout timeout with
                        | None ->
                            ()
                        | Some event ->
                            handle_event event
            )
    done;
    Texture_cache.clear_all ();
    match window with
      | None ->
          ()
      | Some window ->
          Window.free window;
          quit ()
end
