open Js_of_ocaml

module String_map = Map.Make (String)

exception Error of string

let error message = raise (Error message)

(* let quit () = () *)

(* module Hint = *)
(* struct *)
(*   let render_vsync _ = false *)
(* end *)

type node = Dom.node Js.t

module Timer =
struct
  let milliseconds_since_epoch () = Js.date##now |> int_of_float
  let start = milliseconds_since_epoch ()
  let get_ticks () = milliseconds_since_epoch () - start
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

  (* TODO: we may want to be able to have a text field
     to distinguish lower and upper cases. How to do that in SDL too? *)
  let of_js = function
    | "a" | "A" -> A
    | "b" | "B" -> B
    | "c" | "C" -> C
    | "d" | "D" -> D
    | "e" | "E" -> E
    | "f" | "F" -> F
    | "g" | "G" -> G
    | "h" | "H" -> H
    | "i" | "I" -> I
    | "j" | "J" -> J
    | "k" | "K" -> K
    | "l" | "L" -> L
    | "m" | "M" -> M
    | "n" | "N" -> N
    | "o" | "O" -> O
    | "p" | "P" -> P
    | "q" | "Q" -> Q
    | "r" | "R" -> R
    | "s" | "S" -> S
    | "t" | "T" -> T
    | "u" | "U" -> U
    | "v" | "V" -> V
    | "w" | "W" -> W
    | "x" | "X" -> X
    | "y" | "Y" -> Y
    | "z" | "Z" -> Z
    | "Shift" -> Lshift (* Actually also Rshift but... *)
    | "Control" -> Lctrl
    | "Alt" -> Lalt
    | "AltGraph" -> Ralt
    | "OS" -> Lgui
    | "ArrowLeft" -> Left
    | "ArrowRight" -> Right
    | "ArrowUp" -> Up
    | "ArrowDown" -> Down
    | " " -> Space
    | "F1" -> F1
    | "F2" -> F2
    | "F3" -> F3
    | "F4" -> F4
    | "F5" -> F5
    | "F6" -> F6
    | "F7" -> F7
    | "F8" -> F8
    | "F9" -> F9
    | "F10" -> F10
    | "F11" -> F11
    | "F12" -> F12
    | "Enter" -> Return
    | "Backspace" -> Backspace
    | "Tab" -> Tab
    | "PageUp" -> Pageup
    | "PageDown" -> Pagedown
    | "Home" -> Home
    | "End" -> End
    | "1" -> K1 (* TODO: check that in SDL it's actually K1 *)
    | "2" -> K2
    | "3" -> K3
    | "4" -> K4
    | "5" -> K5
    | "6" -> K6
    | "7" -> K7
    | "8" -> K8
    | "9" -> K9
    | "0" -> K0
    | "-" -> Minus (* TODO: check in SDL *)
    | "=" -> Equals (* TODO: check in SDL *)
    | code -> Unsupported code (* TODO *)
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

  let of_js = function
    | "KeyA" -> A
    | "KeyB" -> B
    | "KeyC" -> C
    | "KeyD" -> D
    | "KeyE" -> E
    | "KeyF" -> F
    | "KeyG" -> G
    | "KeyH" -> H
    | "KeyI" -> I
    | "KeyJ" -> J
    | "KeyK" -> K
    | "KeyL" -> L
    | "KeyM" -> M
    | "KeyN" -> N
    | "KeyO" -> O
    | "KeyP" -> P
    | "KeyQ" -> Q
    | "KeyR" -> R
    | "KeyS" -> S
    | "KeyT" -> T
    | "KeyU" -> U
    | "KeyV" -> V
    | "KeyW" -> W
    | "KeyX" -> X
    | "KeyY" -> Y
    | "KeyZ" -> Z
    | "ShiftLeft" -> Lshift
    | "ShiftRight" -> Rshift
    | "ControlLeft" -> Lctrl
    | "ControlRight" -> Rctrl
    | "AltLeft" -> Lalt
    | "AltRight" -> Ralt
    | "OSLeft" -> Lgui
    | "OSRight" -> Rgui
    | "ArrowLeft" -> Left
    | "ArrowRight" -> Right
    | "ArrowUp" -> Up
    | "ArrowDown" -> Down
    | "Space" -> Space
    | "F1" -> F1
    | "F2" -> F2
    | "F3" -> F3
    | "F4" -> F4
    | "F5" -> F5
    | "F6" -> F6
    | "F7" -> F7
    | "F8" -> F8
    | "F9" -> F9
    | "F10" -> F10
    | "F11" -> F11
    | "F12" -> F12
    | "Enter" -> Return
    | "Backspace" -> Backspace
    | "Tab" -> Tab
    | "PageUp" -> Pageup
    | "PageDown" -> Pagedown
    | "Home" -> Home
    | "End" -> End
    | "Digit1" -> K1 (* TODO: check that in SDL it's actually K1 *)
    | "Digit2" -> K2
    | "Digit3" -> K3
    | "Digit4" -> K4
    | "Digit5" -> K5
    | "Digit6" -> K6
    | "Digit7" -> K7
    | "Digit8" -> K8
    | "Digit9" -> K9
    | "Digit0" -> K0
    | "Minus" -> Minus (* TODO: check in SDL *)
    | "Equal" -> Equals (* TODO: check in SDL *)
    | code -> Unsupported code (* TODO *)
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
end

module Event =
struct
  type button_state = Pressed | Released

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

  type t =
    | Key_down of keyboard
    | Key_up of keyboard
    | Mouse_button_down of mouse_button
    | Mouse_button_up of mouse_button
    | Mouse_motion of mouse_motion
    | Mouse_wheel of mouse_wheel

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
        timestamp: int;
        window_id: int;
        which: int;
        button: Mouse_button.t;
        state: button_state;
        clicks: int;
        x: int;
        y: int;
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

  let show = function
    | Key_down event ->
        show_keyboard_event "Key_down" event
    | Key_up event ->
        show_keyboard_event "Key_up" event
    | Mouse_button_down event ->
        show_mouse_button_event "Mouse_button_down" event
    | Mouse_button_up event ->
        show_mouse_button_event "Mouse_button_up" event
    | Mouse_motion {
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
        timestamp: int;
        window_id: int;
        which: int;
        x: int;
        y: int;
      } ->
        show_event "Mouse_wheel" [
          "timestamp", string_of_int timestamp;
          "window_id", string_of_int window_id;
          "which", string_of_int which;
          "x", string_of_int x;
          "y", string_of_int y;
        ]

  let queue: t Queue.t = Queue.create ()

  let on_event = ref (fun () -> ())

  let push event =
    Queue.add event queue;
    !on_event ()

  let poll () = Queue.take_opt queue

  (* TODO: if wait and wait_timeout expected a continuation,
     we could have them for JS too. *)
end

module Window =
struct
  type t =
    {
      id: int;
      (* [w, h] are the size of [canvas]. *)
      mutable w: int;
      mutable h: int;
      auto_resize_canvas: bool;
      mutable alive: bool;
      mutable parent: node option;
      canvas: Dom_html.canvasElement Js.t;
      renderer: Dom_html.canvasRenderingContext2D Js.t;
      mutable scale_x: float;
      mutable scale_y: float;
      mutable clip_rect: (float * float * float * float) option;
      mutable viewport: (float * float * float * float) option;
      mutable mod_lshift: bool;
      mutable mod_rshift: bool;
      mutable mod_lctrl: bool;
      mutable mod_rctrl: bool;
      mutable mod_lalt: bool;
      mutable mod_ralt: bool;
      mutable mod_lgui: bool;
      mutable mod_rgui: bool;
      mutable mouse_x: int;
      mutable mouse_y: int;
    }

  let next_id = ref 0
  let on_create_first_window = ref (fun _ -> ())

  let free window =
    if window.alive then (
      window.alive <- false;
      match window.parent with
        | None ->
            ()
        | Some parent ->
            let _: node = parent##removeChild (window.canvas :> node) in
            window.parent <- None;
            ()
    )

  let setup_events window =
    let should_stop_propagation (event: Event.t) =
      match event with
        | Key_down kb | Key_up kb ->
            (
              match kb.key_code with
                | F5 | F12 ->
                    (* Let users refresh and open dev tools. *)
                    false
                | L when kb.mod_ctrl ->
                    (* Let users move to the address bar. *)
                    false
                | _ ->
                    true
            )
        | _ ->
            true
    in
    let push js_event event =
      Event.push event;
      if should_stop_propagation event then
        (
          Dom_html.stopPropagation js_event;
          Js._false
        )
      else
        Js._true
    in
    let decode_keyboard (state: Event.button_state) (event: Dom_html.keyboardEvent Js.t):
      Event.keyboard =
      let code = Js.Optdef.case event##.code (fun () -> "") Js.to_string in
      let key = Js.Optdef.case event##.key (fun () -> "") Js.to_string in
      let scan_code = Scan_code.of_js code in
      let key_code = Key_code.of_js key in
      (
        let pressed = match state with Pressed -> true | Released -> false in
        match scan_code with
          | Lshift -> window.mod_lshift <- pressed
          | Rshift -> window.mod_rshift <- pressed
          | Lctrl -> window.mod_lctrl <- pressed
          | Rctrl -> window.mod_rctrl <- pressed
          | Lalt -> window.mod_lalt <- pressed
          | Ralt -> window.mod_ralt <- pressed
          | Lgui -> window.mod_lgui <- pressed
          | Rgui -> window.mod_rgui <- pressed
          | _ -> ()
      );
      {
        timestamp = Timer.get_ticks ();
        window_id = window.id;
        state;
        repeat = false (* TODO: but event##.repeat is not bound *);
        scan_code;
        key_code;
        mod_lshift = window.mod_lshift;
        mod_rshift = window.mod_rshift;
        mod_lctrl = window.mod_lctrl;
        mod_rctrl = window.mod_rctrl;
        mod_lalt = window.mod_lalt;
        mod_ralt = window.mod_ralt;
        mod_lgui = window.mod_lgui;
        mod_rgui = window.mod_rgui;
        mod_num = false (* TODO *);
        mod_caps = false (* TODO *);
        mod_mode = false (* TODO: but even in the SDL version this doesn't really work *);
        mod_ctrl = Js.to_bool event##.ctrlKey;
        mod_shift = Js.to_bool event##.shiftKey;
        mod_alt = Js.to_bool event##.altKey;
        mod_gui = Js.to_bool event##.metaKey;
      }
    in
    let on_key_down (event: Dom_html.keyboardEvent Js.t)  =
      push event (Key_down (decode_keyboard Pressed event))
    in
    let on_key_up (event: Dom_html.keyboardEvent Js.t)  =
      push event (Key_up (decode_keyboard Released event))
    in
    let decode_mouse_button state (event: Dom_html.mouseEvent Js.t): Event.mouse_button =
      (* TODO: in JS we have access to mod_ctrl etc., so it would be nice to add it,
         and it could be useful in SDL too. *)
      let rect = window.canvas##getBoundingClientRect in
      {
        timestamp = Timer.get_ticks ();
        window_id = window.id;
        which = 0; (* TODO *)
        button = (
          match event##.button with
            | 0 -> Left
            | 1 -> Middle
            | 2 -> Right
            | i -> Button i
        );
        state;
        clicks = 0; (* TODO: event##.detail but not bound *)
        x = event##.clientX - int_of_float rect##.left;
        y = event##.clientY - int_of_float rect##.top;
      }
    in
    let on_mouse_down (event: Dom_html.mouseEvent Js.t) =
      push event (Mouse_button_down (decode_mouse_button Pressed event))
    in
    let on_mouse_up (event: Dom_html.mouseEvent Js.t) =
      push event (Mouse_button_up (decode_mouse_button Pressed event))
    in
    let on_mouse_move (event: Dom_html.mouseEvent Js.t) =
      let buttons = 0 (* TODO: event##.buttons but not bound *) in
      let rect = window.canvas##getBoundingClientRect in
      let x = event##.clientX - int_of_float rect##.left in
      let y = event##.clientY - int_of_float rect##.top in
      let old_x = window.mouse_x in
      window.mouse_x <- x;
      let old_y = window.mouse_y in
      window.mouse_y <- y;
      push event @@ Mouse_motion {
        timestamp = Timer.get_ticks ();
        window_id = window.id;
        which = 0; (* TODO *)
        (* Note: technically button 1 is not "left" but "primary" here. *)
        button_left = buttons land 1 <> 0;
        (* Note: technically button 4 is not "middle" but "auxiliary" here. *)
        button_middle = buttons land 4 <> 0;
        (* Note: technically button 2 is not "right" but "secondary" here. *)
        button_right = buttons land 2 <> 0;
        button_x1 = buttons land 8 <> 0;
        button_x2 = buttons land 16 <> 0;
        x;
        y;
        xrel = x - old_x;
        yrel = y - old_y;
      }
    in
    let maybe_auto_resize_canvas w h =
      if window.auto_resize_canvas then (
        window.w <- w;
        window.h <- h;
        window.canvas##.width := w;
        window.canvas##.height := h;
      )
    in
    let get_size () =
      let w = Dom_html.window##.innerWidth in
      let h = Dom_html.window##.innerHeight in
      w, h
    in
    let on_resize (_: Dom_html.event Js.t) =
      let w, h = get_size () in
      maybe_auto_resize_canvas w h;
      (* TODO: push event *)
      Js._true
    in
    (* TODO: put keyboard events on window.canvas instead, but how to allow
       it to get the focus? *)
    Dom_html.document##.onkeydown := Dom.handler on_key_down;
    Dom_html.document##.onkeyup := Dom.handler on_key_up;
    window.canvas##.onmousedown := Dom.handler on_mouse_down;
    window.canvas##.onmouseup := Dom.handler on_mouse_up;
    window.canvas##.onmousemove := Dom.handler on_mouse_move;
    Dom_html.window##.onresize := Dom.handler on_resize;
    (* TODO: Mouse_wheel, but onmousewheel seems not bound *)
    let w, h = get_size () in
    maybe_auto_resize_canvas w h;
    ()

  let create ?(resizable = false) ~w ~h () =
    let id = !next_id in
    incr next_id;
    let canvas = Dom_html.createCanvas Dom_html.document in
    canvas##.width := w;
    canvas##.height := h;
    let renderer = canvas##getContext Dom_html._2d_ in
    (* We maintain the invariant that the save/restore stack always has one element in it.
       This is so that we can implement [set_clip_rect] etc.
       This invariant is used by [apply_transformations]. *)
    renderer##save;
    let window =
      {
        id;
        w;
        h;
        auto_resize_canvas = resizable;
        alive = true;
        parent = None;
        canvas;
        renderer;
        scale_x = 1.;
        scale_y = 1.;
        clip_rect = None;
        viewport = None;
        mod_lshift = false;
        mod_rshift = false;
        mod_lctrl = false;
        mod_rctrl = false;
        mod_lalt = false;
        mod_ralt = false;
        mod_lgui = false;
        mod_rgui = false;
        mouse_x = 0;
        mouse_y = 0;
      }
    in
    let on_load _ =
      if window.alive then (
        let parent_id = "fungame_window_" ^ string_of_int id in
        Js.Opt.case
          (Dom_html.window##.document##getElementById (Js.string parent_id))
          (fun () -> error ("no element with id: " ^ parent_id))
        @@ fun parent ->
        window.parent <- Some (parent :> node);
        let _: node = parent##appendChild (canvas :> node) in
        setup_events window;
        ()
      );
      Js._true
    in
    Dom_html.window##.onload := Dom.handler on_load;
    if id = 0 then !on_create_first_window window;
    window

  let id window = window.id
  let w window = window.w
  let h window = window.h
  let size window = w window, h window
end

module Render =
struct
  type target =
    | Nowhere
    | Window of Window.t

  let target = ref Nowhere
  let get_target () = !target
  let set_target new_target = target := new_target
  let () = Window.on_create_first_window := (fun window -> set_target (Window window))

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

  let get_window () =
    match !target with
      | Nowhere ->
          None
      | Window window ->
          Some window

  let get_window_exn () =
    match get_window () with
      | None ->
          error "no current rendering target (tip: create a window first)"
      | Some window ->
          window

  (* TODO: can we / should we do double buffering? *)
  let present () = ()

  (* https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D *)

  (* Note: if we set fillStyle and strokeStyle instead,
     relying on the canvas to remember the color,
     we would need to restore the value in [apply_transformations]
     after [renderer##restore]. *)
  let draw_color = ref (Js.string "#000000")

  let set_draw_color ~r ~g ~b =
    draw_color := Js.string (Printf.sprintf "#%02x%02x%02x" r g b)

  let snapshot_transformations window =
    let
      {
        (* Things we want to snapshot. *)
        scale_x;
        scale_y;
        clip_rect;
        viewport;
        (* Things we don't want to snapshot. *)
        Window.id = _;
        w = _;
        h = _;
        auto_resize_canvas = _;
        alive = _;
        parent = _;
        canvas = _;
        renderer = _;
        mod_lshift = _;
        mod_rshift = _;
        mod_lctrl = _;
        mod_rctrl = _;
        mod_lalt = _;
        mod_ralt = _;
        mod_lgui = _;
        mod_rgui = _;
        mouse_x = _;
        mouse_y = _;
      }
      = window
    in
    scale_x, scale_y, clip_rect, viewport

  let apply_transformations (window: Window.t) =
    let renderer = window.renderer in
    (* We rely on the invariant described in [create]. *)
    renderer##restore;
    renderer##save;
    (* Clip rect (intersection of [clip_rect] and [viewport]). *)
    let clip_rect = function
      | None ->
          ()
      | Some (x, y, w, h) ->
          renderer##beginPath;
          renderer##rect x y w h;
          renderer##clip
    in
    clip_rect window.clip_rect;
    clip_rect window.viewport;
    (* Translation. *)
    (
      match window.viewport with
        | None ->
            ()
        | Some (x, y, _, _) ->
            renderer##translate x y
    );
    (* Scale. *)
    renderer##scale window.scale_x window.scale_y

  let restore_transformations (window: Window.t) (scale_x, scale_y, clip_rect, viewport) =
    window.scale_x <- scale_x;
    window.scale_y <- scale_y;
    window.clip_rect <- clip_rect;
    window.viewport <- viewport;
    apply_transformations window

  let clear () =
    match !target with
      | Nowhere ->
          ()
      | Window window ->
          let old_transformations = snapshot_transformations window in
          Fun.protect
            ~finally: (fun () -> restore_transformations window old_transformations)
          @@ fun () ->
          window.clip_rect <- None;
          window.viewport <- None;
          window.scale_x <- 1.;
          window.scale_y <- 1.;
          apply_transformations window;
          window.renderer##.fillStyle := !draw_color;
          window.renderer##fillRect 0. 0. (float window.w) (float window.h)

  let draw_point ~x ~y =
    match !target with
      | Nowhere ->
          ()
      | Window window ->
          window.renderer##.fillStyle := !draw_color;
          window.renderer##fillRect (float x) (float y) 1. 1.

  let draw_line ~x1 ~y1 ~x2 ~y2 =
    match !target with
      | Nowhere ->
          ()
      | Window window ->
          window.renderer##.strokeStyle := !draw_color;
          window.renderer##beginPath;
          window.renderer##moveTo (float x1 +. 0.5) (float y1 +. 0.5);
          window.renderer##lineTo (float x2 +. 0.5) (float y2 +. 0.5);
          window.renderer##stroke

  let draw_rect ~x ~y ~w ~h =
    match !target with
      | Nowhere ->
          ()
      | Window window ->
          window.renderer##.strokeStyle := !draw_color;
          window.renderer##strokeRect (float x +. 0.5) (float y +. 0.5) (float w) (float h)

  let fill_rect ~x ~y ~w ~h =
    match !target with
      | Nowhere ->
          ()
      | Window window ->
          window.renderer##.fillStyle := !draw_color;
          window.renderer##fillRect (float x) (float y) (float w) (float h)

  (* Note: with negative numbers, this flips coordinates.
     So we could use this to implement [hflip] and [vflip] in [copy]. *)
  let set_scale ~x ~y =
    match !target with
      | Nowhere ->
          ()
      | Window window ->
          window.scale_x <- x;
          window.scale_y <- y;
          apply_transformations window

  let set_clip_rect ~x ~y ~w ~h =
    match !target with
      | Nowhere ->
          ()
      | Window window ->
          let sx = window.scale_x and sy = window.scale_y in
          let x = float x and y = float y and w = float w and h = float h in
          let x = x *. sx and y = y *. sy in
          let x, y =
            match window.viewport with
              | None -> x, y
              | Some (dx, dy, _, _) -> x +. dx, y +. dy
          in
          window.clip_rect <- Some (x, y, w *. sx, h *. sy);
          apply_transformations window

  let unset_clip_rect () =
    match !target with
      | Nowhere ->
          ()
      | Window window ->
          window.clip_rect <- None;
          apply_transformations window

  let set_viewport ~x ~y ~w ~h =
    match !target with
      | Nowhere ->
          ()
      | Window window ->
          let sx = window.scale_x and sy = window.scale_y in
          let x = float x and y = float y and w = float w and h = float h in
          window.viewport <- Some (x *. sx, y *. sy, w *. sx, h *. sy);
          apply_transformations window

  let unset_viewport () =
    match !target with
      | Nowhere ->
          ()
      | Window window ->
          window.viewport <- None;
          apply_transformations window

  (* TODO: textures (JS = ImageBitmap, but how to use that with js_of_ocaml?
     do I need my own binding? => looks like so, so probably should do that later,
     but the doc is at https://ocsigen.org/js_of_ocaml/latest/manual/bindings).
     https://developer.mozilla.org/en-US/docs/Web/API/createImageBitmap
     Apparently one way to create an ImageBitmap is from a variety of objects,
     including ImageData which is the underlying object of a canvas
     (can be used to *read* the renderer),
     and HTMLImageElement, i.e. <img>, which we probably want to define
     as surfaces? But not exactly because I'm not sure we can modify them?
     So not sure what to use for surfaces that we want to build pixel by pixel.
     Note: <img> can also be used to render directly, but apparently it's maybe
     less efficient than ImageBitmap. *)
end

module Main_loop =
struct
  let run
      ?(handle_event = fun (_: Event.t) -> false)
      ?logical_delay
      ?(logical_frame = fun () -> false)
      ?(max_logical_catch_up_delay = 200)
      ?draw_delay
      ?(draw = fun () -> ())
      () =
    let now = Timer.get_ticks () in
    let last_draw = ref now in
    let need_to_draw =
      let pending = ref false in
      fun () ->
        (* Not sure requestAnimationFrame is really needed but it should be a way
           to make sure we don't try to draw too many times, for instance if we
           handle 10 events at once, we could draw only after we process them? *)
        if not !pending then
          let _: Dom_html.animation_frame_request_id =
            let callback (_: float) =
              pending := false;
              draw ();
              let now = Timer.get_ticks () in
              last_draw := now
            in
            Dom_html.window##requestAnimationFrame (Js.wrap_callback callback)
          in
          ()
    in
    (* Event handling. *)
    let on_event () =
      match Event.poll () with
        | None ->
            ()
        | Some event ->
            if handle_event event then need_to_draw ()
    in
    Event.on_event := on_event;
    (* Logical frame loop. *)
    (
      match logical_delay with
        | None ->
            ()
        | Some logical_delay ->
            let last_logical_frame = ref now in
            let rec logical_frame_loop () =
              let now = Timer.get_ticks () in
              let timeout = !last_logical_frame + logical_delay - now in
              let timeout = max 1 timeout in
              let call_logical_frame () =
                let now = Timer.get_ticks () in
                last_logical_frame :=
                  max
                    (now - logical_delay - max_logical_catch_up_delay)
                    (!last_logical_frame + logical_delay);
                if logical_frame () then need_to_draw ();
                logical_frame_loop ()
              in
              (* Note: if we tried to loop immediately if timeout <= 0,
                 we would risk "too many recursions" error. *)
              let _: Dom_html.timeout_id_safe =
                Dom_html.setTimeout call_logical_frame (float timeout)
              in
              ()
            in
            logical_frame_loop ()
    );
    (* Draw frame loop. *)
    (
      match draw_delay with
        | None ->
            ()
        | Some draw_delay ->
            let rec draw_loop () =
              let now = Timer.get_ticks () in
              let timeout = !last_draw + draw_delay - now in
              let timeout = max 1 timeout in
              let call_draw () =
                (* TODO: we may have already drawn since we set the timeout
                   (because of logical_frame or handle_event), we could avoid
                   re-drawing if it is the case. *)
                need_to_draw ();
                draw_loop ()
              in
              (* Note: if we tried to draw immediately if timeout <= 0,
                 we would risk "too many recursions" error. *)
              let _: Dom_html.timeout_id_safe =
                Dom_html.setTimeout call_draw (float timeout)
              in
              ()
            in
            draw_loop ()
    );
    (* Start with something on the screen. *)
    need_to_draw ()
end
