(lang dune 2.7)
(generate_opam_files true)

(source (github rbardou/fungame2))
(license MIT)
(authors "Romain Bardou")
(maintainers "Romain Bardou")

(package
 (name fungame2)
 (synopsis "Wrapper to simplify the use of SDL2")
 (description "Quickly build simple games with sprites, sound, text rendered using TTF fonts, and a widget engine.")
 (depends
  (ocaml (>= 4.07))
  (tsdl (>= 0.9.6))
  (tsdl_ttf (>= 0.2))
  (tsdl_mixer (>= 0.2))
  (tsdl_image (>= 0.2.0))))
