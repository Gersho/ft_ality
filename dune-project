(lang dune 3.8)

(name ft_ality)

(generate_opam_files true)

(source
 (github Gersho/ft_ality))

(authors "Simon Cros" "Karim Zennoune")

(maintainers "Simon Cros" "Karim Zennoune")

(license MIT)

(documentation https://github.com/Gersho/ft_ality)

(package
 (name ft_ality)
 (synopsis "A functional state machine")
 (description "A state machine in functional programming reading keyboard input and displaying corresponding moves based on a provided grammar file")
 (depends ocaml dune ocamlsdl2)
 (tags
  ("42"
  "ocaml"
  "ocamlsdl2"
  "functional programming"
  "Chomsky hierarchy"
  "type 3 languages"
  "regular languages"
  "finite-state automata"
  "finite-state machines")))
