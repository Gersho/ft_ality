FROM ocaml/opam:debian-12-ocaml-4.14

RUN sudo apt install -y pkg-config autoconf libsdl1.2-dev

RUN eval $(opam env) && opam install -y ocamlsdl

RUN sudo chown opam *

COPY ./srcs .

CMD sleep infinity