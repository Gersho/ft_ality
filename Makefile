all:
	dune build
	cp -f ./_build/install/default/bin/ft_ality ./ft_ality

exec: all
	./ft_ality ./grammar/SF_Ryu.gmr

clean:
	rm -rf ./_build
	rm -rf ft_ality.opam

fclean: clean
	rm -rf ft_ality


