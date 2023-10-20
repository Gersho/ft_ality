all:
	dune build
	mv -sf ./_build/install/default/bin/ft_ality ./ft_ality

clean:
	rm -rf ./_build

fclean: clean
	rm -rf ft_ality
