all:
	dune build
	mv -f ./_build/default/bin/main.exe ./ft_ality

clean:
	rm -rf ./_build

fclean: clean
	rm -rf ft_ality

re: fclean all