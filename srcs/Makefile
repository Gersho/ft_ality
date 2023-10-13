build:
	dune build

test:
	dune test

run:
	dune exec ft_ality

exec: build
	./_build/install/default/bin/ft_ality ./grammar/SF_Ryu.gmr

PID = $(shell ps -aux |grep ft_ality | awk {'print $$2'})

kill:
	kill -9 $(PID)
