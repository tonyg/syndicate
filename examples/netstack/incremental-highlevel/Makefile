all:

run:
	raco make main.rkt && racket main.rkt

clean:
	find . -name compiled -type d | xargs rm -rf
	rm -f cpingresp

cpingresp: cpingresp.c
	$(CC) -o $@ $<
	sudo setcap cap_net_raw+p+i+e $@
