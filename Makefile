all:

run:
	racketmake main.rkt

clean:
	find . -name compiled -type d | xargs rm -rf
