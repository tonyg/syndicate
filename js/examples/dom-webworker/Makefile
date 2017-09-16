all: index.expanded.js worker.expanded.js

%.expanded.js: %.js
	../../bin/syndicatec $< > $@ || (rm -f $@; false)

clean:
	rm -f *.expanded.js
