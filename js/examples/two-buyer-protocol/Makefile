TARGETS=index.expanded.js index.md

all: $(TARGETS)

%.expanded.js: %.js
	../../bin/syndicatec $< > $@ || (rm -f $@; false)

%.md: %.js
	sed -E -e 's:^:    :g' -e 's:^    /// ?::g' -e 's:^    $$::g' < $< > $@

clean:
	rm -f $(TARGETS)
