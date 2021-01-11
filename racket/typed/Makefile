pan : pan.c
	gcc -o pan pan.c

pan.c : leader-and-seller.pml
	spin -a leader-and-seller.pml

# -a to analyze, -f for (weak) fairness
# -n to elide report of unreached states
# -N spec-name to verify a particular specification
check: pan
	./pan -a -f -n
