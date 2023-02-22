#! /bin/sh

# Helper script to execute SRFI test programs.
#
# Written by Akce 2020.
# SPDX-License-Identifier: Unlicense

if [ -z "$1" ]; then
	echo "use: $0 <path-to-libdir>"
	echo ""
	echo "Note that <path-to-libdir> must be the containing directory of the installed srfi libs to test."
	echo "eg,"
	echo "  $ $0 /tmp/chezlibs"
	exit 1
fi

set -o errexit
set -o xtrace

# Set libdirs to library under test only.
export CHEZSCHEMELIBDIRS="$1"

SCHEME=scheme-script

tests='
testing.sps
and-let%2a.sps
ascii.sps
char-sets.sps
compare-procedures.sps
cut.sps
eager-comprehensions.sps
intermediate-format-strings.sps
let.sps
lightweight-testing.sps
list-queues.sps
lists.sps
multi-dimensional-arrays--arlib.sps
os-environment-variables.sps
r6rs-hashtables.sps
random-bits.sps
rec.sps
records.sps
regexp.sps
tables-test.sps
vectors.sps
bitwise-operations.sps
boxes.sps
mapping.sps
mapping-hash.sps
ideques.sps
ilists.sps
lseqs.sps
generators-and-accumulators.sps
fixnums.sps
sets-and-bags.sps
flexvectors.sps
transducers.sps
'

fails='
multi-dimensional-arrays.sps
time.sps
'

# Have to test this one some other way as it requires manual intervention.
#lazy.sps

# Execute tests.

for t in $tests; do
	$SCHEME $t
done
