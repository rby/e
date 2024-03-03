#!/usr/bin/env zsh
for f in test/it_SUITE_data/*.e; do
	cat $f \
	| _build/default/bin/elang 2>/dev/null \
	| tail -n +2 \
	| sed -e 's/Parsed: //g' \
	> ${f/.e/.out}
done
