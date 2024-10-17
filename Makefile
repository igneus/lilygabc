test:
	ls tests/unit/*_test.scm | GUILE_LOAD_PATH=. xargs -L1 guile

visual_ly:
	cd tests/visual && make ly

visual_tex:
	cd tests/visual && make tex

gold:
	cd tests/gold_standard && ./test.sh
