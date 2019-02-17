test:
	# ../../vendor/ocaml/bin/ocamlc.opt -c js.ml
	../../vendor/ocaml/bin/ocamlc.opt -ppx ../../lib/belt_bsppx.exe -I ./ -c test.mli
	../../vendor/ocaml/bin/ocamlc.opt -ppx ../../lib/belt_bsppx.exe -I ./ -c test.ml
	../../vendor/ocaml/bin/ocamlc.opt -ppx ../../lib/belt_bsppx.exe -I ./ -c test_use.ml
	../../vendor/ocaml/bin/ocamlc.opt test.cmo test_use.cmo
	./a.out
	../../vendor/ocaml/bin/ocamlc.opt -dsource -ppx ../../lib/belt_bsppx.exe -I ./ -c test.ml &> tmp_dsource.ml
	icdiff dsource.ml tmp_dsource.ml
	rm a.out test.cm* test_use.cm* tmp_dsource.ml
