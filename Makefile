all: test_my_list rapport


test_my_list: my_list.ml test_my_list.ml
	ocamlc my_list.mli my_list.ml test_my_list.ml -o test_my_list


rapport: rapport.tex
	pdflatex -shell-escape rapport.tex -o rapport

clean:
	rm -rf test_list *.cmi *.cmo *~
