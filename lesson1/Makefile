MAIN_OUT=main.byte
SRC=src

default: $(MAIN_OUT)

$(MAIN_OUT): $(SRC)
	ocamlbuild -use-ocamlfind $(MAIN_OUT) -I $(SRC)

clean:
	ocamlbuild -clean

.PHONY: default run clean
