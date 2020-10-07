MAIN_OUT=main.byte
TEST_OUT=test.byte
SRC=src

default: $(MAIN_OUT)

$(MAIN_OUT): $(SRC)
	ocamlbuild -use-ocamlfind $(MAIN_OUT) -I $(SRC)

$(TEST_OUT): $(SRC)
	ocamlbuild -use-ocamlfind $(TEST_OUT) -I $(SRC)

test: $(MAIN_OUT) $(TEST_OUT)
	./$(TEST_OUT) && \
	for dir in test* ; do \
		if [ -d $$dir ]; then \
			cd $$dir && turnt *.bril ; \
			cd .. ; \
		fi \
	done

clean:
	ocamlbuild -clean

.PHONY: default run test clean
