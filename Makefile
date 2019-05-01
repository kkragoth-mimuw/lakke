run:
	cabal v2-run
	
lint:
	hlint src/*
	
generate_grammar:
	mkdir -p generated && \
	cd generated && \
	bnfc -m ../grammar/lakke.cf 
	cd generated && make

.PHONY: clean
clean:
	rm -rf generated
