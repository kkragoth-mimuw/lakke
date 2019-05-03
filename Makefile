run:
	cabal v2-run
	
lint:
	hlint src/*

wc:
	find src -name '*.hs' | xargs wc -l
	
generate_grammar:
	mkdir -p generated && \
	cd generated && \
	bnfc -m ../grammar/lakke.cf 
	cd generated && make

.PHONY: clean
clean:
	rm -rf generated
