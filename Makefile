bin/%: src/%.hs
	ghc $< -o $@

clean:
	rm src/*.hi
	rm src/*.o

.PHONY: clean
