
all: NetworkServer

NetworkServer: $(wildcard *.hs)
	ghc --make -O2 -fno-spec-constr-count NetworkServer

clean:
	rm -f *.o *.hi NetworkServer

.PHONY: all clean
