GHCOPTS=-fwarn-tabs -fwarn-unused-imports -fwarn-overlapping-patterns -fwarn-missing-signatures
SRC=Main.hs

all:
	ghc ${GHCOPTS} ${SRC} -o tftp-client

clean:
	rm -f tftp-client *.o *.hi
