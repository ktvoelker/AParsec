
.PHONY: all debug release clean repl

SOURCES=$(shell find ml -type f -name '*.ml*')

THREADS=$(shell sysctl -n hw.ncpu || echo 1)

BUILD_OPTS=-verbose 0 -I ml -j ${THREADS}

DEBUG_OPTS=-g,-warn-error,@a

RELEASE_OPTS=-w,+a,-warn-error,-a

aparsec.byte: ${SOURCES}
	ocamlbuild ${BUILD_OPTS} -cflags ${DEBUG_OPTS} aparsec.byte

aparsec.native: ${SOURCES}
	ocamlbuild ${BUILD_OPTS} -cflags ${RELEASE_OPTS} aparsec.native

all: debug release

debug: aparsec.byte

release: aparsec.native

clean:
	ocamlbuild -clean

repl: aparsec.byte
	utop -I ml -I _build/ml

