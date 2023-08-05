TARGETS=App.exe

.PHONY: all build clean %.exe

all: build link

build:
	dune build app/App.exe --profile release # ignores warnings as errors

link: $(TARGETS)

%.exe:
	if [ ! -f $@ ]; then ln -s _build/default/app/$@ . ; fi

clean:
	rm -rf _build *.install *.pdf $(TARGETS)
