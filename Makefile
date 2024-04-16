ARCH := $(shell uname -m)
DOCKER_IMAGE := "gerbil/gerbilxx:$(ARCH)"
default: linux-static

build-release:
	/opt/gerbil/bin/gxpkg deps -i
	/opt/gerbil/bin/gxpkg build --release

linux-static:
	docker run -it \
	-e USER=$(USER) \
	-e GERBIL_PATH=/src/.gerbil \
	-v $(PWD):/src:z \
	$(DOCKER_IMAGE) \
	make -C /src/ build-release

install:
	mv .gerbil/bin/gerbil-sequentia /usr/local/bin/gerbil-sequentia

clean:
	gerbil clean
	gerbil clean all

repl:
	gxi -:te

build-sequentia:
	cd ../SEQ-Core-Elements; ./autogen.sh; ./configure; make -j$(shell nproc)

build-sequentia-debug:
	cd ../SEQ-Core-Elements; make clean; ./autogen.sh; ./configure --enable-debug; make -j$(shell nproc)

rebuild-sequentia:
	cd ../SEQ-Core-Elements; make -j$(shell nproc)

record-demo:
	./scripts/debug.ss start; gerbil build; asciinema rec demo.cast --command ./scripts/demo.ss; agg demo.cast demo.gif; rm demo.cast; ./scripts/debug.ss stop
