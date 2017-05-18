all: build/02.pdf

build/02.pdf: 02.tex
	mkdir -p build
	rubber --pdf --warn all --unsafe --into build 02.tex

clean:
	$(RM) -r build
