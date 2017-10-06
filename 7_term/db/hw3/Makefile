all: build/main.pdf

build/main.pdf: main.tex
	mkdir -p build
	rubber --pdf --warn all --unsafe --shell-escape --into build main.tex

clean:
	$(RM) -r build/_minted-main
	$(RM) build/*
	$(RM) *.pdf *.pdf_tex
