all: build/main.pdf

build/main.pdf: main.tex ERM.svg PDM.svg scripts/create.sql scripts/inflate.sql
	mkdir -p build
	rubber --pdf --warn all --unsafe --shell-escape --into build main.tex

clean:
	$(RM) -r build/_minted-main
	$(RM) build/*
	$(RM) *.pdf *.pdf_tex
