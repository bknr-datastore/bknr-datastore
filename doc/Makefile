PBOOK=../../tools/pbook.py

all: manual.pdf

manual.pdf: manual.tex introduction.tex guidedtour.tex indices2.tex datastore2.tex impex2.tex 
	pdflatex manual && pdflatex manual
	cp manual.pdf ../../../projects/bknr-website/pdf/

indices2.tex: ../src/indices/tutorial.lisp
	$(PBOOK) -c BknrTexFile -t "BKNR Indices" -o indices2.tex ../src/indices/tutorial.lisp

datastore2.tex: ../src/data/tutorial.lisp
	$(PBOOK) -c BknrTexFile -t "BKNR Datastore" -o datastore2.tex ../src/data/tutorial.lisp

impex2.tex: ../src/xml-impex/tutorial.lisp
	$(PBOOK) -c BknrTexFile -t "BKNR XML Import/Export" -o impex2.tex ../src/xml-impex/tutorial.lisp

clean:
	- rm -rf manual.pdf impex2.tex datastore2.tex indices2.tex

