pdf:
	cd lecture-notes && texi2pdf lecture-notes.tex --output=../lecture-notes.pdf && cd ..
	cd assignment && texi2pdf assignment.tex --output=distribution/assignment.pdf

clean:
	sh -c 'rm -f lecture-notes/*.{aux,dvi,log,ps,pdf,log,lot,toc,lof,bbl,4ct,4tc,css,fff,idv,lg,rtf,sdw,tmp,ttt,xref,nav,snm,out}'
	sh -c 'rm -f assignment/*.{aux,dvi,log,ps,pdf,log,lot,toc,lof,bbl,4ct,4tc,css,fff,idv,lg,rtf,sdw,tmp,ttt,xref,nav,snm,out}'
	sh -c 'rm -f lecture-notes.pdf'
	sh -c 'rm -f assignment.pdf'

# vim:filetype=make:
# set PUBLISHAS and GOAL before the "include" line
# if there are EPS figures, set EPS
# if there are EPIC figures, set EPIC
# if files depend on sources, set SOURCES
# set PS2PDFFLAGS if needed to embed fonts
# set DVIPSFLAGS if needed to add material, such as -t a4
#
#LFLAGS = --shell-escape # to let embedded "dot" work
#PDF = --output-format=pdf
#DVI = --output-format=dvi
#LATEX = pdflatex
#
#default: seePDF
#
#.SUFFIXES: .tex .xfig .fig .dvi .eps .epic .pdf
#
#.PHONY: dvi
#dvi: $(GOAL).dvi 
#	# xdvi -s 6 -k -expert -xoffset -1.3in -geometry 600x550 $(GOAL).dvi
#	# xdvi -s 6 -k -expert -xoffset .3in -geometry 732x580+10+10 $(GOAL).dvi
#	# xdvi -s 6 -k -expert -xoffset .5in -yoffset .5in -geometry 732x980+141+10 \
#	# 	$(GOAL).dvi
#	xdvi -s 6 -k -expert $(GOAL).dvi -geometry 854x1102+200+10
#
#.PHONY: seeDVI
#seeDVI: $(GOAL).dvi
#	xdvi $(GOAL).dvi
#
#seeEnd: $(GOAL).dvi
#	xdvi + -s 6 -k -expert -xoffset .3in -yoffset .3in \
#		-geometry 732x980+141+10 $(GOAL).dvi
#
#.xfig.eps: 
#	fig2dev -L eps -m 0.7 $*.xfig $*.eps
#
#.fig.eps: 
#	fig2dev -L eps -m 0.7 $*.fig $*.eps
#
#.fig.epic:
#	fig2dev -L epic -m 0.7 $*.fig $*.epic
#
#$(GOAL).dvi: $(GOAL).tex $(EPS) $(EPIC) $(SOURCES)
#	echo "" | $(LATEX) $(DVI) $(LFLAGS) $(GOAL).tex > /dev/null
#	while grep 'Rerun' $(GOAL).log ; do echo "" \
#		| $(LATEX) $(DVI) $(LFLAGS) $(GOAL).tex > /dev/null; done
#
#spell: $(GOAL).tex
#	ispell -t -l < $(GOAL).tex > spell
#	sort -u < spell | less
#	-/bin/rm spell
#
#.PHONY: ps
#ps: $(GOAL).ps
#$(GOAL).ps: $(GOAL).dvi
#	# dvips $(GOAL) -o $(GOAL).ps
#	rm -f $(GOAL).ps
#	dvips $(GOAL) $(DVIPSFLAGS) -Ppdf -G0 -o $(GOAL).ps
#
## postscript see
#.PHONY: seePS
#seePS: $(GOAL).ps
#	gv $(GOAL).ps
#
#.PHONY: pdf
#pdf: $(GOAL).pdf
#$(GOAL).pdf: $(GOAL).tex $(EPS)
#	$(LATEX) $(LFLAGS) $(PDF) $(GOAL)
#	while grep 'Rerun' $(GOAL).log ; do echo "" \
#		| $(LATEX) $(LFLAGS) $(PDF) $(GOAL) > /dev/null; done
#
#.PHONY: seePDF
#seePDF: $(GOAL).pdf
#	xpdf $(GOAL).pdf
#
#.PHONY: publish
#publish: $(GOAL).pdf
#	cp $(GOAL).pdf /homes/$(USER)/HTML/$(PUBLISHAS).pdf
#	# cp $(GOAL).tex /homes/$(USER)/HTML/$(PUBLISHAS).tex
#
#clean: 
#	tcsh -c 'rm -f $(GOAL).{aux,dvi,log,ps,pdf,log,lot,toc,lof,bbl,4ct,4tc,css,fff,idv,lg,rtf,sdw,tmp,ttt,xref,nav,snm,out} '
#
## openOffice format, from which you can get other formats.
#odt: $(GOAL).odt
#
#$(GOAL).odt: $(GOAL).dvi
#	# uses the tex4ht package
#	# htlatex $(GOAL) "xhtml,ooffice" "ooffice/! -cmozhtf" "-coo -cvalidate"
#	htlatex $(GOAL)
#	soffice $(GOAL).html
#
#rtf: $(GOAL).rtf
#
## depends on dvi, because we need all the aux files to be right.
#$(GOAL).rtf: $(GOAL).dvi
#	/bin/rm -rf $(GOAL).rtf
#	latex2rtf $(GOAL)
#
#
