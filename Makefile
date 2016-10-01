#list all steps
all: clean merge notes
clean: fdi_clean.Rout
merge: fdi_merge.Rout
notes: fdi_notes.pdf

#run cleaning Code
fdi_clean.Rout: Code/fdi_clean.R
	R CMD BATCH Code/fdi_clean.R

#merge other variables
fdi_merge.Rout: Code/fdi_merge.R
	R CMD BATCH Code/fdi_merge.R

#create latex doc
FDI_Project.pdf: fdi_notes.tex
	latexmk -pdf -quiet fdi_notes

# Clean up stray files
clean_m:
	rm -fv *.aux *.log *.toc *.blg *.bbl *.synctex.gz
	rm -fv *.out *.bcf *blx.bib *.run.xml
	rm -fv *.fdb_latexmk *.fls

