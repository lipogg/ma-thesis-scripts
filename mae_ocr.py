# Pymupdf-Tesseract OCR for journal "Sborniki Muzej antropologii i etnografii SSSR"

import fitz
import glob 
import os

dir = os.getcwd()
outdirname = 'textfiles-pymupdf'
outdir = os.path.join(dir, outdirname)
print(outdir)
os.makedirs(outdir)  
             
for filename in glob.glob('*.pdf'): 
    filepath = os.path.join(dir, filename) 
    outfilepath = os.path.join(outdir, filename.replace('.pdf', '.txt')) 

    with fitz.open(filepath) as f:
        for page in f: 
            full_tp = page.get_textpage_ocr(flags=16, full=True, dpi=600, language="rus+lat+fra+eng+tgk+uzb+uzb_cyrl")
            pagetext = page.get_text(textpage=full_tp)
            outfile = open(outfilepath, 'a')
            outfile.writelines(pagetext)
            outfile.close()

           
