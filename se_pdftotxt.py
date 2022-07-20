# Convert machine readable PDF to textfiles, for journal Sovetskaya etnografiya

import fitz
import glob 
import os

dir = os.getcwd()
outdirname = 'textfiles'
outdir = os.path.join(dir, outdirname)
print(outdir)
os.makedirs(outdir) 


for filepath in glob.glob(dir+'/pdfs/*.pdf'): 
    filename = os.path.basename(filepath)
    outfilepath = os.path.join(outdir, filename.replace('.pdf', '.txt')) 
    with fitz.open(filepath) as f:
        for page in f: 
            pagetext = page.get_text(flags=16) # defaulting to "simple" mode; try also "block" and "layout": https://pymupdf.readthedocs.io/en/latest/module.html
            outfile = open(outfilepath, 'a')
            outfile.writelines(pagetext)
            outfile.close()  