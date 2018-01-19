## Imports
import PyPDF2
import textract
#from nltk.tokenize import word_tokenize
#from nltk.corpus import stopwords

## Reading PDF FILE, configure here when reading multiple files
filename = 'invoice.pdf' 
pdfFileObj = open(filename,'rb')
pdfReader = PyPDF2.PdfFileReader(pdfFileObj)
num_pages = pdfReader.numPages
count = 0
text = ""
while count < num_pages:
    pageObj = pdfReader.getPage(count)
    count +=1
    text += pageObj.extractText()

if text != "":
   text = text

else:
   text = textract.process(fileurl, method='tesseract', language='eng')
   
## text refers to entire string read in the pdf file
s = text
#print s 

## Naming conventions to easily pick out data
attention = 'Attention:'
designation = 'Designation:'
companyname = 'Company Name:'
address = 'Address:'
zipcode = 'Zip Code:'
dateissue = 'Date of issue:'
projectTitle = 'Project Title:'
invoicedes = 'Invoice Description:'
invoicenum = 'Invoice Number:'
des = 'Description'

arr = [attention, designation, companyname, address, zipcode, dateissue, projectTitle, invoicedes, invoicenum, des]


for i,var in enumerate(arr):
    if i == len(arr) -1:
        prep = s[s.find("Prep Day")+len("Prep Day"):s.rfind("Final Payment for Prep Day")]
        prep_qty = prep.split('$')[0]
        prep_unitcost = prep.split('$')[1][1:]
        prep_cost = prep.split('$')[2][1:]
        print prep_qty, prep_unitcost, prep_cost
        
        proper = s[s.find("Day Proper")+len("Day Proper"):s.rfind("Final Payment")]
        proper_qty = proper.split('$')[0]
        proper_unitcost = proper.split('$')[1][1:]
        proper_cost = proper.split('$')[2][1:]
        print proper_qty, proper_unitcost, proper_cost
        
        prep_description = s[s.find("Final Payment for Prep Day on")+len("Final Payment for Prep Day on"):s.rfind("Day Proper")]
        print prep_description
        
        proper_description = s[s.find("Final Payment for F.E.P program")+len("Final Payment for F.E.P program"):s.rfind("Subtotal")]
        print proper_description
    else:
        print s[s.find(var)+len(var):s.rfind(arr[i+1])]


## String search, finding between 2 strings: print s[s.find(attention)+len(attention):s.rfind(designation)]
## Usage: print s[s.find(first string)+len(first string):s.rfind(2nd string)]
        
