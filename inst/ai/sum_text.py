# Summarizing Text

# Libraries 

#... functions for loading PDF ----------------------------------------------------

from PyPDF2 import PdfReader

#... functions for cleaning PDF --------------------------------------------------

import re

#... functions for summarizingg text ----------------------------------------------

import torch
from transformers import AutoTokenizer, AutoModelWithLMHead

# Prepping PDF / Text -------------------------------------------------------------

#... Loading Text -----------------------------------------------------------------

reader = PdfReader("inst/ai/full-sample.pdf")
text = ""
for page in reader.pages:
    text += page.extract_text() + "\n"
    
text

#... Cleaning Text ----------------------------------------------------------------

import re
def clean_data(data):
  text = re.sub(r"\[[0-9]*\]"," ",data)
  text = text.lower()
  text = re.sub(r'\s+'," ",text)
  text = re.sub(r","," ",text)
  return text

cleaned_article_content = clean_data(text)

# Analzying Text by Summarizing using Google's T5 ---------------------------------

# from (#4): https://www.turing.com/kb/5-powerful-text-summarization-techniques-in-python

tokenizer = AutoTokenizer.from_pretrained('t5-base')
model = AutoModelWithLMHead.from_pretrained('t5-base', return_dict=True)

inputs = tokenizer.encode("summarize: " + cleaned_article_content,
return_tensors='pt',
max_length=512,
truncation=True)

summary_ids = model.generate(inputs, max_length=500, min_length=80, length_penalty=5., num_beams=2)

summary = tokenizer.decode(summary_ids[0])

print(summary)
