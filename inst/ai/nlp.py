# Tokenizing Words

from nltk.tokenize import sent_tokenize, word_tokenize

example_text = """
there once was a man named Jim. Jim had a friend named Jimbo who was also a Jim. 
Jim and Jimbo liked to go to the gym, where they would play the tambourine."""

sent_tokenize(example_text)

example_string = """
Muad'Dib learned rapidly because his first training was in how to learn.
And the first lesson of all was the basic trust that he could learn.
It's shocking to find how many people do not believe they can learn,
and how many more believe learning to be difficult."""

sent_tokenize(example_string)

word_tokenize(example_text)

# Stop Words 

nltk.download("stopwords")
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize

worf_quote = "Sir, I protest. I am not a merry man!"
words_in_quote = word_tokenize(worf_quote)
words_in_quote

stop_words = set(stopwords.words("english"))

filtered_list = []

for word in words_in_quote:
   if word.casefold() not in stop_words:
        filtered_list.append(word)
        

filtered_list = [
    word for word in words_in_quote if word.casefold() not in stop_words
]

# Stemming Words


