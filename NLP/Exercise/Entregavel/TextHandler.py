import nltk
import re
import string
import unicodedata
import spacy

from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords

class TextHandler:

    def __normalize_str(self, text):
        text = text.lower()
        text = re.sub("(https?:\/\/)?([\da-z\.-]+)\.([a-z\.]{2,6})([\/\w \.-]*)", " ",text) #remove links
        text = self.__remove_punctuation(text)
        text = self.__normalize_accents(text)
        text = re.sub(re.compile(r" +"), " ",text)
        return " ".join([w for w in text.split()])

    def __normalize_accents(self, text):
        return unicodedata.normalize("NFKD", text).encode("ASCII", "ignore").decode("utf-8")

    def __remove_punctuation(self, text):
        punctuations = string.punctuation
        table = str.maketrans({key: " " for key in punctuations})
        text = text.translate(table)
        return text

    def tokenizer(self, text):
        stop_words = nltk.corpus.stopwords.words("portuguese") # portuguese, caso o dataset seja em português
        if isinstance(text, str):
            text = self.__normalize_str(text)
            text = "".join([w for w in text if not w.isdigit()])
            text = word_tokenize(text)
            text = [x for x in text if x not in stop_words]
            text = [y for y in text if len(y) > 2]
            return " ".join([t for t in text])
        else:
            return None

    def lemmatizer(self, text):
        nlp = spacy.load('pt_core_news_sm')
        doc = nlp(text)
        return " ".join([word.lemma_ if word.pos_ == "VERB" else word.text for word in doc])