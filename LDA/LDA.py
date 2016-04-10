# -*- coding: utf-8 -*-
#"""
#Created on Mon Oct 19 16:25:54 2015
#
#@author: Will
#"""

from bs4 import BeautifulSoup, CData
import nltk
from nltk.tokenize import RegexpTokenizer
from nltk.stem.porter import PorterStemmer
from nltk.corpus import stopwords
from xml.dom import minidom
from gensim import corpora, models
import gensim

#"""
##run this only once
#nltk.download('stopwords')
#"""

tokenizer = RegexpTokenizer(r'\w+')


#"""
#Stopwords are words that I don't want to convert to
#featurs,becuase they aren't especially useful. Words
#like 'a', 'and', and 'the' are good stopwords in english.
#I can use a built in list of stopwords from nltk to get
#me started. Then, I'll add some custom stopwords that
#are 'html junk' that I need to clean out of my data.
#"""

en_stop = set(stopwords.words('english'))
en_stop.update(['lt','p','/p','br','amp','quot','field','font','normal','span','0px','rgb','style','51',
                'spacing','text','helvetica','size','family', 'space', 'arial', 'height', 'indent', 'letter'
                'line','none','sans','serif','transform','line','variant','weight','times', 'new','strong', 'video', 'title'
                'white','word','letter', 'roman','0pt','16','color','12','14','21', 'neue', 'apple', 'class','00','000','002','<c>'])


#"""
# Create p_stemmer of class PorterStemmer
#"""
#p_stemmer = PorterStemmer()

#"""
I'll use BeautifulSoup to
#process the XML
#"""
xmldoc = minidom.parse('news_documents.xml')
itemlist = xmldoc.getElementsByTagName('c')
print(len(itemlist))


# compile sample documents into a list
doc_set = [str(i.toprettyxml()).replace("<c><![CDATA[","").replace("]]></c>","") for i in itemlist]


# list for tokenized documents in loop
texts = []

# loop through document list
for i in doc_set:

    # clean and tokenize document string
    raw = i.lower()
    tokens = tokenizer.tokenize(raw)

    # remove stop words from tokens
    stopped_tokens = [i for i in tokens if not i in en_stop]

    # stem tokens
    # stemmed_tokens = [p_stemmer.stem(i) for i in stopped_tokens]

    # add tokens to list
    #texts.append(stemmed_tokens)
    texts.append(stopped_tokens)

# turn our tokenized documents into a id <-> term dictionary
dictionary = corpora.Dictionary(texts)

# convert tokenized documents into a document-term matrix
corpus = [dictionary.doc2bow(text) for text in texts]

# generate LDA model
ldamodel = gensim.models.ldamodel.LdaModel(corpus, num_topics=25, id2word = dictionary, passes=100)

print(ldamodel.print_topics(num_topics=2, num_words=4))



