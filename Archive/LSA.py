## -*- coding: utf-8 -*-
#"""
#Created on Mon Oct 19 16:25:54 2015
#
#@author: Will
#"""

from bs4 import BeautifulSoup, CData
import nltk
from nltk.corpus import stopwords
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import TruncatedSVD
from xml.dom import minidom
import xml.etree.ElementTree
#run this only once
#nltk.download('stopwords')



#"""
#I exported the forum posts for module 0 into an XML file.
#Each post is wrapped in tags. I'll use BeautifulSoup to
#process the XML
#"""

e = xml.etree.ElementTree.parse('news_documents.xml').getroot()

for page in list(e):
     title = page.find('doc').find('c').text
     print('title: %s' % (title) #, content))
     #content = page.find('CDATA').text
     #print('title: %s; content: %s' % (title, content))


xmldoc = minidom.parse('news_documents.xml')
itemlist = xmldoc.getElementsByTagName('c')
print(len(itemlist))

postDocs = [str(i.toprettyxml()).replace("<c><![CDATA[","").replace("]]></c>","") for i in itemlist]


#"""
#create lists of the information from the XML documents
#"""

postDocs.pop(0)
postDocs = [x.lower() for x in postDocs]



#"""
#Stopwords are words that I don't want to convert to
#featurs,becuase they aren't especially useful. Words
#like 'a', 'and', and 'the' are good stopwords in english.
#I can use a built in list of stopwords from nltk to get
#me started. Then, I'll add some custom stopwords that
#are 'html junk' that I need to clean out of my data.
#"""

stopset = set(stopwords.words('english'))
stopset.update(['lt','p','/p','br','amp','quot','field','font','normal','span','0px','rgb','style','51',
                'spacing','text','helvetica','size','family', 'space', 'arial', 'height', 'indent', 'letter'
                'line','none','sans','serif','transform','line','variant','weight','times', 'new','strong', 'video', 'title'
                'white','word','letter', 'roman','0pt','16','color','12','14','21', 'neue', 'apple', 'class','00','000','002'])

#"""
#TF-IDF Vectorizing¶
#I'm going to use scikit-learn's TF-IDF vectorizer to take
# my corpus and convert each document into a sparse matrix
# of TFIDF Features...
#"""

#Before!
postDocs[0]

vectorizer = TfidfVectorizer(stop_words=stopset,
                                 use_idf=True, ngram_range=(1, 3))
X = vectorizer.fit_transform(postDocs)

X[0]

#After
print(X[0])

#"""
#LSA
#Input: X, a matrix where m is the number of documents I have, and n is the number of terms.
#Process: I'm going to decompose X into three matricies called U, S, and T. When we do the
#decomposition, we have to pick a value k, that's how many concepts we are going to keep.
#X≈USVT
#U will be a m x k matrix. The rows will be documents and the columns will be 'concepts'
#S will be a k x k diagnal matrix. The elements will be the amount of variation captured from each concept.
#V will be a m x k (mind the transpose) matrix. The rows will be terms and the columns will be conepts.
#"""

X.shape

lsa = TruncatedSVD(n_components=25, n_iter=100, algorithm="arpack")
lsa.fit(X)
#
#"""
##This is the first row for V
#"""

lsa.components_[0]

terms = vectorizer.get_feature_names()
for i, comp in enumerate(lsa.components_):
    termsInComp = zip (terms,comp)
    sortedTerms =  sorted(termsInComp, key=lambda x: x[1], reverse=True) [:10]
    print("Concept %d:" % i)
    for term in sortedTerms:
        print(term[0])
    print(" ")

