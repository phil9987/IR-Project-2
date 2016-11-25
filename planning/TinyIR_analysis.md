TinyIR analysis
===============

## IO
TipsterStream - takes directory with zips, creates ZipDirStream and stream of all documents


TipsterCorpusIterator

## Processing
StopWords
PorterStemmer
TipsterParse (!!! no title!!!)
		override def title  : String = "" 
		override def body   : String = read(doc.getElementsByTagName("TEXT"))
		override def name   : String = read(doc.getElementsByTagName("DOCNO")).filter(_.isLetterOrDigit)
		override def date   : String = ""
		override def content: String = body  

better: override def title : String = read(doc.getElementsByTagName("HEAD"))

## math
probvector - some vector processing

## indexing
SimpleIndex
PosIndex
FreqIndex
InvertedIndex

## compression
IntegerCompression
