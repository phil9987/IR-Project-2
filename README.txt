In order to run place tinyir-1.1.jar in the lib folder and documents.zip, questions-descriptions.txt,
relevance-judgements.csv and test-questions.txt in the src/resources folder.
Then run `sbt run`. You will be presented with an menu and further instructions.

Depending on your system, sbt might not be able to set the JVM memory. In that case please explicitly set the java
heap size. We recommend 4GB. (.i.e. run `export JAVA_OPTS="-Xmx4G"`)

Note:
The first time any tf model is run, a database for the normalization factors is created. This can also be done
explicitly by using menu point 6.