FROM hseeberger/scala-sbt:8u151-2.12.4-1.1.0

ENV SPARK_VERSION=2.4.3
ENV HADOOP_VERSION=2.7

RUN wget -q http://apache.mirror.iphh.net/spark/spark-${SPARK_VERSION}/spark-${SPARK_VERSION}-bin-hadoop${HADOOP_VERSION}.tgz \
  && tar xzf spark-${SPARK_VERSION}-bin-hadoop${HADOOP_VERSION}.tgz \
  && mv spark-${SPARK_VERSION}-bin-hadoop${HADOOP_VERSION} /spark \
  && rm spark-${SPARK_VERSION}-bin-hadoop${HADOOP_VERSION}.tgz

ENV SPARK_HOME=/spark

WORKDIR /code/mogrif
RUN git clone https://github.com/salesforce/TransmogrifAI.git
RUN cd TransmogrifAI && ./gradlew compileTestScala installDist

EXPOSE 8080
