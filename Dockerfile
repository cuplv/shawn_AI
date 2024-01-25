FROM sbtscala/scala-sbt:graalvm-community-21.0.1_1.9.8_3.3.1
RUN mkdir /home/ai
COPY . /home/ai

CMD ["/bin/bash"]
