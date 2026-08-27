FROM eclipse-temurin:25-jre-alpine

RUN addgroup -S cch && adduser -S -G cch -h /app cch
WORKDIR /app
COPY --chown=cch:cch target/cch.jar /app/cch.jar

USER cch
ENTRYPOINT ["java", "-XX:+UseSerialGC", "-Xmx256m", "-XX:MaxMetaspaceSize=128m", "-jar", "/app/cch.jar"]
CMD ["serve", "--host", "0.0.0.0", "--port", "8888"]
