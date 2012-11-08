set MAVEN_REPOSITORY=%USERPROFILE%\.m2\repository
@set cp=
@set cp=%cp%;%MAVEN_REPOSITORY%\xmlbeans\xbean\2.0.0\xbean-2.0.0.jar
@set cp=%cp%;%MAVEN_REPOSITORY%\xmlbeans\jsr173_api\1.0\jsr173_api-1.0.jar
@set cp=%cp%;%JAVA_HOME%\lib\tools.jar
@set cp=%cp%;%MAVEN_REPOSITORY%\xml-resolver\xml-resolver\1.1\xml-resolver-1.1.jar
@set cp=%cp%;..\..\xsdextension-fw\src\encoderfrmwk-xsdextension.jar

java -classpath %cp% org.apache.xmlbeans.impl.tool.SchemaCompiler -src . -out customencoder-xsdextension.jar customencoder-xsdextension.xsd customencoder-xsdextension.xsdconfig
