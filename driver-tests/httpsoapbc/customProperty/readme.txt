this is used along with the integration test 


test.jbi.integration.testx.TestCustomProperty

Before running the above test do the following 

1. deploy the composite application "CustomPropertyCA"
2. put saml.jar in the glassfish classpath


then run the junit test "test.jbi.integration.testx.TestCustomProperty" 

check the appserver log , for someting like 



[#|2008-09-24T13:16:13.538-0700|INFO|sun-appserver9.1|javax.enterprise.system.stream.out|_ThreadID=33;_ThreadName=HTTPBC-OutboundReceiver-5;|
========================= Printing out runtime properties...|#]

[#|2008-09-24T13:16:13.538-0700|INFO|sun-appserver9.1|javax.enterprise.system.stream.out|_ThreadID=33;_ThreadName=HTTPBC-OutboundReceiver-5;|
{saml1=Hello World 1, saml2=Hello World 2, SignatureConfirmation=[], token.policy=com.sun.xml.wss.impl.policy.mls.AuthenticationTokenPolicy@60b493, javax.xml.ws.soap.http.soapaction.use=true, javax.xml.ws.http.request.headers={}}|#]

[#|2008-09-24T13:16:13.538-0700|INFO|sun-appserver9.1|javax.enterprise.system.stream.out|_ThreadID=33;_ThreadName=HTTPBC-OutboundReceiver-5;|
========================= Done printing out runtime properties...|#]

