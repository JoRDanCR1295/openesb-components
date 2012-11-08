Transformation, XsltAdapter and associated Comp App projects 
in open-jbi-components\driver-tests\bpelse\benchmark\ are created 
to compare the performance of BPEL-SE and XSLT-SE doing the 
transformation on the request and request while working 
as proxy for the MockService in the same folder. 
MockService is simple web project withe a servelet, that echos request.

test-soapui-project.xml is the soap-ui project for load testing, 
it is checked in open-jbi-components\driver-tests\bpelse\benchmark\XsltAdapterApp