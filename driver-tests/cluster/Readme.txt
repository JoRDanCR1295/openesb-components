This projects are created to test the cluster support.

BPEL projects checked-in to this folder constructed using abstract WSDL,
corresponding comp app projects are configured with SOAP binding.

We would be creating equivalent CompApp projects for all the binding
components we support.

InOnlyService & InOutService are basic test case

InOutServiceCorrelation & InOnlyServiceCorrelation test the correlation

InOutServiceCorrelation & InOnlyServiceCorrelation projects have BP with
correlation defined on /ns0:order/@id, if you send more than one request
at a time for a given order id, it would result in correlation violation,
in turn resulting error. There is no limit on the concurrent requests,
make sure that each request carries payload with different id.
To complete the request/business process, you need to send two requests,
first one for calcPOTotalValueOperation and send for approveCalcPOTotalValue,
refer to attached test cases.


Business Process in InOnlyService & InOnlyServiceCorrelation invokes an
external service at the end of the process, to support the testing "InOnlyServer"
Apache CXF based server is provided. on the command line call
java -jar InOnlyServer.jar
WS listens at 9000 port. make sure that you have no application running at this port.
Corresponding CompApp projects are configured to invoke this service. 
Source code is available for review and enhancements.



