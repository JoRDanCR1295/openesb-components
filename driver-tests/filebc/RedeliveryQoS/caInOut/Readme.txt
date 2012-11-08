This project is to test Re-Delivery implementation for a 
IN-OUT (Request-Response) message. Its not an automated 
driver test as those tests mainly do SOAP based testing 
which is not applicable in this case.

The BPEL process is set to be Atomic and throws an exception 
as the second-last step (before reply). If the throw activity 
is removed and the service assembly re-built and deployed, 
it would generate:

one response file (Res.xml) and one output file (out_%d.xml) 
per input(Req.xml). 

To test re-delivery throw activity is used to cause BPEL engine
to send a ERROR status on the message exchange, which would cause
FILE BC to re-deliver the in-out message. Note that the throw is 
before the reply but after the invoke(which is used to generate 
the out_%d.xml file as a means to keep track of how many times the 
process has been invoked). Re-delivery settings are configured in CASA.
MaxRetries is set to 2. Therefore when throw activity is used in the 
BPEL process, FILE BC will send the message 2 more times. So in this case:

There should be 3 out_%d.xml files (1 for the original message + 2 for retrys).
But no Res.xml file (since throw is before the reply).


