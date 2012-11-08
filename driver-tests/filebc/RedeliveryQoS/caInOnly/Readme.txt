This project is to test Re-Delivery implementation. Its not an automated 
driver test as those tests mainly do SOAP based testing which is not 
applicable in this case.

The BPEL process is set to be Atomic and throws an exception as the last 
step. If the throw activity is removed and the service assembly re-built
and deployed, it would generate one output file per input. 

To test re-delivery throw activity is used to cause BPEL engine to send 
a ERROR status on the message exchange, which would cause FILE BC to 
re-deliver the one-way inbound message. Re-delivery settings are configured 
in CASA. MaxRetries is set to 2. Therefore when throw activity is used in
the BPEL process, FILE BC will send the message 2 more times. So the number
of output files generated should be 3 (1 for the original message + 2 for retrys). 


