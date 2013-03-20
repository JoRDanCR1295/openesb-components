
refer the bpel files at ${drivertest_dir}/bpelse\correlation\CorrelationOnePropBPEL\src\EventHandler\InvokeAndReply

Covers the following tests.

Initiate Activitiy   EventHandler > OnEvent (correlate [initiate=yes])

P2	Positive	Invoke (create instance = n, correlate [initiate=empty], pattern = request/response)
P1	Positive	Invoke (create instance = n, correlate [initiate=no], pattern = request/response)
P3	Positive	Invoke (create instance = n, correlate [initiate=join], pattern = request/response)
P2	Positive	Invoke (create instance = n, correlate [initiate=empty], pattern = request)
P1	Positive	Invoke (create instance = n, correlate [initiate=no], pattern = request)
P3	Positive	Invoke (create instance = n, correlate [initiate=join], pattern = request)
P2	Positive	Invoke (create instance = n, correlate [initiate=empty], pattern = response)
P1	Positive	Invoke (create instance = n, correlate [initiate=no], pattern = response)
P3	Positive	Invoke (create instance = n, correlate [initiate=join], pattern = response)
P2	Positive	Reply (create instance = n, correlate [initiate=empty])
P1	Positive	Reply (create instance = n, correlate [initiate=no])
P3	Positive	Reply (create instance = n, correlate [initiate=join])
