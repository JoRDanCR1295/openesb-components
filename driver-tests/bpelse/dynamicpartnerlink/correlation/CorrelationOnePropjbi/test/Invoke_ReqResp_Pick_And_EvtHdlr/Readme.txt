
Uses the test BPEL at ${drivertest_dir}/bpelse\correlation\CorrelationOnePropBPEL\src\Invoke_ReqResp\Pick_And_EveHdlr

Covers the following tests.
Initiate activity, Invoke (correlate [initiate=yes], pattern = request/response)

P2 	Positive	Pick > OnMessage (correlate [initiate=empty])
P1	Positive	Pick > OnMessage (correlate [initiate=no])
P3	Positive	Pick > OnMessage (correlate [initiate=join])
P2	Positive	EventHandler > OnEvent (correlate [initiate=empty])
P1	Positive	EventHandler > OnEvent (correlate [initiate=no])
P3	Positive	EventHandler > OnEvent (correlate [initiate=join])
