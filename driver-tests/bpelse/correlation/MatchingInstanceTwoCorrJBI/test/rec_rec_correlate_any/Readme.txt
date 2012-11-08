
refer the BPEL files at ${drivertest_dir}/bpelse\correlation\MatchingInstanceTwoCorrBPEL\src\rec_rec_correlate_any

Covers the following tests.

Initiating Activities   - Receive (correlation1 [initiate=yes]) and Receive (correlation2 [initiate=yes])
Correlation activities  - Receive, Receive in flow, pick-onMsg, Evt Handler-onEvent, Invoke 2way (req-resp, req, resp)

P2	Positive	Receive (create instance = n, C1 & C2 [initiate=empty])
P1	Positive	Receive (create instance = n, C1 & C2 [initiate=no])
P3	Positive	Receive (create instance = n, C1 & C2 [initiate=join])
P2	Positive	Receive in flow (create instance = n, C1 & C2 [initiate=empty])
P1	Positive	Receive in flow (create instance = n, C1 & C2 [initiate=no])
P3	Positive	Receive in flow (create instance = n, C1 & C2 [initiate=join])
P2	Positive	Pick > OnMessage (create instance = n, C1 & C2 [initiate=empty])
P1	Positive	Pick > OnMessage (create instance = n, C1 & C2 [initiate=no])
P3	Positive	Pick > OnMessage (create instance = n, C1 & C2 [initiate=join])
P2	Positive	EventHandler > OnEvent (create instance = n, C1 & C2 [initiate=empty])
P1	Positive	EventHandler > OnEvent (create instance = n, C1 & C2 [initiate=no])
P3	Positive	EventHandler > OnEvent (create instance = n, C1 & C2 [initiate=join])
P2	Positive	Invoke (create instance = n, C1 & C2 [initiate=empty], pattern = request/response)
P1	Positive	Invoke (create instance = n, C1 & C2 [initiate=no], pattern = request/response)
P3	Positive	Invoke (create instance = n, C1 & C2 [initiate=join], pattern = request/response)
P2	Positive	Invoke (create instance = n, C1 & C2 [initiate=empty], pattern = request)
P1	Positive	Invoke (create instance = n, C1 & C2 [initiate=no], pattern = request)
P3	Positive	Invoke (create instance = n, C1 & C2 [initiate=join], pattern = request)
P2	Positive	Invoke (create instance = n, C1 & C2 [initiate=empty], pattern = response)
P1	Positive	Invoke (create instance = n, C1 & C2 [initiate=no], pattern = response)
P3	Positive	Invoke (create instance = n, C1 & C2 [initiate=join], pattern = response)
P2	Positive	Reply (create instance = n, C1 & C2 [initiate=empty])
P1	Positive	Reply (create instance = n, C1 & C2 [initiate=no])
P3	Positive	Reply (create instance = n, C1 & C2 [initiate=join])

