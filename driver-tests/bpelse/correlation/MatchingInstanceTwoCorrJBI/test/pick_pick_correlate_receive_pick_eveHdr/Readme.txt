
refer the bpel files at ${drivertest_dir}/bpelse\correlation\MatchingInstanceTwoCorrBPEL\src\pick_pick_correlate_receive

Covers the following tests.

Initiating Activities   - Pick1 (correlation1 [initiate=yes]) and Pick2 (correlation2 [initiate=yes])
Correlation activities  - Receive, Receive in flow, pick-onMsg, Evt Handler-onEvent

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

