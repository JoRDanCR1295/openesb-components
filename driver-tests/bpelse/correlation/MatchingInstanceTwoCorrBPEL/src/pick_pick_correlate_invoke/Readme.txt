
refer the test case files at ${drivertest_dir}/bpelse\correlation\MatchingInstanceTwoCorrJBI\test\pick_pick_correlate_invoke

Covers the following tests.

Initiating Activities   - Pick1 (correlation1 [initiate=yes]) and Pick2 (correlation2 [initiate=yes])
Correlation activities  - Invoke and reply

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


