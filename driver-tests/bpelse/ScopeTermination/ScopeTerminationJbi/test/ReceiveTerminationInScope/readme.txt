This test tests the termination of Receives (one way as well as two-way). It also tests that the messages received for the terminated Receives are not discarded but are stored.

The parent BP has a foreach with 2 iterations. Each iteration is identical and calls the SubBP.

Iteration 1:
1. The Driver sends a message to the SubBP using a two-way invoke.
2. The SubBP instance is created. The SubBP has a flow with three branches. On two of the branches, two Receives (Receive2 and Receive3) wait for a message. After a second a fault is thrown from the third branch. This should terminate the two waiting receives. 
3. The Driver sends the messages corresponding to the two Receives in the Sub-BP. These messages are not consumed since the receives were terminated. However, the messages are stored by the sun-bpel-se and NOT discarded. This is the indended behavior.
4. The Sub-BP sends a reply from the catch block.
5. The driver's first invoke completes (since the Sub-BP replied as noted in the above step)
6. The driver does some assignments and then throws a fault. The scope catches it and does nothing (empty activity). This complets the first iteration of the foreach

Iteration 2:
In step 2 the receives will NOT wait, but will consume the messages stored by the sun-bpel-se (as noted in step 3 above).
The rest is same as iteration 1. 

Once both the iterations are completed, the output will have the results of both the first and the second iteration.

