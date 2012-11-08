/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)WSInvoker.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package corrSample;

public class WSInvoker implements Runnable {
    private CorrelatedBpelClientPortType port;
    private int iterations;
    private int threadId;
    private CSInput inMessage = new CSInput();
    private long[][] latencyInfo;
    long minLatency=0, peekLatency=0, totalLatency=0;
    double avgLatency=0;
    
    public WSInvoker(CorrelatedBpelClientPortType port, int threadId, int iterations){
        this.port = port;
        this.iterations = iterations;
        this.threadId = threadId;
        latencyInfo = new long[iterations][2];
    }
    public void run() {
        String threadName = Thread.currentThread().getName();
        inMessage.setInMessage(threadName);
        for (int i = 0; i < iterations; i++) {
            inMessage.setId(threadId * 10000 + i);
            latencyInfo[i][0] = System.currentTimeMillis();
            port.messageInOperation(inMessage);
            latencyInfo[i][1] = System.currentTimeMillis();
        }
        initLatency();
    }
    void initLatency(){
        minLatency = latencyInfo[0][1] - latencyInfo[0][0];
        
        for (int i = 0; i < iterations; i++) {
            long latency = latencyInfo[i][1] - latencyInfo[i][0];
            totalLatency += latency;
            if( latency < minLatency){
                minLatency = latency;
            }
            if( latency > peekLatency){
                peekLatency = latency;
            }
        }
        avgLatency = totalLatency/iterations;
    }
    public double getAvgLatency() {
        return avgLatency;
    }
    public long getMinLatency() {
        return minLatency;
    }
    public long getPeekLatency() {
        return peekLatency;
    }
}
