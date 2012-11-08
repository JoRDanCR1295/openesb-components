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
 * @(#)BCCoyoteConnector.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.embedded;

import java.util.Timer;

import org.apache.catalina.LifecycleException;
import org.apache.catalina.Request;
import org.apache.catalina.Response;
import org.apache.coyote.tomcat5.CoyoteConnector;
import org.apache.coyote.tomcat5.CoyoteResponse;

import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;

public class BCCoyoteConnector extends CoyoteConnector{

    private static final String USE_COYOTE_CONNECTOR = 
                "com.sun.enterprise.web.connector.useCoyoteConnector";

    private static final String GRIZZLY_CONNECTOR = 
                "com.sun.enterprise.web.connector.grizzly.GrizzlyHttpProtocol";

   
    /**
     * Are we recycling objects
     */
    protected boolean recycleObjects;
    
    
     /**
     * The number of acceptor threads.
     */   
    protected int maxAcceptWorkerThreads;
    
    
    /**
     * The number of reader threads.
     */
    protected int maxReadWorkerThreads;
    
    
    /**
     * The request timeout value used by the processor threads.
     */
    protected int processorWorkerThreadsTimeout;
    
    
    /**
     * The increment number used by the processor threads.
     */
    protected int minProcessorWorkerThreadsIncrement;
    
    
    /**
     * The size of the accept queue.
     */
    protected int minAcceptQueueLength;
    
    
    /**
     * The size of the read queue
     */
    protected int minReadQueueLength;
    
    
    /**
     * The size of the processor queue.
     */ 
    protected int minProcessorQueueLength;   
    
    
    /**
     * Use direct or non direct byte buffer.
     */
    protected boolean useDirectByteBuffer;
        
    /*
     * Number of seconds before idle keep-alive connections expire
     */
    private int keepAliveTimeoutInSeconds;

    /*
     * Number of keep-alive threads
     */
    private int keepAliveThreadCount;

    /*
     * Specifies whether response chunking is enabled/disabled
     */
    private boolean chunkingDisabled;

    /**
     * Maximum pending connection before refusing requests.
     */
    private int maxPendingCount = 4096;
    
    
    /**
     * Set the number of <code>Selector</code> used by Grizzly.
     */
    public int selectorReadThreadsCount = 0;

    private int maxQueueSizeInBytes = -1;
    
    private HttpSoapBindingLifeCycle lifeCycle;
    
    public BCCoyoteConnector(HttpSoapBindingLifeCycle lifeCycle) {
        boolean coyoteOn = false;
        if (System.getProperty(USE_COYOTE_CONNECTOR) != null){
            coyoteOn =
                   Boolean.valueOf(System.getProperty(USE_COYOTE_CONNECTOR))
                                                               .booleanValue();
        }
        
        // By default, turn on the Coyote Connector
        if (!coyoteOn) 
            setProtocolHandlerClassName(GRIZZLY_CONNECTOR);
        
        this.lifeCycle = lifeCycle;
        
    }
    

    /**
     * Enables or disables chunked encoding for any responses returned by this
     * Connector.
     *
     * @param chunkingDisabled true if chunking is to be disabled, false
     * otherwise
     */
    public void setChunkingDisabled(boolean chunkingDisabled) {
        this.chunkingDisabled = chunkingDisabled;
    }


    /**
     * @return true if chunking is disabled on this Connector, and false
     * otherwise
     */
    public boolean isChunkingDisabled() {
        return this.chunkingDisabled;
    }


    /** 
     * Create (or allocate) and return a Request object suitable for
     * specifying the contents of a Request to the responsible Container.
     */
    public Request createRequest() {
        
        BCCoyoteRequest request = new BCCoyoteRequest();
        request.setConnector(this);
        return (request);

    }


    /**
     * Creates and returns Response object.
     *
     * @return Response object
     */ 
    public Response createResponse() {

        //CoyoteResponse response = new CoyoteResponse(isChunkingDisabled());
        CoyoteResponse response = new BCCoyoteResponse(isChunkingDisabled());
        response.setConnector(this);
        return (response);

    }


    /**
     * Gets the number of seconds before a keep-alive connection that has
     * been idle times out and is closed.
     *
     * @return Keep-alive timeout in number of seconds
     */
    public int getKeepAliveTimeoutInSeconds() {
        return keepAliveTimeoutInSeconds;
    }


    /**
     * Sets the number of seconds before a keep-alive connection that has
     * been idle times out and is closed.
     *
     * @param timeout Keep-alive timeout in number of seconds
     */
    public void setKeepAliveTimeoutInSeconds(int timeout) {
        keepAliveTimeoutInSeconds = timeout;
        setProperty("keepAliveTimeoutInSeconds", String.valueOf(timeout));
    }


    /**
     * Gets the number of keep-alive threads.
     *
     * @return Number of keep-alive threads
     */
    public int getKeepAliveThreadCount() {
        return keepAliveThreadCount;
    }


    /**
     * Sets the number of keep-alive threads.
     *
     * @param threadCount Number of keep-alive threads
     */
    public void setKeepAliveThreadCount(int threadCount) {
        keepAliveThreadCount = threadCount;
        setProperty("keepAliveThreadCount", String.valueOf(threadCount));
    }


    /**
     * Set the maximum pending connection this <code>Connector</code>
     * can handle.
     */
    public void setMaxPendingConnection(int maxPendingCount){
        this.maxPendingCount = maxPendingCount;
        setProperty("maxPendingConnection", String.valueOf(maxPendingCount));
    }


    /**
     * Return the maximum pending connection.
     */
    public int getMaxPendingConnection(){
        return maxPendingCount;
    }


    /**
     * Set the <code>recycle-tasks</code> used by this <code>Selector</code>
     */   
    public void setRecycleObjects(boolean recycleObjects){
        this.recycleObjects= recycleObjects;
        setProperty("recycleObjects", 
                    String.valueOf(recycleObjects));
    }
    
    
    /**
     * Return the <code>recycle-tasks</code> used by this 
     * <code>Selector</code>
     */      
    public boolean getRecycleObjects(){
        return recycleObjects;
    }
   
   
    /**
     * Set the <code>reader-thread</code> from domian.xml.
     */    
    public void setMaxReadWorkerThreads(int maxReadWorkerThreads){
        this.maxReadWorkerThreads = maxReadWorkerThreads;
        setProperty("maxReadWorkerThreads", 
                    String.valueOf(maxReadWorkerThreads));
    }
    
    
    /**
     * Return the <code>read-thread</code> used by this <code>Selector</code>
     */  
    public int getMaxReadWorkerThreads(){
        return maxReadWorkerThreads;
    }

    
    /**
     * Set the <code>reader-thread</code> from domian.xml.
     */    
    public void setMaxAcceptWorkerThreads(int maxAcceptWorkerThreads){
        this.maxAcceptWorkerThreads = maxAcceptWorkerThreads;
        setProperty("maxAcceptWorkerThreads", 
                    String.valueOf(maxAcceptWorkerThreads));
    }
    
    
    /**
     * Return the <code>read-thread</code> used by this <code>Selector</code>
     */  
    public int getMaxAcceptWorkerThreads(){
        return maxAcceptWorkerThreads;
    }
    
    
    /**
     * Set the <code>acceptor-queue-length</code> value 
     * on this <code>Selector</code>
     */
    public void setMinAcceptQueueLength(int minAcceptQueueLength){
        this.minAcceptQueueLength = minAcceptQueueLength;
        setProperty("minAcceptQueueLength", 
                    String.valueOf(minAcceptQueueLength));
    }
 
    
    /**
     * Return the <code>acceptor-queue-length</code> value
     * on this <code>Selector</code>
     */
    public int getMinAcceptQueueLength(){
        return minAcceptQueueLength;
    }
    
    
    /**
     * Set the <code>reader-queue-length</code> value 
     * on this <code>Selector</code>
     */
    public void setMinReadQueueLength(int minReadQueueLength){
        this.minReadQueueLength = minReadQueueLength;
        setProperty("minReadQueueLength", 
                    String.valueOf(minReadQueueLength));
    }
    
    
    /**
     * Return the <code>reader-queue-length</code> value
     * on this <code>Selector</code>
     */
    public int getMinReadQueueLength(){
        return minReadQueueLength;
    }
    
    
    /**
     * Set the <code>processor-queue-length</code> value 
     * on this <code>Selector</code>
     */
    public void setMinProcessorQueueLength(int minProcessorQueueLength){
        this.minProcessorQueueLength = minProcessorQueueLength;
        setProperty("minProcessorQueueLength", 
                    String.valueOf(minProcessorQueueLength));
    }
    
    
    /**
     * Return the <code>processor-queue-length</code> value
     * on this <code>Selector</code>
     */  
    public int getMinProcessorQueueLength(){
        return minProcessorQueueLength;
    }
    
    
    /**
     * Set the <code>use-nio-non-blocking</code> by this <code>Selector</code>
     */  
    public void setUseDirectByteBuffer(boolean useDirectByteBuffer){
        this.useDirectByteBuffer = useDirectByteBuffer;
        setProperty("useDirectByteBuffer", 
                    String.valueOf(useDirectByteBuffer));
    }
    
    
    /**
     * Return the <code>use-nio-non-blocking</code> used by this 
     * <code>Selector</code>
     */    
    public boolean getUseDirectByteBuffer(){
        return useDirectByteBuffer;
    }

    
    public void setProcessorWorkerThreadsTimeout(int timeout){
        this.processorWorkerThreadsTimeout = timeout;
        setProperty("processorWorkerThreadsTimeout", 
                    String.valueOf(timeout));        
    }
    
    
    public int getProcessorWorkerThreadsTimeout(){
        return processorWorkerThreadsTimeout;
    }
    
    
    public void setProcessorWorkerThreadsIncrement(int increment){
        this.minProcessorWorkerThreadsIncrement = increment;
        setProperty("processorThreadsIncrement", 
                    String.valueOf(increment));      
    }
    
    
    public int getMinProcessorWorkerThreadsIncrement(){
        return minProcessorWorkerThreadsIncrement;
    }
 
    public void setSelectorReadThreadsCount(int selectorReadThreadsCount){
        setProperty("selectorReadThreadsCount", 
                     String.valueOf(selectorReadThreadsCount)); 
    }

    public void start() throws LifecycleException {
        super.start();

        com.sun.enterprise.web.connector.grizzly.GrizzlyHttpProtocol protocolHandler = 
            (com.sun.enterprise.web.connector.grizzly.GrizzlyHttpProtocol)getProtocolHandler();
        com.sun.enterprise.web.connector.grizzly.SelectorThread selThread = protocolHandler.selectorThread();
        
        if (selThread.getProcessorPipeline() instanceof LinkedListThrottlePipeline) {
            LinkedListThrottlePipeline pipeline = (LinkedListThrottlePipeline)selThread.getProcessorPipeline();
            pipeline.setHttpSoapBindingLifeCycle(lifeCycle);
        }
    }
    
    public void stop() throws LifecycleException {
        com.sun.enterprise.web.connector.grizzly.GrizzlyHttpProtocol protocolHandler = 
            (com.sun.enterprise.web.connector.grizzly.GrizzlyHttpProtocol)getProtocolHandler();
        com.sun.enterprise.web.connector.grizzly.SelectorThread selThread = protocolHandler.selectorThread();

        if (selThread.getProcessorPipeline() instanceof LinkedListThrottlePipeline) {
            LinkedListThrottlePipeline pipeline = (LinkedListThrottlePipeline)selThread.getProcessorPipeline();
            pipeline.cleanup();
        }
        super.stop();
    }
}
