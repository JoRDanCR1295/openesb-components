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
 * @(#)LinkedListThrottlePipeline.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.embedded;

import java.nio.channels.SelectionKey;
import java.nio.ByteBuffer;
import java.nio.BufferUnderflowException;
import java.nio.channels.SocketChannel;

import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Iterator;

import com.sun.enterprise.web.connector.grizzly.LinkedListPipeline;
import com.sun.enterprise.web.connector.grizzly.SelectorThread;
import com.sun.enterprise.web.connector.grizzly.Task;
import com.sun.enterprise.web.connector.grizzly.ReadTask;
import com.sun.enterprise.web.connector.grizzly.HtmlHelper;
import com.sun.enterprise.web.connector.grizzly.AsyncReadTask;
import com.sun.enterprise.web.connector.grizzly.WorkerThread;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;
import com.sun.jbi.httpsoapbc.Endpoint;
import com.sun.jbi.httpsoapbc.RequestThrottlingController;

/**
 *
 * Throttle pipeline for throttling requests
 */
public class LinkedListThrottlePipeline extends LinkedListPipeline implements RequestThrottlingController {
    private static final Messages mMessages =
        Messages.getMessages(LinkedListThrottlePipeline.class);
    private static final Logger mLogger =
        Messages.getLogger(LinkedListThrottlePipeline.class);
    
    private SelectorThread st;

    private final static String QUERY_STRING="?";   
    private final static String PATH_STRING="/";   

    private HttpSoapBindingLifeCycle lifeCycle;

    private ExecutorService resumeThreadPool;

    protected ConcurrentHashMap<String, ConcurrentLinkedQueue<SelectionKeyByteBuffer>> suspendedKeys = 
            new ConcurrentHashMap<String, ConcurrentLinkedQueue<SelectionKeyByteBuffer>>();

    protected ConcurrentLinkedQueue<ByteBuffer> bbPool =
            new ConcurrentLinkedQueue<ByteBuffer>();

    class SelectionKeyByteBuffer {
        private SelectionKey sk;
        private ByteBuffer bb;
        
        SelectionKeyByteBuffer (SelectionKey sk, ByteBuffer bb) {
            this.sk = sk;
            this.bb = bb;
        }
        
        SelectionKey getSelectionKey() { return sk; }
        ByteBuffer getByteBuffer() {return bb; }
    }
    
    class ResumerTask implements Runnable {
        LinkedListThrottlePipeline pipeline;
        Endpoint endpoint;
        ResumerTask (LinkedListThrottlePipeline pipeline, Endpoint endpoint) {
            this.pipeline = pipeline;
            this.endpoint = endpoint;
        }

        public void run() {
            synchronized (pipeline) {
                String uri = endpoint.getUrlContext();
                int port = endpoint.getUrlPort();
                int resumeCount = endpoint.getMaxConcurrencyLimit() - endpoint.getPendingExchangeReplies();
                String key = uri+":"+port;
                ConcurrentLinkedQueue<SelectionKeyByteBuffer> linkQ = pipeline.suspendedKeys.get(key);
                if (mLogger.isLoggable(Level.FINER)) {
                    mLogger.log(Level.FINER, "Resume SelectionKey(s) triggered; URI '"+uri+
                                             "', port '"+port+
                                             "'; resumeCount= "+resumeCount+
                                             ", suspended size="+linkQ.size());
                }
                if (linkQ != null && linkQ.size() > 0) {
                    for (int i=0; i < resumeCount; i++){
                        // only resume if there are still suspended ones..                        
                        if (linkQ.size()>0) {
                            SelectionKeyByteBuffer skbb = linkQ.poll();
                            try {
                                SelectionKey sk = skbb.getSelectionKey();
                                ByteBuffer bb = skbb.getByteBuffer();
                                if (mLogger.isLoggable(Level.FINER)) {
                                    mLogger.log(Level.FINER, "Request URI '" + uri + "', port '" + port + "'; got cached Selectionkey [" + sk + "] and cached ByteBuffer [" + bb + "]");
                                }        
                                AsyncReadTask task = (AsyncReadTask)st.getReadTask(sk); 
                                task.getByteBuffer().put(bb);
                                task.setBytesAvailable(true);
                                endpoint.incrementPendingExchangeReplies();
                                pipeline.internalAddTask(task); 
                                if (mLogger.isLoggable(Level.FINER)) {
                                    mLogger.log(Level.FINER, "Request URI '" + uri + "', port '" + port + "'; RESUMED Selectionkey [" + task.getSelectionKey() + "],  ByteBuffer [" + task.getByteBuffer() + "] on Task [" + task + "]");
                                }        
                                returnReadByteBuffer(bb);
                            } catch (Throwable t) {
                                String error = mMessages.getString("HTTPBC-E00670.Exception_during_resume_suspended_selectionkey",
                                                                   new Object [] {uri, new Integer(port), skbb.getSelectionKey(), t.getLocalizedMessage()});
                                mLogger.log(Level.WARNING, error, t);
                            }
                        }
                    }

                    if (linkQ.size() == 0) {
                        pipeline.suspendedKeys.remove(key);
                    }
                }                
            }
        }
    }

    public LinkedListThrottlePipeline () {
        super();
    }

    public synchronized void addTask(Task task) {
        if (mLogger.isLoggable(Level.FINER)) {
            mLogger.log (Level.FINER, "addTask called with Task [" + task + "]");
        }
        
        if (task.getType() == Task.READ_TASK) {
            
            String requestURI = null;
            boolean isWsdlQuery = false;
            ByteBuffer bb = getReadByteBuffer();
            
            try {
            	PeekBuffer pb = peekRequestURI((ReadTask)task, bb);
                requestURI = pb.getToken();
                isWsdlQuery = pb.getIsWsdlQuery();
            } catch (Throwable t) {
                AsyncReadTask asyncRT = (AsyncReadTask)task;
                asyncRT.terminate(false);
                returnReadByteBuffer(bb);            
                if (mLogger.isLoggable(Level.FINER)) {
                    mLogger.log (Level.FINER, "Client has closed the socket...");
                }
                return;
            }

            if (requestURI != null) {
                if (mLogger.isLoggable(Level.FINER)) {
                    mLogger.log (Level.FINER, "Request URI is '" + requestURI + "'");
                }
                int port = task.getSelectorThread().getPort();
                Endpoint endpoint = this.lifeCycle.getEndpointBeanForContext(requestURI, port);

                // Only associate throttler controler to endpoint if endpoint is configured for throttling
                if (endpoint != null && endpoint.getMaxConcurrencyLimit() > 0) {
                    int maxConcurrencyLimit = endpoint.getMaxConcurrencyLimit();
                    // associate this controller with endpoint first
                    endpoint.setInboundRequestThrottlingController(this);
                    
                    int pendingExchangeReplies = endpoint.getPendingExchangeReplies();
                    if (mLogger.isLoggable(Level.FINER)) {
                        mLogger.log(Level.FINER, "Found Endpoint for request URI '" + requestURI + "' and port '" + port + "'; maxConcurrencyLimit="+maxConcurrencyLimit+", pendingExchangeReplies="+pendingExchangeReplies);
                    }

                    String key = requestURI+":"+port;
                    // suspend selection key if maxed out concurrent messages
                    if (maxConcurrencyLimit != -1 && (pendingExchangeReplies >= maxConcurrencyLimit) && !isWsdlQuery) {
                        // Disable keep-alive
                        task.getSelectionKey().attach(null);
                        st = task.getSelectorThread();
                        if (!suspendedKeys.containsKey(key)) {
                            suspendedKeys.put(key, new ConcurrentLinkedQueue<SelectionKeyByteBuffer>());
                        }
                        ConcurrentLinkedQueue<SelectionKeyByteBuffer> linkQ = suspendedKeys.get(key);
                        SelectionKey sk = task.getSelectionKey();
                        SelectionKeyByteBuffer skbb = new SelectionKeyByteBuffer(sk, bb);
                        linkQ.offer(skbb);  // add the byte buffer and selection key to our internal cache
                        task.recycle();  // tells Grizzly to reuse the task for the next request
                        st.returnTask(task);
                        if (mLogger.isLoggable(Level.FINER)) {
                            mLogger.log(Level.FINER, "Request URI '" + requestURI + "', port '" + port + "'; SUSPENDED Selectionkey [" + sk + "], ByteBuffer [" + bb + "] from Task [" + task + "]; suspended size="+linkQ.size());
                        }        
                        return;                        
                    }
                }

                // Proceed with the read task - copying the contents of the "temp" ByteBuffer to
                // the one in ReadTask.
                ((ReadTask)task).getByteBuffer().put(bb);  
                if (mLogger.isLoggable(Level.FINER)) {
                    mLogger.log(Level.FINER, "ReadTask ByteBuffer [" + ((ReadTask)task).getByteBuffer()+ "]");
                }
                ((ReadTask)task).setBytesAvailable(true);  // tells Grizzly not to start reading from the stream buffer until the temp buffer is read

                if (endpoint != null && !isWsdlQuery) {
                    endpoint.incrementPendingExchangeReplies();
                }
            }
            
            returnReadByteBuffer(bb);
        }
        super.addTask(task);  // calls the super to resume normal processing of the LinkedListPipeline
    }
    
    public void resumeSuspendedRequests(Endpoint endpoint) {        
        if (resumeThreadPool == null) {
            resumeThreadPool = Executors.newCachedThreadPool();
        }
        resumeThreadPool.submit(new ResumerTask(this,endpoint));        
    }    

    public synchronized void cleanup() {        
        if (resumeThreadPool != null) {
            resumeThreadPool.shutdown();
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, "shutdown resume task thread pool");
            }
        }
        
        // Cancel any suspended tasks
        if (suspendedKeys != null && suspendedKeys.size() > 0) {
            for (Iterator<String> iter1=suspendedKeys.keySet().iterator(); iter1.hasNext();) {
                String uriPort = iter1.next();
                String uri = uriPort.substring(0,uriPort.indexOf(":"));
                String port = uriPort.substring(uriPort.indexOf(":")+1, uriPort.length());
                ConcurrentLinkedQueue<SelectionKeyByteBuffer> linkQ = suspendedKeys.remove(uri);
                if (linkQ != null && linkQ.size() > 0) {
                    for (Iterator iter2=linkQ.iterator();iter2.hasNext();) {
                        SelectionKeyByteBuffer ssbb = linkQ.poll();
                        SelectionKey selkey = ssbb.getSelectionKey();
                        try {
                            Task task = st.getReadTask(selkey);
                            task.cancelTask("No resources available.", HtmlHelper.OK);
                        } catch (Throwable t) {
                            String error = mMessages.getString("HTTPBC-E00671.Exception_during_cancel_suspended_selectionkey",
                                                               new Object [] {uri, port, selkey, t.getLocalizedMessage()});
                            mLogger.log(Level.WARNING, error, t);
                        }
                    }
                }
                if (mLogger.isLoggable(Level.FINER)) {
                    mLogger.log(Level.FINER, "cancelled all suspended selection key read tasks for URI '" + uri + "' on port '" + port + "'");
                }        
            }
        }        
    }
    
    public void setHttpSoapBindingLifeCycle (HttpSoapBindingLifeCycle lifeCycle) {
        this.lifeCycle = lifeCycle;
    }
    
    /***
     * Get the request URI from the <code>ByteBuffer</code>
     */
    protected PeekBuffer peekRequestURI(ReadTask readTask, ByteBuffer byteBuffer) throws Exception {
        SocketChannel socketChannel = 
                    (SocketChannel)readTask.getSelectionKey().channel();
        String token = null;
        boolean isWsdlQuery = false;
        
        if (socketChannel.isOpen()) {
            socketChannel.read(byteBuffer);
            int limit = byteBuffer.limit();
            int position = byteBuffer.position();
			
            if (parse (byteBuffer)) {
                byte[] chars = new byte[byteBuffer.limit() - byteBuffer.position()];

                byteBuffer.get(chars);

                token = new String(chars);

                String normalizedToken = token.toLowerCase();
                if (normalizedToken.indexOf ("?wsdl") != -1) {
                    isWsdlQuery = true;
                }
                
                int index = token.indexOf(0x20);
                if ( index != -1){
                    token = token.substring(0,index);
                }

                // Remove query string.
                index = token.indexOf(QUERY_STRING);
                if ( index != -1){
                    token = token.substring(0,index);
                }

                boolean slash = token.endsWith(PATH_STRING);
                if ( slash ){
                    token = token.substring(0,token.length() -1);
                }  
            } 

            byteBuffer.limit(limit);
            byteBuffer.position(position);
            byteBuffer.flip();
        }
        
        return new PeekBuffer(token, isWsdlQuery);
    }  
    
    protected ByteBuffer getReadByteBuffer() {
        int size = 1028;  // keep it small
        ByteBuffer bb = bbPool.poll();
        if (bb == null) {
            bb = ByteBuffer.allocate(size);
        }
        return bb;                
    }
    
    protected void returnReadByteBuffer(ByteBuffer bb) {
        bb.clear();
        bbPool.offer(bb);
    }
    
    protected void internalAddTask (Task task) {
        super.addTask(task);
    }
    
    /**
     * Parse the request line in search of the context-root bytes of the HTTP
     * Method. The <code>ByteBuffer</code> position and limit refer 
     * respectively to the start and the end of the context root.
     * @param byteBuffer The byteBuffer containing the requests bytes
     * @return true if the context-root has been found.
     */
    public boolean parse(ByteBuffer byteBuffer) {
        boolean isFound = false;
        
        int curPosition = byteBuffer.position();
        int curLimit = byteBuffer.limit();
      
        // Rule a - If nothing, return to the Selector.
        if (byteBuffer.position() == 0)
            return false;
       
        byteBuffer.position(0);
        byteBuffer.limit(curPosition);
        int state =0;
        int start =0;
        int end = 0;        
        
        try {                         
            byte c;            
            
            // Rule b - try to determine the context-root
            while(byteBuffer.hasRemaining()) {
                c = byteBuffer.get();

                // State Machine
                // 0 - Search for the first SPACE ' ' between the method and the
                //     the request URI
                // 1 - Search for the second SPACE ' ' between the request URI
                //     and the method
                switch(state) {
                    case 0: // Search for first ' '
                        if (c == 0x20){
                            state = 1;
                            start = byteBuffer.position();
                            //start = byteBuffer.position() + 1;
                        }
                        break;
                    case 1: // Search for next ' '
                        if (c == 0x20){
                            end = byteBuffer.position() - 1;
                            return true;
                        }
                        break;
                    default:
                        throw new IllegalArgumentException("Unexpected state");
                }      
            }
            return false;
        } catch (BufferUnderflowException bue) {
            return false;
        } finally {     
            if ( end > 0 ){
                byteBuffer.position(start);
                byteBuffer.limit(end);
            } else {
                byteBuffer.limit(curLimit);
                byteBuffer.position(curPosition);                               
            }
        }       
    }
    
    class PeekBuffer {
        private boolean isWsdlQuery = false;
        private String token;
        
        PeekBuffer(String token, boolean isWsdlQuery) {
            this.token = token;
            this.isWsdlQuery = isWsdlQuery;
        }
        
        String getToken() {
            return this.token;
        }
        
        boolean getIsWsdlQuery() {
            return this.isWsdlQuery;
        }
    }
}
