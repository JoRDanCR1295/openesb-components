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
 * @(#)SNMPRAConfig.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpengine;


/**
 * Engine configuration
 * 
 * @author fkieviet
 */
public class SNMPRAConfig implements Cloneable {
    private int mPort;
    private SNMPPacketInterceptor mSecondaryInterceptor;
    private SNMPPacketInterceptor mPrimaryInterceptor;
    private SNMPQueryInterceptor mQueryInterceptor;
    private int mNThreads = 1;
    private int mBatchSize = 1;
    private int mMaxWait = 0;
    private int mProcessorBatchSize = 1;
    private int mProcessorMaxWait = 0;
    private int mQueryBatchSize = 1;
    private int mQueryMaxWait = 0;
    private int mTrapBatchSize = 1;
    private int mTrapMaxWait = 0;
    private int mProcessorMaxConcurrency = 1;
    private int mTrapMaxConcurrency = 1;
    private int mQueryMaxConcurrency = 1;
    private boolean mIsAsync = false;
    
    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return String.format("(threads=%d, pbatch=%d, qbatch=%d, tbatch=%d, " +
            "pwait=%d, qwait=%d, twait=%d, pmaxc=%d, qmaxc=%d)", 
            mNThreads, mProcessorBatchSize, mQueryBatchSize, mTrapBatchSize,
            mProcessorMaxWait, mQueryMaxWait, mTrapMaxWait, mProcessorMaxConcurrency, mQueryMaxConcurrency);
    }

    /**
     * @param waitInMilliseconds how long to wait for a full buffer
     */
    public void setMaxWait(int waitInMilliseconds) {
        mMaxWait = waitInMilliseconds;
        mTrapMaxWait = waitInMilliseconds;
        mProcessorMaxWait = waitInMilliseconds;
        mQueryMaxWait = waitInMilliseconds;
    }

    /**
     * Configures the port to listen on
     * 
     * @param port port
     */
    public void setPort(int port) {
        mPort = port;
    }
    
    /**
     * @param numberOfTraps how many traps in a batch
     */
    public void setBatchSize(int numberOfTraps) {
        mBatchSize = numberOfTraps;
        mTrapBatchSize = numberOfTraps;
        mQueryBatchSize = numberOfTraps;
        mProcessorBatchSize = numberOfTraps;
    }
    
    /**
     * Called on the primary receive thread
     * 
     * @param s interceptor
     */
    public void setPrimaryInterceptor(SNMPPacketInterceptor s) {
        mPrimaryInterceptor = s;
    }
        
    /**
     * Called on the packet processing thread
     * 
     * @param s interceptor
     */
    public void setSecondayInterceptor(SNMPPacketInterceptor s) {
        mSecondaryInterceptor = s;
    }

    /**
     * @param threads number of processors in TrapQueue
     */
    public void setNThreads(int threads) {
        mNThreads = threads;
    }

    /**
     * Getter for mBatchSize
     *
     * @return int
     */
    public int getBatchSize() {
        return mBatchSize;
    }

    /**
     * Getter for mMaxWait
     *
     * @return int
     */
    public int getMaxWait() {
        return mMaxWait;
    }

    /**
     * Getter for mNThreads
     *
     * @return int
     */
    public int getNThreads() {
        return mNThreads;
    }

    /**
     * Getter for mPort
     *
     * @return int
     */
    public int getPort() {
        return mPort;
    }

    /**
     * Getter for mPrimaryInterceptor
     *
     * @return SNMPPacketInterceptor
     */
    public SNMPPacketInterceptor getPrimaryInterceptor() {
        return mPrimaryInterceptor;
    }

    /**
     * Getter for mSecondaryInterceptor
     *
     * @return SNMPPacketInterceptor
     */
    public SNMPPacketInterceptor getSecondaryInterceptor() {
        return mSecondaryInterceptor;
    }

    /**
     * @return batch size
     */
    public int getProcessorBatchSize() {
        return mProcessorBatchSize;
    }

    /**
     * @return maxwait
     */
    public int getProcessorMaxWait() {
        return mProcessorMaxWait;
    }

    /**
     * Setter for processorBatchSize
     *
     * @param processorBatchSize intThe processorBatchSize to set.
     */
    public void setProcessorBatchSize(int processorBatchSize) {
        this.mProcessorBatchSize = processorBatchSize;
    }

    /**
     * Setter for processorMaxWait
     *
     * @param processorMaxWait intThe processorMaxWait to set.
     */
    public void setProcessorMaxWait(int processorMaxWait) {
        this.mProcessorMaxWait = processorMaxWait;
    }

    /**
     * Getter for mQueryBatchSize
     *
     * @return int
     */
    public int getQueryBatchSize() {
        return mQueryBatchSize;
    }

    /**
     * Setter for mQueryBatchSize
     *
     * @param queryBatchSize intThe mQueryBatchSize to set.
     */
    public void setQueryBatchSize(int queryBatchSize) {
        mQueryBatchSize = queryBatchSize;
    }

    /**
     * Getter for mQueryMaxWait
     *
     * @return int
     */
    public int getQueryMaxWait() {
        return mQueryMaxWait;
    }

    /**
     * Setter for mQueryMaxWait
     *
     * @param queryMaxWait intThe mQueryMaxWait to set.
     */
    public void setQueryMaxWait(int queryMaxWait) {
        mQueryMaxWait = queryMaxWait;
    }

    /**
     * Getter for mProcessorMaxConcurrency
     *
     * @return int
     */
    public int getProcessorMaxConcurrency() {
        return mProcessorMaxConcurrency;
    }

    /**
     * Setter for mProcessorMaxConcurrency
     *
     * @param processorMaxConcurrency intThe mProcessorMaxConcurrency to set.
     */
    public void setProcessorMaxConcurrency(int processorMaxConcurrency) {
        mProcessorMaxConcurrency = processorMaxConcurrency;
    }

    /**
     * Getter for mQueryMaxConcurrency
     *
     * @return int
     */
    public int getMQueryMaxConcurrency() {
        return mQueryMaxConcurrency;
    }

    /**
     * Setter for mQueryMaxConcurrency
     *
     * @param queryMaxConcurrency intThe mQueryMaxConcurrency to set.
     */
    public void setMQueryMaxConcurrency(int queryMaxConcurrency) {
        mQueryMaxConcurrency = queryMaxConcurrency;
    }

    /**
     * Getter for mTrapBatchSize
     *
     * @return int
     */
    public int getTrapBatchSize() {
        return mTrapBatchSize;
    }

    /**
     * Setter for mTrapBatchSize
     *
     * @param trapBatchSize intThe mTrapBatchSize to set.
     */
    public void setTrapBatchSize(int trapBatchSize) {
        mTrapBatchSize = trapBatchSize;
    }

    /**
     * Getter for mTrapMaxWait
     *
     * @return int
     */
    public int getTrapMaxWait() {
        return mTrapMaxWait;
    }

    /**
     * Setter for mTrapMaxWait
     *
     * @param trapMaxWait intThe mTrapMaxWait to set.
     */
    public void setTrapMaxWait(int trapMaxWait) {
        mTrapMaxWait = trapMaxWait;
    }

    /**
     * Getter for mQueryInterceptor
     *
     * @return SNMPQueryInterceptor
     */
    public SNMPQueryInterceptor getQueryInterceptor() {
        return mQueryInterceptor;
    }

    /**
     * Setter for mQueryInterceptor
     *
     * @param queryInterceptor SNMPQueryInterceptorThe mQueryInterceptor to set.
     */
    public void setQueryInterceptor(SNMPQueryInterceptor queryInterceptor) {
        mQueryInterceptor = queryInterceptor;
    }

    /**
     * Getter for mTrapMaxConcurrency
     *
     * @return int
     */
    public int getTrapMaxConcurrency() {
        return mTrapMaxConcurrency;
    }

    /**
     * Setter for mTrapMaxConcurrency
     *
     * @param trapMaxConcurrency intThe mTrapMaxConcurrency to set.
     */
    public void setTrapMaxConcurrency(int trapMaxConcurrency) {
        mTrapMaxConcurrency = trapMaxConcurrency;
    }

    /**
     * Getter for mIsAsync
     *
     * @return boolean
     */
    public boolean getIsAsync() {
        return mIsAsync;
    }

    /**
     * Setter for mIsAsync
     *
     * @param isAsync booleanThe mIsAsync to set.
     */
    public void setIsAsync(boolean isAsync) {
        mIsAsync = isAsync;
    }
}
