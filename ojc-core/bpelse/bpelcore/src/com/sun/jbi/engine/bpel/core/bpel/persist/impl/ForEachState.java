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
 * @(#)ForEachState.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist.impl;

/**
 * A data structure representing the state of a ForEach activity.
 * 
 * @author Kevan Simpson
 */
class ForEachState {
    private long mForEachId;
    private int mCounter, mSuccesses, mStartCount, mFinalCount, mCompletionCount;
    private boolean mInserted = false, mStarted = false, mDirty = true;
    
    public ForEachState(long forEachId) {
        mForEachId = forEachId;
    }
    
    /** Prepares this object for a new execution. */
    public void reset(int startCount, int finalCount, int completionCount) {
        mStartCount = startCount;
        mFinalCount = finalCount;
        mCompletionCount = completionCount;
    }

    /** @return Returns the iteration counter. */
    public int getCounter() { 
        return mCounter; 
    }
    /** @return Returns the ForEach activity id. */
    public long getForEachId() { 
        return mForEachId; 
    }
    /** @return Returns the count of successful iterations. */
    public int getSuccesses() { 
        return mSuccesses; 
    }
    /** @return Returns the start counter value. */
    public int getStartCount() { 
        return mStartCount; 
    }
    /** @return Returns the final counter value. */
    public int getFinalCount() { 
        return mFinalCount; 
    }
    /** @return Returns the completion condition counter. */
    public int getCompletionCount() { 
        return mCompletionCount; 
    }
    
    public void updateState(int counter, int successes) {
        if (mCounter != counter || mSuccesses != successes) mDirty = true;
        mCounter = counter; 
        mSuccesses = successes; 
    }

    public boolean isInserted() { 
        return mInserted; 
    }
    public void markInserted() { 
        mInserted = true; 
    }
    
    public boolean isStarted() {
        return mStarted;
    }
    public void start() {
        mStarted = true;
    }
    public void end() {
        mStarted = false;
    }
    
    public boolean isDirty() {
        return mDirty;
    }
    public void markClean() {
        mDirty = false;
    }
}
