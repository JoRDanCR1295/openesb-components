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
 * @(#)BPELMemoryMonitorImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELMemoryMonitor;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;

/**
 * @author mbhasin
 * 
 *  If the free memory falls below ph1LowerFreeMemory ratio,
 *  phase 1 scalability solution will kick in and passivate the in-memory
 *  variables of long running instances. This phase solution will continue till
 *  free memory defined by ph1UpperFreeMemory is reached or
 *  no more pending instances can be identified to variable passivation.
 */

public class BPELMemoryMonitorImpl implements BPELMemoryMonitor {

	boolean mIsScalabilityEnabled;
	boolean mIsPhase1Enabled;
	boolean mIsPhase2Enabled;
    

	private long mIdleThresholdStart;
	private long mIdleThresholdEnd;
    
    /* 
     * This is the trigger for Phase 1 Solution. Also, after Phase 1
     * is completed, if the free memory is still not more than as defined by
     * Lower Free Memory Ratio, Phase 2 will kick in
     */
    private float mLowerMemoryThreshold;
    
    /* 
     * Scalability Solution (Phase 1 and Phase 2, if needed) when kicks in 
     * will work till the free memory reaches Upper Free Memory Ratio
     */  
    private float mUpperMemoryThreshold;


    public BPELMemoryMonitorImpl(String scalabilityLevel, float upperMemoryThreshold,
			float lowerMemoryThreshold, long idleThreshold) {

    	if (scalabilityLevel.equals(Engine.SCALABILITY_LEVEL_NONE)) {
    		this.mIsScalabilityEnabled = false;
    		this.mIsPhase1Enabled = false;
    		this.mIsPhase2Enabled = false;

    	} else {
    		this.mIsScalabilityEnabled = true;
    		
    		if (scalabilityLevel.equals(Engine.SCALABILITY_LEVEL_PHASE2)) {
    			this.mIsPhase1Enabled = true;
    			this.mIsPhase2Enabled = true;
    		} else if (scalabilityLevel.equals(Engine.SCALABILITY_LEVEL_PHASE1)) {
    			this.mIsPhase1Enabled = true;
    		}
    		
    		this.mUpperMemoryThreshold = upperMemoryThreshold;
    		this.mLowerMemoryThreshold = lowerMemoryThreshold;
    		
    		this.mIdleThresholdStart = new Double(Math.pow(idleThreshold, IDLENSS_EXPONENT)).longValue() * 1000;
    		this.mIdleThresholdEnd = idleThreshold * 1000;
    	}
    }
    
    public boolean isScalabilityEnabled() {
    	return mIsScalabilityEnabled;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELMemoryMonitor#isBelowUpperMemoryThreshold()
     */
    public boolean isBelowUpperMemoryThreshold() {
        return mUpperMemoryThreshold > getCurrentMemoryUtilization();
    }


    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELMemoryMonitor#isBelowLowerMemoryThreshold()
     */
    public boolean isBelowLowerMemoryThreshold() {
        return mLowerMemoryThreshold > getCurrentMemoryUtilization();
    }
    

	public boolean isPhase1Enabled() {
		return mIsPhase1Enabled;
	}

	public boolean isPhase2Enabled() {
		return mIsPhase2Enabled;
	}
	
	public long getIdleThresholdStart() {
		return mIdleThresholdStart;
	}
	
	public long getIdleThresholdEnd() {
		return mIdleThresholdEnd;
	}
	
	public float getLowerMemoryThreshold() {
		return mLowerMemoryThreshold;
	}

	public float getUpperMemoryThreshold() {
		return mUpperMemoryThreshold;
	}
	
    private float getCurrentMemoryUtilization() {
        float freeMem = Runtime.getRuntime().freeMemory();
        float totalMem = Runtime.getRuntime().totalMemory();
        
        float usedMem = totalMem - freeMem;
        float maxMem = Runtime.getRuntime().maxMemory();
        
        float memUtilization = usedMem/maxMem;
        return memUtilization;
    }
}
