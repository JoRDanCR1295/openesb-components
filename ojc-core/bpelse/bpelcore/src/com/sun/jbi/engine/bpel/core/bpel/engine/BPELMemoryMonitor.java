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
 * @(#)BPELMemoryMonitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;

public interface BPELMemoryMonitor {

	/* 
     * The following power exponent will be used to for for identifying qualifying 
     * instances so that the older instances are picked before the more recent ones.
     * The Scalability logic works in graduated scale where first the most idle instances
     * will be picked, if that does not release enough memory (as per defined levels)
     * the idleness criterion will be reduced by half and tried again, this call is made
     * recursively. The following multiplier value will ensure that the user configure value
     * will be reached in 5 iterations (hence restricting the stack size also) 
     * This in effect means that for a user setting of 1 secs, in first round instances
     * that are older than 32 secs will be tried, for a setting of 5 secs, this would mean
     * around 2.5 minutes and so on.
     */
	public static final int IDLENSS_EXPONENT = 5;
	
    
	public boolean isScalabilityEnabled();
	
 
	/**
	 * The api checks the current heap levels compared to  
	 * the level as defined by the Upper Memory Threshold.
	 * 
	 * @return boolean true if the current levels are less 
	 * 					than as defined by the Upper Memory Threshold.
	 */
	public abstract boolean isBelowUpperMemoryThreshold();


    /**
	 * The api checks the current heap levels compared to  
	 * the level as defined by the Lower Memory Threshold.
	 * 
	 * @return boolean true if the current levels are less 
	 * 					than as defined by the Lower Memory Threshold.
     * @return
     */
    public abstract boolean isBelowLowerMemoryThreshold();
    

	public boolean isPhase1Enabled();

	public boolean isPhase2Enabled();
	
	public long getIdleThresholdStart();
	
	public long getIdleThresholdEnd();

	public float getLowerMemoryThreshold();

	public float getUpperMemoryThreshold();
}