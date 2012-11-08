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
 * @(#)BPELMemoryMonitorTestImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.bpel.jbiadaptor.test.scalability;

import java.util.List;

import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELMemoryMonitorImpl;

public class BPELMemoryMonitorTestImpl extends BPELMemoryMonitorImpl {

    List ph1LowerFailurePoints;
    int currentCounterForPh1LowerCheck = 0;
    
    // The following is in seconds
	public static final long JUNIT_IDLE_THRESHOLD = 2;
	
    public BPELMemoryMonitorTestImpl() {
    	super(Engine.SCALABILITY_LEVEL_PHASE2, Engine.UPPER_MEMORY_THRESHOLD_FACTORYDEFAULT, Engine.LOWER_MEMORY_THRESHOLD_FACTORYDEFAULT, JUNIT_IDLE_THRESHOLD);
    }
    
    public boolean isBelowUpperMemoryThreshold() {

        if (ph1LowerFailurePoints != null) {
            Object firstValue = ph1LowerFailurePoints.get(0);
            
            if (firstValue instanceof String && ((String)firstValue).equals("ALL_FAIL")) {
                return false;
            } else {
                currentCounterForPh1LowerCheck++;
                if (ph1LowerFailurePoints.contains(currentCounterForPh1LowerCheck)) {
                    // check next value, if it is NEXT_ALL_FAIL set the first value as ALL_FAIL
                    // so that subsequent calls to this method would always return false
                    int index = ph1LowerFailurePoints.indexOf(currentCounterForPh1LowerCheck);
                    if (ph1LowerFailurePoints.size() > (index + 1)) {
                        Object nextValue = ph1LowerFailurePoints.get(index + 1);
                        if (nextValue instanceof String && ((String)nextValue).equals("NEXT_ALL_FAIL")) {
                            ph1LowerFailurePoints.add(0, "ALL_FAIL");
                        }
                    }
                    return false;
                }  
                return true;
            }
        } else {
            return true;
        }
    }

    public boolean isBelowLowerMemoryThreshold() {
        return false;
    }

    public boolean passesPh2LowerFreeMemoryRatio() {
        return false;
    }

    public boolean passesPh2UpperFreeMemoryRatio() {
        return false;
    }
    
    public void setPh1LowerFailureCallPoints(List failPoints) {
        resetPh1CheckCount();
        this.ph1LowerFailurePoints = failPoints;
    }

    private void resetPh1CheckCount() {
        currentCounterForPh1LowerCheck = 0;
    }
    
}
