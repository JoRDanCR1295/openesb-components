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
 * @(#)CodeReUseHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;


/**
 * code re-use helper
 *
 * @author Sun Microsystems
 */
public class CodeReUseHelper {
    /**
     * executes child activities
     *
     * @param frame callframe
     * @param bpit BP process instance thread
     * @param rObjs requried objects
     * @param childActUnit activity unit
     *
     * @return boolean: on successfully completion of child activities, returns true; otherwise,
     *         returns false
     *
     * @throws Exception Exception
     */
    public static boolean executeChildActivities( ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects rObjs, ActivityUnit childActUnit) throws Exception {
    	
        boolean childActCompleted = false;

        while ((childActUnit != null)) {
        	
            childActCompleted = childActUnit.doAction(frame, bpit, rObjs);

            if (childActCompleted) {
            	/* Either the activity is incomplete (for example invoke waiting for a response) 
            	 * or was terminated because of a fault in an enclosing the scope. Note that we 
            	 * do not need to check whether the scope is being terminated. There are two cases 
            	 * for the scope being terminated:
            	 * 1. If the call returns to a scope in the hierarchy, the scope will check if
            	 * an ancestor scope has faulted, in which case it will do TH
            	 * 2. The call could return to a doResumeAction of some structured act., in
            	 * which case, that method will check whether the structured acvity needs to be 
            	 * terminated and if yes, will call doPassControlToParent().
            	 */

                //Check whether the structured activity is being terminated
            	if(childActUnit.getEnclosingUnit().getContext().getFaultHandlingContext().isBeingTerminated()){
            		/* We set the current PC to the structured activity to indicate that it was the
            		 * last activity that this thread returned from. Ideally, this should have been done in the
            		 * code for structured activity itself, but since this code-reuse helper acts as part
            		 * of the structured activity unit, we do it here. Note: that if the structured activity is 
            		 * CatchUnit or CatchAllUnit, we don't do this because these are not activities per se, so
            		 * we don't need to terminate them and/or set them as the PC.
            		 */
            		if (!(childActUnit.getEnclosingUnit() instanceof  CatchAllUnitImpl)) {
            			frame.setProgramCounter(childActUnit.getEnclosingActivityUnit());
            			frame.onActivityTerminate(childActUnit.getEnclosingActivityUnit());
            		}
            		return true;
            	}
            	
                childActUnit = childActUnit.getNextActivityUnit();

                /* Note that the childActUnit.getNextActivityUnit() will be null all structured 
                 * activities except SequenceUnitImpl and VirtualCatchAllImpl. For these, before 
                 * we execute the next child activity, we check whether the they need to be terminated. 
                 */
                
            } else {
            	return false;
            }
        }

        //The child activity/activities are complete.
        return true;
    }
}
