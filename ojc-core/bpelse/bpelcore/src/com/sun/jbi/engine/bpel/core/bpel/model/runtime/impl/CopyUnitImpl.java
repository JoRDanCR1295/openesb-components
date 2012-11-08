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
 * @(#)CopyUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.Iterator;

import com.sun.bpel.model.Copy;
import com.sun.bpel.model.From;
import com.sun.bpel.model.To;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.util.FromEvaluatorFactory;
import com.sun.jbi.engine.bpel.core.bpel.util.ToExecutorFactory;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.jbi.engine.bpel.core.bpel.util.FromEvaluatorFactory.FromEvaluator;
import com.sun.jbi.engine.bpel.core.bpel.util.ToExecutorFactory.ToExecutor;

/**
 * Implements Assign.copy
 * 
 * @author Sun Microsystems
 */
public class CopyUnitImpl {
	
	public static final String YES = "yes";

    public void doCopy(ICallFrame frame, Copy copy, Context ctx) throws Exception {
        
        frame.onLineChange(frame.getProgramCounter(), copy);
        
        // Check to see if missing from data is to ignored. 
        // Check process level value first.
        boolean ignoreMissingFromData = frame.getProcessInstance().getBPELProcessManager().ignoreMissingFromData();
        
        // If process level value is false check at the copy level.
        if (!ignoreMissingFromData) {
        	ignoreMissingFromData = (YES.equals(copy.getIgnoreMissingFromData())) ? true : false;
        }
        
        // evaluate From
        From from = copy.getFrom();
        FromEvaluator fromEval = FromEvaluatorFactory.getFromEvaluator(from);
        To to = copy.getTo();
        
        Object fromVal = null;
        
        try {
        	fromVal = fromEval.evaluateFrom(from, ctx, to);
        } catch (StandardException se) {
            Fault fault = se.getFault();
            if (ignoreMissingFromData
                    && (StandardException.Fault.UninitializedVariable.equals(fault) 
                            || StandardException.Fault.SelectionFailure.equals(fault))) {
                throw new IgnoreMissingFromDataException();
            }
            throw se;
        }
        
        // If the value is null or is an empty iterator, we throw a selection failure
        if (fromVal == null || (fromVal instanceof Iterator && !((Iterator) fromVal).hasNext())) {
        	if (ignoreMissingFromData) {
        		throw new IgnoreMissingFromDataException();
        	}
        	throw Utility.selectionFailure(frame.getProcess().getBPELId(), 
        	        copy.getFrom().getLocator().getLineNumber());
        }

        ToExecutor toExec = ToExecutorFactory.getToExecutor(to);
        try {
        	toExec.executeTo(to, ctx, fromVal, frame);
        } catch (StandardException se) {
        	throw se;
        } catch (Exception e) {
        	throw new StandardException(Fault.InvalidVariables, e.getLocalizedMessage(), e);
        }
        
        frame.onSubActivityComplete(frame.getProgramCounter(), copy);
    }
}
