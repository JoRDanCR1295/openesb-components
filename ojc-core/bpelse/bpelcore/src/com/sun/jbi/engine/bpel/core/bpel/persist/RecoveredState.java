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
 * @(#)RecoveredState.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist;

import java.util.List;
import java.util.Map;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.dbo.EventHandlerDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.LastCheckPointDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.PartnerLinkDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.ScopeDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.VariableDBO;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELInterpreter;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;

/**
 * Defines contract for state recovered from engine persistence.
 * 
 * @author Kevan Simpson
 */
public interface RecoveredState {

	public static final String IDS_SEPARATOR = "-";
	
    void addCallFrame(ICallFrame frame);

    boolean containsCheckpoint(long actId);

    boolean continueTraversing();

    LastCheckPointDBO findCheckpoint(long actId);

    List<EventHandlerDBO> findEventhHandlers(String scopeGuid);
    
    ForEachDBO findForEach(long actId);

    List<ICallFrame> getCallFrames();

    Engine getEngine();

    List<ICallFrame> getParentCFsWithCheckPointedChildCFs();

    List<ForEachDBO> getForEaches();
    
    List<ICallFrame> getInitiatedChildBranches(ICallFrame frame);

    BPELInterpreter getInterpreter();
    
    
    List<ICallFrame> getUninitializedEHCallFrames();
    
    void addUninitializedEHCallFrames(ICallFrame callFrame);
    
    RBPELProcess getProcessDef();

    BPELProcessInstance getProcessInstance();

    ICallFrame getRootCallFrame();

    boolean hasCheckpoints();
    
    boolean canScheduleRecoveredEventFrame(String onEventEHId);
    
	List<ScopeDBO> getScopeDBOs(String scopeInstancesKey);

	Map <String, VariableDBO> getVariables();
	
    Map <String, PartnerLinkDBO> getPartnerLinks();
    
	boolean isEHAssociatedScopeComplete(String ehId);
}
