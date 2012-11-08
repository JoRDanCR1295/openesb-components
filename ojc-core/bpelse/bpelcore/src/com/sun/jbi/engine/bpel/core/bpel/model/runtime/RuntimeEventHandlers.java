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
 * @(#)RuntimeEventHandlers.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

import com.sun.bpel.model.EventHandlersOnAlarm;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;

/**
 * The runtime unit for EventHandlers. RuntimeEventHandlers
 * creates <code>OnEventUnit</code> and <code>OnAlarmUnit</code>,
 * it also provides callback for the completion of the enclosing scope or
 * process intance and each OnEventUnit and OnAlarmUnit
 * 
 * @see EventHandlersOnEventUnit
 * @see EventHandlersOnAlarmUnit
 * @see ScopeOrProcessUnit
 * 
 * @author Sun Microsystems
 *
 */
public interface RuntimeEventHandlers {
    
    /**
     * Create an EventHandlersOnEventUnit and optionally enable it
     * @param onEvent    EventHandlersOnEvent model
     */
    void createAndSchedule(EventHandlersOnEvent onEvent);
   
    /** We need this API to set the context and the state context properly. This way we can
     * have some abstraction for instance, Only RuntimeEventHandlers knows about the StateContext.
     * @param onAlarm
     * @param repeatWaitTime
     */
    void createAndSchedule(EventHandlersOnAlarm onAlarm, long repeatWaitTime);

     /**
      * Callback when the enclosed scope within onEvent or onAlarm starts. 
      * @param onEventOronAlarm Either <code>EventHandlersOnEventUnit</code> or <code>EventHandlersOnAlarmUnit</code>
      * @param callFrame
      * @return <code>true</code> if the task can start. In the case that the scope associated with this 
      * EventHandlerUnit is completed, it will return false.
      */
     boolean onAlarmStarts(EventHandlersChildAct onEventOronAlarm, ICallFrame callFrame);
     
    /**
     * @param onEventOronAlarm
     * @param waitingForEvent
     * @param callFrame
     * @return Object[] {Message, boolean}. The boolean is true if the associated scope 
     * is already complete. Else it will be false
     */
    Object[] onEventStarts(EventHandlersChildAct onEventOronAlarm, 
            InComingEventKeyImpl waitingForEvent, ICallFrame callFrame);
     
     /**
      * Callback when the enclosed scope within onEvent or onAlarm starts
      * @param onEventOronAlarm
      */
     void taskCompletes(EventHandlersChildAct onEventOronAlarm, RequiredObjects rObjs);

     /**
      * Invoked when the associated scope or process is to completed. After the scope or process completes it informs 
      * the RuntimeEventHandlers that it is done and waits for it to clean up and complete. 
      * 
      * @param callFrame The callframe associated with the scope
      *
      */
     void associatedScopeCompletes();     
     
     /** schedules the onEvents and onAlarms
     */
    void startEventHandlers();
    
    /** TODO JavaDOc
     * @param onEvent
     * @param eveHdlrStateId TODO
     * @return
     */
    ICallFrame createFrameForRecovery(EventHandlersOnEvent onEvent, String eveHdlrStateId);
 
    /** Creates Callframes for all the onAlarms that have not yet been persisted 
     * before the crash
     * @param onAlarm
     * @return
     */
    ICallFrame createFrameForRecovery(EventHandlersOnAlarm onAlarm);
    
    /** construct the call frame for the onAlarm that was persisted before the crash
     * @param onAlarm
     * @param eveHdlrStateId
     * @param timerVal
     * @param repeatEveryValue
     * @return
     */
    ICallFrame createFrameForRecovery(EventHandlersOnAlarm onAlarm, String eveHdlrStateId,
            long timerVal, long repeatEveryValue);
    
    /**
     * @param onAlarm
     * @param eveHdlrStateId
     * @return
     */
    EventHandlersOnAlarmUnit createOnAlarmUnitForRecovery(EventHandlersOnAlarm onAlarm, 
            String eveHdlrStateId);
    
    /**
     * @param onEvent
     * @param eveHdlrStateId
     * @return
     */
    EventHandlersOnEventUnit createOnEventUnitForRecovery(EventHandlersOnEvent onEvent, 
            String eveHdlrStateId);
}
