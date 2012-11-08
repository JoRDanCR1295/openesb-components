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
 * @(#)RBPELDocumentImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.parser.impl;

import com.sun.bpel.model.Assign;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Case;
import com.sun.bpel.model.Catch;
import com.sun.bpel.model.Compensate;
import com.sun.bpel.model.CompensateScope;
import com.sun.bpel.model.Correlation;
import com.sun.bpel.model.CorrelationSet;
import com.sun.bpel.model.Else;
import com.sun.bpel.model.ElseIf;
import com.sun.bpel.model.Empty;
import com.sun.bpel.model.EventHandlers;
import com.sun.bpel.model.EventHandlersOnAlarm;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.bpel.model.Flow;
import com.sun.bpel.model.From;
import com.sun.bpel.model.If;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.OnAlarm;
import com.sun.bpel.model.OnMessage;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.RepeatUntil;
import com.sun.bpel.model.Reply;
import com.sun.bpel.model.Rethrow;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.Switch;
import com.sun.bpel.model.Terminate;
import com.sun.bpel.model.Throw;
import com.sun.bpel.model.To;
import com.sun.bpel.model.Validate;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Wait;
import com.sun.bpel.model.While;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.model.extensions.When;
import com.sun.bpel.model.impl.BPELDocumentImpl;
import com.sun.bpel.model.impl.CompensateImpl;
import com.sun.bpel.model.impl.CompensateScopeImpl;
import com.sun.bpel.model.meta.impl.RAssignImpl;
import com.sun.bpel.model.meta.impl.RBPELProcessImpl;
import com.sun.bpel.model.meta.impl.RCaseImpl;
import com.sun.bpel.model.meta.impl.RCatchImpl;
import com.sun.bpel.model.meta.impl.RCompensateImpl;
import com.sun.bpel.model.meta.impl.RCompensateScopeImpl;
import com.sun.bpel.model.meta.impl.RCorrelationImpl;
import com.sun.bpel.model.meta.impl.RCorrelationSetImpl;
import com.sun.bpel.model.meta.impl.RElseIfImpl;
import com.sun.bpel.model.meta.impl.RElseImpl;
import com.sun.bpel.model.meta.impl.REmptyImpl;
import com.sun.bpel.model.meta.impl.REventHandlersImpl;
import com.sun.bpel.model.meta.impl.REventHandlersOnAlarmImpl;
import com.sun.bpel.model.meta.impl.REventHandlersOnEventImpl;
import com.sun.bpel.model.meta.impl.RFlowImpl;
import com.sun.bpel.model.meta.impl.RForEachImpl;
import com.sun.bpel.model.meta.impl.RForEachImplSBYN;
import com.sun.bpel.model.meta.impl.RFromImpl;
import com.sun.bpel.model.meta.impl.RIfImpl;
import com.sun.bpel.model.meta.impl.RInvokeImpl;
import com.sun.bpel.model.meta.impl.ROnAlarmImpl;
import com.sun.bpel.model.meta.impl.ROnMessageImpl;
import com.sun.bpel.model.meta.impl.RPickImpl;
import com.sun.bpel.model.meta.impl.RReceiveImpl;
import com.sun.bpel.model.meta.impl.RRepeatUntilImpl;
import com.sun.bpel.model.meta.impl.RReplyImpl;
import com.sun.bpel.model.meta.impl.RRethrowImpl;
import com.sun.bpel.model.meta.impl.RScopeImpl;
import com.sun.bpel.model.meta.impl.RSequenceImpl;
import com.sun.bpel.model.meta.impl.RSwitchImpl;
import com.sun.bpel.model.meta.impl.RTerminateImpl;
import com.sun.bpel.model.meta.impl.RThrowImpl;
import com.sun.bpel.model.meta.impl.RToImpl;
import com.sun.bpel.model.meta.impl.RValidateImpl;
import com.sun.bpel.model.meta.impl.RVariableImpl;
import com.sun.bpel.model.meta.impl.RWaitImpl;
import com.sun.bpel.model.meta.impl.RWhenImpl;
import com.sun.bpel.model.meta.impl.RWhileImpl;


/**
 * Runtime BPEL document implementation
 *
 * @author Sun Microsystems
 */
public final class RBPELDocumentImpl extends BPELDocumentImpl {
    
    private RBPELDocumentImpl() {
        super();
    }

    /**
     * creates BPEL document
     *
     * @return BPELDocument runtime BPEL document implementation
     */
    public static final BPELDocument createBPELDocument() {
        return new RBPELDocumentImpl();
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createProcess()
     */
    public BPELProcess createProcess() {
        return new RBPELProcessImpl(this);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createAssign()
     */
    public Assign createAssign() {
        return new RAssignImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createVariable()
     */
    public Variable createVariable() {
        return new RVariableImpl(this, mVariableCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createEmpty()
     */
    public Empty createEmpty() {
        return new REmptyImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createFlow()
     */
    public Flow createFlow() {
        return new RFlowImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createInvoke()
     */
    public Invoke createInvoke() {
        return new RInvokeImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createPick()
     */
    public Pick createPick() {
        return new RPickImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createReceive()
     */
    public Receive createReceive() {
        return new RReceiveImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createReply()
     */
    public Reply createReply() {
        return new RReplyImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createScope()
     */
    public Scope createScope() {
        return new RScopeImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createSequence()
     */
    public Sequence createSequence() {
        return new RSequenceImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createSwitch()
     */
    public Switch createSwitch() {
        return new RSwitchImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createTerminate()
     */
    public Terminate createTerminate() {
        return new RTerminateImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createThrow()
     */
    public Throw createThrow() {
        return new RThrowImpl(this, mActivityCounter++);
    }

    /** @see com.sun.bpel.model.impl.BPELDocumentImpl#createRethrow()
     */
    public Rethrow createRethrow() {
        return new RRethrowImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createWait()
     */
    public Wait createWait() {
        return new RWaitImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createWhile()
     */
    public While createWhile() {
        return new RWhileImpl(this, mActivityCounter++);
    }


    /**
     * @see com.sun.bpel.model.BPELDocument#createIf()
     */
    public If createIf() {
        return new RIfImpl(this, mActivityCounter++);
    }
    
    /**
     * @see com.sun.bpel.model.BPELDocument#createCase()
     */
    public Case createCase() {
        return new RCaseImpl(this);
    }
    
    /**
     * @see com.sun.bpel.model.BPELDocument#createElseIf()
     */
    public ElseIf createElseIf() {
        return new RElseIfImpl(this);
    }
    
    
    /** @see com.sun.bpel.model.impl.BPELDocumentImpl#createElse()
     */
    public Else createElse() {
        return new RElseImpl(this);
    }

    /** @see com.sun.bpel.model.impl.BPELDocumentImpl#createElse()
    public Else createElse() {
            return super.createElse();
        }


    /**
     * @see com.sun.bpel.model.BPELDocument#createRepeatUntil()
     */
    public RepeatUntil createRepeatUntil() {
        return new RRepeatUntilImpl(this, mActivityCounter++);
    }
    
    /**
     * @see com.sun.bpel.model.BPELDocument#createWhen()
     */
    public When createWhen() {
        return new RWhenImpl(this);
    }
    
    /**
     * @see com.sun.bpel.model.BPELDocument#createFrom()
     */
    public From createFrom() {
        return new RFromImpl(this);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createTo()
     */
    public To createTo() {
        return new RToImpl(this);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createCatch()
     */
    public Catch createCatch() {
        return new RCatchImpl(this, CATCH_START_ID);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createForEach()
     */
    public ForEach createForEachSBYN() {
        return new RForEachImplSBYN(this);
    }
    
    /**
     * @see com.sun.bpel.model.BPELDocument#createForEach()
     */
    public com.sun.bpel.model.ForEach createForEach () {
        return new RForEachImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createOnMessage()
     */
    public OnMessage createOnMessage() {
        return new ROnMessageImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createOnAlarm()
     */
    public OnAlarm createOnAlarm() {
        return new ROnAlarmImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.impl.BPELDocumentImpl#createCorrelation()
     */
    public Correlation createCorrelation() {
        return new RCorrelationImpl(this);
    }

    /**
     * @see com.sun.bpel.model.impl.BPELDocumentImpl#createCorrelationSet()
     */
    public CorrelationSet createCorrelationSet() {
        return new RCorrelationSetImpl(this, mCorrelationCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createReceive()
     */
    public EventHandlersOnEvent createEventHandlersOnEvent() {
        return new REventHandlersOnEventImpl(this, mActivityCounter++);
    }
    
    /**
     * @see com.sun.bpel.model.BPELDocument#createEventHandlersOnAlarm()
     */
    public EventHandlersOnAlarm createEventHandlersOnAlarm() {
        return new REventHandlersOnAlarmImpl(this, mActivityCounter++);
    }
    
    /**
     * @see com.sun.bpel.model.BPELDocument#createEventHandlers()
     */
    public EventHandlers createEventHandlers() {
        return new REventHandlersImpl(this, mActivityCounter++);
    }
    
    /** @see com.sun.bpel.model.BPELDocument#createCompensate()
     */
    public Compensate createCompensate() {
        return new RCompensateImpl(this, mActivityCounter++);
    }
    
    /** @see com.sun.bpel.model.BPELDocument#createCompensateScope()
     */
    public CompensateScope createCompensateScope() {
        return new RCompensateScopeImpl(this, mActivityCounter++);
    }

    /**
     * @see com.sun.bpel.model.BPELDocument#createValidate()
     */
    @Override
    public Validate createValidate() {
        return new RValidateImpl(this, mActivityCounter++);
    }


    
}
