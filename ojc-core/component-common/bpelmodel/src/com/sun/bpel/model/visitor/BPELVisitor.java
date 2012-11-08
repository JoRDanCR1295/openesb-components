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
 * @(#)BPELVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.visitor;

import com.sun.bpel.model.Assign;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Branches;
import com.sun.bpel.model.Case;
import com.sun.bpel.model.Catch;
import com.sun.bpel.model.CatchAll;
import com.sun.bpel.model.Compensate;
import com.sun.bpel.model.CompensateScope;
import com.sun.bpel.model.CompensationHandler;
import com.sun.bpel.model.CompletionCondition;
import com.sun.bpel.model.Condition;
import com.sun.bpel.model.Copy;
import com.sun.bpel.model.Correlation;
import com.sun.bpel.model.CorrelationSet;
import com.sun.bpel.model.CorrelationSets;
import com.sun.bpel.model.Correlations;
import com.sun.bpel.model.Else;
import com.sun.bpel.model.ElseIf;
import com.sun.bpel.model.Empty;
import com.sun.bpel.model.EventHandlers;
import com.sun.bpel.model.EventHandlersOnAlarm;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.bpel.model.ExtensionAssignOperation;
import com.sun.bpel.model.FaultHandlers;
import com.sun.bpel.model.FinalCounterValue;
import com.sun.bpel.model.Flow;
import com.sun.bpel.model.For;
import com.sun.bpel.model.From;
import com.sun.bpel.model.FromPart;
import com.sun.bpel.model.If;
import com.sun.bpel.model.Import;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.Iterator;
import com.sun.bpel.model.Link;
import com.sun.bpel.model.Links;
import com.sun.bpel.model.Literal;
import com.sun.bpel.model.OnAlarm;
import com.sun.bpel.model.OnMessage;
import com.sun.bpel.model.Otherwise;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.PartnerLinks;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.RepeatEvery;
import com.sun.bpel.model.RepeatUntil;
import com.sun.bpel.model.Reply;
import com.sun.bpel.model.Rethrow;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.Source;
import com.sun.bpel.model.StartCounterValue;
import com.sun.bpel.model.SunExtExpression;
import com.sun.bpel.model.Switch;
import com.sun.bpel.model.Target;
import com.sun.bpel.model.Terminate;
import com.sun.bpel.model.TerminationHandler;
import com.sun.bpel.model.Throw;
import com.sun.bpel.model.To;
import com.sun.bpel.model.ToPart;
import com.sun.bpel.model.Until;
import com.sun.bpel.model.Validate;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.Wait;
import com.sun.bpel.model.While;
import com.sun.bpel.model.extensions.Alert;
import com.sun.bpel.model.extensions.Choose;
import com.sun.bpel.model.extensions.Default;
import com.sun.bpel.model.extensions.ExtensibilityElement;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.model.extensions.Log;
import com.sun.bpel.model.extensions.Trace;
import com.sun.bpel.model.extensions.When;
import com.sun.bpel.xml.common.visitor.AutonomousVisitor;
import com.sun.bpel.xml.common.visitor.ChildrenParentVisitor;
import com.sun.bpel.xml.common.visitor.ParentChildrenParentVisitor;
import com.sun.bpel.xml.common.visitor.ParentChildrenVisitor;
import com.sun.bpel.xml.common.visitor.Visitor;



/** Describes a visitor to the BPEL4WS model tree.
 * <B>Do not just implement the interface only, but also one of the See Also
 * interfaces below.</B>
 *
 * @see AutonomousVisitor
 * @see ParentChildrenVisitor
 * @see ChildrenParentVisitor
 * @see ParentChildrenParentVisitor
 *
 * @author Sun Microsystems
 * @version 
 */
public interface BPELVisitor extends Visitor {
    
    /** Prepares the visitor for use.
     * @param   v   Values to use.
     */
    void prepare(Object[] v);
    
    /** Visits a BPEL document.
     * @param   d   a BPEL document.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(BPELDocument d);

    /** Visits a process element.
     * @param   p   a process element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(BPELProcess p);
    
    /** Visits a Import element.
     * @param   p   a Import element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Import p);
    
    
    /** Visits a partners element.
     * @param   p   a partners element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(PartnerLinks p);
    
    /** Visits a partner element.
     * @param   p   a partner element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(PartnerLink p);
    
    /** Visits a containers element.
     * @param   c   a containers element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Variables c);
    
    /** Visits a container element.
     * @param   c   a container element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Variable c);
    
    /** Visits a correlationSets element.
     * @param   c   a correlationSets element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(CorrelationSets c);
    
    /** Visits a correlationSet element.
     * @param   c   a correlationSet element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(CorrelationSet c);
    
    /** Visits a faultHandlers element.
     * @param   f   a faultHandlers element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(FaultHandlers f);
    
    /** Visits a catch element.
     * @param   c   a catch element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Catch c);
    
    /** Visits a catchAll element.
     * @param   c   a catchAll element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(CatchAll c);
    
    /** Visits a compensationHandler element.
     * @param   c   a compensationHandler element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(CompensationHandler c);
    
    /** Visits a terminationHandler element.
     * @param   t   a terminationHandler element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(TerminationHandler t);
    
    /** Visits a receive element.
     * @param   r   a receive element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Receive r);
    
    /** Visits a target element.
     * @param   t   a target element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Target t);
    
    /** Visits a source element.
     * @param   s   a source element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Source s);
    
    /** Visits a correlations element.
     * @param   c   a correlations element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Correlations c);
    
    /** Visits a correlation element.
     * @param   c   a correlation element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Correlation c);
    
    /** Visits a sequence element.
     * @param   s   a sequence element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Sequence s);
    
    /** Visits a flow element.
     * @param   f   a flow element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Flow f);
    
    /** Visits a links element.
     * @param   l   a links element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Links l);
    
    /** Visits a link element.
     * @param   l   a link element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Link l);
    
    /** Visits a switch element.
     * @param   s   a switch element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Switch s);
    
    /** Visits a case element.
     * @param   c   a case element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Case c);
    
    /** Visits a otherwise element.
     * @param   o   a otherwise element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Otherwise o);
    
    /** Visits a assign element.
     * @param   a   a assign element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Assign a);
    
    /** Visits a copy element.
     * @param   c   a copy element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Copy c);
    
    /** Visits a from element.
     * @param   f   a from element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(From f);
    
    /** Visits a to element.
     * @param   t   a to element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(To t);
    
    /** Visits a reply element.
     * @param   r   a reply element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Reply r);
    
    /** Visits a invoke element.
     * @param   i   a invoke element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Invoke i);
    
    /** Visits a throw element.
     * @param   t   a throw element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Throw t);
    
    /** Visits a terminate element.
     * @param   t   a terminate element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Terminate t);
    
    /** Visits a wait element.
     * @param   w   a terminate wait.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Wait w);
    
    /** Visits a empty element.
     * @param   e   a empty element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Empty e);
    
    /** Visits a while element.
     * @param   w   a while element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(While w);
    
    /** Visits a pick element.
     * @param   p   a pick element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Pick p);
    
    /** Visits a onMessage element.
     * @param   o   a onMessage element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(OnMessage o);
    
    /** Visits a onAlarm element.
     * @param   o   a onAlarm element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(OnAlarm o);
    
    /** Visits a scope element.
     * @param   s   a scope element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Scope s);

    /** Visits a compensate element.
     * @param   c   a compensate element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Compensate c);
    
    /** Visits a compensateScope element.
     * @param   c   a compensateScope element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(CompensateScope c);

    /**
     * Visits a ForEach element.
     *
     * @param   f   a <code>ForEach</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(ForEach f);
    
    /**
     * Visits an extensibility element.
     *
     * @param   e   a <code>ExtensibilityElement</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(ExtensibilityElement e);

    /**
     * Visits a Choose element.
     *
     * @param   c   a <code>Choose</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Choose c);

    /**
     * Visits a When element.
     *
     * @param   w   a <code>When</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(When w);

    /**
     * Visits s Default element.
     *
     * @param   d   a <code>Default</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Default d);

    /**
     * Visits s Condition element.
     *
     * @param   d   a <code>Condition</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Condition d);

    /**
     * Visits s Literal element.
     *
     * @param   d   a <code>Literal</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Literal d);
    
    /**
     * Visits s For element.
     *
     * @param   d   a <code>For</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(For f);
    
    /**
     * Visits s Until element.
     *
     * @param   d   a <code>Until</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Until d);
   
    /**
     * Visits s FromPart element.
     *
     * @param   d   a <code>FromPart</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(FromPart d);
   
    
    /**
     * Visits s ToPart element.
     *
     * @param   d   a <code>ToPart</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(ToPart d);
   
    
    /**
     * Visits s Rethrow element.
     *
     * @param   d   a <code>Rethrow</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Rethrow d);
    
    /**
     * Visits s RepeatUntil element.
     *
     * @param   d   a <code>RepeatUntil</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(RepeatUntil d);
    
    
    /**
     * Visits s If element.
     *
     * @param   d   a <code>If</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(If d);
    
    
    
    /**
     * Visits s ElseIf element.
     *
     * @param   d   a <code>ElseIf</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(ElseIf d);
    
    /**
     * Visits s Else element.
     *
     * @param   d   a <code>Else</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Else d);
    
    /**
     * Visits s Validate element.
     *
     * @param   d   a <code>Validate</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Validate d);
    
    /**
     * Visits s ForEach element.
     *
     * @param   d   a <code>ForEach</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(com.sun.bpel.model.ForEach d);
    
    /**
     * Visits s Iterator element.
     *
     * @param   d   a <code>Iterator</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Iterator d);
    
    /**
     * Visits s StartCounterValue element.
     *
     * @param   d   a <code>StartCounterValue</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(StartCounterValue d);
    
    
    /**
     * Visits s FinalCounterValue element.
     *
     * @param   d   a <code>FinalCounterValue</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(FinalCounterValue d);
    
    /**
     * Visits s CompletionCondition element.
     *
     * @param   d   a <code>CompletionCondition</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(CompletionCondition d);
    
    /**
     * Visits s Branches element.
     *
     * @param   d   a <code>Branches</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Branches d);

    /**
     * Visits an OnEvent in EventHandlers element
     * 
     * @param onEvent a <code>EventHandlersOnEvent</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(EventHandlersOnEvent onEvent);
    
    /**
     * Visits an OnAlarm in EventHandlers element
     * 
     * @param onEvent a <code>EventHandlersOnAlarm</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(EventHandlersOnAlarm onAlarm);
    
    /**
     * Visits an EventHandlers element
     * 
     * @param eventHandlers a <code>EventHandlers</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(EventHandlers eventHandlers);
    
    /**
     * Visits a RepeatEvery element
     * 
     * @param repeatEvery a <code>RepeatEvery</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(RepeatEvery repeatEvery);
    
    /**
     * Visits a Trace element
     * 
     * @param trace a <code>Trace</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Trace trace);
    
    /**
     * Visits a Log element
     * 
     * @param log a <code>Log</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Log log);
    
    /**
     * Visits an Alert element
     * 
     * @param alert an <code>Alert</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Alert alert);

    /**
     * Visits an ExtensionAssignOperation element
     * 
     * @param read an <code>ExtensionAssignOperation</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(ExtensionAssignOperation extensionAssignOperation);

    /**
     * Visits an SunExtExpression element
     * 
     * @param read an <code>SunExtExpression</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(SunExtExpression expression);
}
