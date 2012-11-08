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
 * @(#)AbstractVisitor.java 
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
import com.sun.bpel.model.EventHandlers;
import com.sun.bpel.model.EventHandlersOnAlarm;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.bpel.model.FinalCounterValue;
import com.sun.bpel.model.FromPart;
import com.sun.bpel.model.Iterator;
import com.sun.bpel.model.RepeatEvery;
import com.sun.bpel.model.RepeatUntil;
import com.sun.bpel.model.If;
import com.sun.bpel.model.Rethrow;
import com.sun.bpel.model.StartCounterValue;
import com.sun.bpel.model.TerminationHandler;
import com.sun.bpel.model.ToPart;
import com.sun.bpel.model.Validate;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.Copy;
import com.sun.bpel.model.Correlation;
import com.sun.bpel.model.CorrelationSet;
import com.sun.bpel.model.CorrelationSets;
import com.sun.bpel.model.Correlations;
import com.sun.bpel.model.Empty;
import com.sun.bpel.model.FaultHandlers;
import com.sun.bpel.model.Flow;
import com.sun.bpel.model.For;
import com.sun.bpel.model.From;
import com.sun.bpel.model.Import;
import com.sun.bpel.model.Invoke;
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
import com.sun.bpel.model.Reply;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.Source;
import com.sun.bpel.model.Switch;
import com.sun.bpel.model.Target;
import com.sun.bpel.model.Terminate;
import com.sun.bpel.model.Throw;
import com.sun.bpel.model.To;
import com.sun.bpel.model.Until;
import com.sun.bpel.model.Wait;
import com.sun.bpel.model.While;
import com.sun.bpel.model.common.visitor.ProcessingInstructionVisitor;
import com.sun.bpel.model.common.visitor.VisitorImpl;
import com.sun.bpel.model.extensions.Alert;
import com.sun.bpel.model.extensions.Choose;
import com.sun.bpel.model.extensions.Default;
import com.sun.bpel.model.extensions.ExtensibilityElement;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.model.extensions.Log;
import com.sun.bpel.model.extensions.Trace;
import com.sun.bpel.model.extensions.When;
import com.sun.bpel.xml.common.model.Documentation;
import com.sun.bpel.xml.common.model.XMLComment;
import com.sun.bpel.xml.common.model.XMLProcessingInstruction;
import com.sun.bpel.xml.common.model.XMLText;
import com.sun.bpel.xml.common.visitor.CommentVisitor;
import com.sun.bpel.xml.common.visitor.DocumentationVisitor;
import com.sun.bpel.xml.common.visitor.TextVisitor;


/**
 * A basic implementation for a visitor to a BPEL4WS document.
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class AbstractVisitor extends VisitorImpl
    implements BPELVisitor, CommentVisitor, ProcessingInstructionVisitor,
               TextVisitor, DocumentationVisitor {
    
    /** Creates a new instance of AbstractVisitor */
    public AbstractVisitor() {
        super();
    }
    
    /** Visits a comment section.
     * @param   c   XML comment section
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(XMLComment c) {
        return true;
    }
    
    /** Visits a XML processing instruction section.
     * @param   p   XML processing instruction.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(XMLProcessingInstruction p) {
        return true;
    }
    
    /** Visits a text section.
     * @param   t   XML text section
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(XMLText t) {
        return true;
    }
    
    /** Visits a BPEL document.
     * @param   d   a BPEL document.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(BPELDocument d) {
        return true;
    }
    
    /** Visits a process element.
     * @param   p   a process element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(BPELProcess p) {
        return true;
    }
    
    /** Visits a Import element.
     * @param   p   a Import element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Import p) {
        return true;
    }
    

    /** Visits a partners element.
     * @param   p   a partners element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(PartnerLinks p) {
        return true;
    }
    
    /** Visits a partner element.
     * @param   p   a partner element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(PartnerLink p) {
        return true;
    }
    
    /** Visits a containers element.
     * @param   c   a containers element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Variables c) {
        return true;
    }
    
    /** Visits a container element.
     * @param   c   a container element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Variable c) {
        return true;
    }
    
    /** Visits a correlationSets element.
     * @param   c   a correlationSets element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(CorrelationSets c) {
        return true;
    }
    
    /** Visits a correlationSet element.
     * @param   c   a correlationSet element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(CorrelationSet c) {
        return true;
    }
    
    /** Visits a faultHandlers element.
     * @param   f   a faultHandlers element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(FaultHandlers f) {
        return true;
    }
    
    /** Visits a catch element.
     * @param   c   a catch element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Catch c) {
        return true;
    }
    
    /** Visits a catchAll element.
     * @param   c   a catchAll element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(CatchAll c) {
        return true;
    }
    
    /** Visits a compensationHandler element.
     * @param   c   a compensationHandler element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(CompensationHandler c) {
        return true;
    }
    
    /** Visits a terminationHandler element.
     * @param   t   a terminationHandler element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(TerminationHandler t) {
        return true;
    }
    
    /** Visits a receive element.
     * @param   r   a receive element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Receive r) {
        return true;
    }
    
    /** Visits a target element.
     * @param   t   a target element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Target t) {
        return true;
    }
    
    /** Visits a source element.
     * @param   s   a source element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Source s) {
        return true;
    }
    
    /** Visits a correlations element.
     * @param   c   a correlations element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Correlations c) {
        return true;
    }
    
    /** Visits a correlation element.
     * @param   c   a correlation element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Correlation c) {
        return true;
    }
  
    /** Visits a sequence element.
     * @param   s   a sequence element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Sequence s) {
        return true;
    }
    
    /** Visits a flow element.
     * @param   f   a flow element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Flow f) {
        return true;
    }
     
    /** Visits a links element.
     * @param   l   a links element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Links l) {
        return true;
    }
     
    /** Visits a link element.
     * @param   l   a link element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Link l) {
        return true;
    }
   
    /** Visits a switch element.
     * @param   s   a switch element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Switch s) {
        return true;
    }
    
    /** Visits a case element.
     * @param   c   a case element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Case c) {
        return true;
    }
    
    /** Visits a otherwise element.
     * @param   o   a otherwise element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Otherwise o) {
        return true;
    }
    
    /** Visits a assign element.
     * @param   a   a assign element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Assign a) {
        return true;
    }
    
    /** Visits a copy element.
     * @param   c   a copy element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Copy c) {
        return true;
    }
    
    /** Visits a from element.
     * @param   f   a from element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(From f) {
        return true;
    }
    
    /** Visits a to element.
     * @param   t   a to element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(To t) {
        return true;
    }
    
    /** Visits a reply element.
     * @param   r   a reply element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Reply r) {
        return true;
    }
    
    /** Visits a invoke element.
     * @param   i   a invoke element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Invoke i) {
        return true;
    }
    
    /** Visits a throw element.
     * @param   t   a throw element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Throw t) {
        return true;
    }
    
    /** Visits a terminate element.
     * @param   t   a terminate element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Terminate t) {
        return true;
    }
    
    /** Visits a wait element.
     * @param   w   a terminate wait.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Wait w) {
        return true;
    }
    
    /** Visits a empty element.
     * @param   e   a empty element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Empty e) {
        return true;
    }
    
    /** Visits a while element.
     * @param   w   a while element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(While w) {
        return true;
    }
    
    /** Visits a pick element.
     * @param   p   a pick element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Pick p) {
        return true;
    }
    
    /** Visits a onMessage element.
     * @param   o   a onMessage element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(OnMessage o) {
        return true;
    }
    
    /** Visits a onAlarm element.
     * @param   o   a onAlarm element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(OnAlarm o) {
        return true;
    }
    
    /** Visits a scope element.
     * @param   s   a scope element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Scope s) {
        return true;
    }

    /** Visits a compensate element.
     * @param   c   a compensate element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Compensate c) {
        return true;
    }
  
    /** Visits a compensateScope element.
     * @param   c   a compensateScope element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(CompensateScope c) {
        return true;
    }

    /**
     * Visits the ForEach element
     *
     * @param   f   a <code>ForEach</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(ForEach f) {
        return true;
    }
    
    /**
     * Visits the Trace element
     *
     * @param   t   a <code>Trace</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Trace t) {
        return true;
    }
    
    /**
     * Visits the Log element
     *
     * @param   l   a <code>Log</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Log l) {
        return true;
    }
    
    /**
     * Visits the Alert element
     *
     * @param   a   an <code>Alert</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Alert a) {
        return true;
    }
    

    /**
     * Visits the Choose element
     *
     * @param   c   a <code>Choose</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Choose c) {
        return true;
    }

    /**
     * Visits the When element
     *
     * @param   w   a <code>When</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(When w) {
        return true;
    }
    
    /**
     * Visits the Default element
     *
     * @param   d   a <code>Default</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Default d) {
        return true;
    }


    /**
     * Visits a extensibility element.
     *
     * @param   e   a <code>ExtensibilityElement</code> element
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(ExtensibilityElement e) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(Documentation)
     */
    public boolean visit(Documentation doc) {
        return true;
    }
    
    /**
     * Visits s Condition element.
     *
     * @param   d   a <code>Condition</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Condition d) {
    	return true;
    }
    
    /**
     * Visits s Literal element.
     *
     * @param   d   a <code>Literal</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Literal d) {
    	return true;
    }
    
    /**
     * Visits s For element.
     *
     * @param   d   a <code>For</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(For f) {
    	return true;
    }
    
    /**
     * Visits s Until element.
     *
     * @param   d   a <code>Until</code> element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Until d) {
    	return true;
    }

	public boolean visit(FromPart d) {
		return true;
	}

	public boolean visit(RepeatUntil d) {
		return true;
	}

        public boolean visit(If d) {
            return true;
        }
                
	public boolean visit(Rethrow d) {
		return true;
	}

	public boolean visit(ToPart d) {
		return true;
	}

	public boolean visit(Validate d) {
		return true;
	}
	

	public boolean visit(Branches d) {
		return true;
	}

	public boolean visit(CompletionCondition d) {
		return true;
	}

	public boolean visit(FinalCounterValue d) {
		return true;
	}

	public boolean visit(com.sun.bpel.model.ForEach f) {
		return true;
	}

	public boolean visit(Iterator d) {
		return false;
	}

	public boolean visit(StartCounterValue d) {
		return true;
	}
    
    public boolean visit(EventHandlersOnEvent onEvent) {
        // TODO Auto-generated method stub
        return true;
    }

    public boolean visit(EventHandlersOnAlarm onAlarm) {
        // TODO Auto-generated method stub
        return true;
    }

    public boolean visit(EventHandlers eventHandlers) {
        // TODO Auto-generated method stub
        return true;
    }	
    
    public boolean visit(RepeatEvery repeatEvery) {
        // TODO Auto-generated method stub
        return true;
    }   
    
    
}
