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
 * @(#)SAXWriteVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.visitor;

import java.io.StringWriter;
import java.io.Writer;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Node;

import com.sun.bpel.model.Assign;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Branches;
import com.sun.bpel.model.Case;
import com.sun.bpel.model.Catch;
import com.sun.bpel.model.CatchAll;
import com.sun.bpel.model.Compensate;
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
import com.sun.bpel.model.MultipleActivityHolder;
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
import com.sun.bpel.model.SingleActivityHolder;
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
import com.sun.bpel.model.common.EInsightModelException;
import com.sun.bpel.model.common.visitor.SAXWriterSupport;
import com.sun.bpel.model.common.visitor.XMLWriteVisitorException;
import com.sun.bpel.model.extensions.Alert;
import com.sun.bpel.model.extensions.Choose;
import com.sun.bpel.model.extensions.Default;
import com.sun.bpel.model.extensions.ExtensibilityElement;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.model.extensions.Log;
import com.sun.bpel.model.extensions.Trace;
import com.sun.bpel.model.extensions.When;
import com.sun.bpel.model.util.Utility;
import com.sun.bpel.xml.common.model.Documentation;
import com.sun.bpel.xml.common.model.XMLComment;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.model.XMLProcessingInstruction;
import com.sun.bpel.xml.common.model.XMLText;
import com.sun.bpel.xml.common.visitor.DocumentationVisitor;
import com.sun.bpel.xml.common.visitor.ParentChildrenParentVisitor;


/**
 * Visits all the model nodes and writes out the XML data.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SAXWriteVisitor extends AbstractVisitor
    implements ParentChildrenParentVisitor {
    
    /** Creates a new instance of SAXWriteVisitor */
    public SAXWriteVisitor() {
    }
    
    /** Getter for property writer.
     * @return Value of property writer.
     *
     */
    public Writer getWriter() {
        return getWriterSupport().getWriter();
    }
    
    /** Setter for property writer.
     * @param writer New value of property writer.
     *
     */
    public void setWriter(Writer writer) {
        getWriterSupport().setWriter(writer);
    }
    
    /** Gets the SAX writer visitor support.
     * @return  Visitor support.
     */
    public SAXWriterSupport getWriterSupport() {
        if (null == getVisitorSupport()) {
            setVisitorSupport(new SAXWriterSupport());
        }
        return (SAXWriterSupport) getVisitorSupport();
    }
    
    /** Prepares the visitor for use.
     * @param   v   Values to use.
     *              <ol start="0">
     *              <li><code>java.io.Writer</code> implementing object.</li>
     *              <li><code>Boolean</code> object; <code>TRUE</code> for
     *                  Pretty Print.</li>
     *              <li><code>Boolean</code> object; <code>TRUE</code> for
     *                  Escape Non Ascii characters.</li>
     *              </ol>
     */
    public void prepare(Object[] v) {
        setWriter((Writer) v[0]);
        if (v.length > 1) {
            getWriterSupport().getXmlWriter().setPrettyPrint(
                ((Boolean) v[1]).booleanValue());
            if (v.length > 2) {
                getWriterSupport().getXmlWriter().setEscapeNonAscii(
                    ((Boolean) v[2]).booleanValue());
            }
        }
    }
    
    /** Visits a BPEL document.
     * @param   d   a BPEL document.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(BPELDocument d) {
        try {
            if (getWriterSupport().isElementStart(d)) {                
                getWriterSupport().getXmlWriter().startDocument();
            } else {
                getWriterSupport().getXmlWriter().endDocument();
            }
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException(
                "Cannot serialize BPEL", trw);
        }
        return true;
    }
    
    /** Visits a comment section.
     * @param   c   XML comment section
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(XMLComment c) {
        try {
            getWriterSupport().getXmlWriter().comment(c.getValue());
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException(
                "Cannot serialize XML comment", trw);
        }
        return true;
    }
    
    /** Visits a XML processing instruction section.
     * @param   p   XML processing instruction.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(XMLProcessingInstruction p) {
        try {
            getWriterSupport().getXmlWriter().processingInstruction(
                p.getTarget(), p.getData());
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException(
                "Cannot serialize XML Processing Instruction", trw);
        }
        return true;
    }
    
    /** Visits a process element.
     * @param   p a process element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(BPELProcess p) {
        getWriterSupport().writeElement(p);
        return true;
    }
    
    /** Visits a Import element.
     * @param   p   a Import element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Import p) {
    	getWriterSupport().writeElement(p);
        return true;
    }
    
    /** Visits a partners element.
     * @param   p   a partners element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(PartnerLinks p) {
        getWriterSupport().writeElement(p);
        return true;
    }
    
    /** Visits a partner element.
     * @param   p   a partner element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(PartnerLink p) {
        getWriterSupport().writeEmptyElement(p);
        return true;
    }
    
    /** Visits a containers element.
     * @param   c   a containers element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Variables c) {
        getWriterSupport().writeElement(c);
        return true;
    }
  
    /** Visits a container element.
     * @param   c   a container element.
     * @return  <code>true</code> if traversal is to continue.
     */
    public boolean visit(Variable c) {
        getWriterSupport().writeElement(c);
        return true;
    }
    
    /** Visits a correlationSets element.
     * @param   c   a correlationSets element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CorrelationSets c) {
        getWriterSupport().writeElement(c);
        return true;
    }
    
    /** Visits a correlationSet element.
     * @param   c   a correlationSet element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CorrelationSet c) {
        getWriterSupport().writeEmptyElement(c);
        return true;
    }
    
    /** Visits a faultHandlers element.
     * @param   f   a faultHandlers element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(FaultHandlers f) {
        getWriterSupport().writeElement(f);
        return true;
    }
    
    /** Visits a catch element.
     * @param   c   a catch element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Catch c) {
        getWriterSupport().writeElement(c);
        return true;
    }
    
    /** Visits a catchAll element.
     * @param   c   a catchAll element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CatchAll c) {
        getWriterSupport().writeElement(c);
        return true;
    }
    
    /** Visits a compensationHandler element.
     * @param   c   a compensationHandler element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CompensationHandler c) {
        getWriterSupport().writeElement(c);
        return true;
    }
    
    /** Visits a terminationHandler element.
     * @param   t   a terminationHandler element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(TerminationHandler t) {
        getWriterSupport().writeElement(t);
        return true;
    }
    
    /** Visits a receive element.
     * @param   r   a receive element.
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Receive r) {
        getWriterSupport().writeElement(r);
       
        return true;
    }
    
    /** Visits a target element.
     * @param   t   a target element.
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Target t) {
        getWriterSupport().writeEmptyElement(t);
        return true;
    }
    
    /** Visits a source element.
     * @param   s   a source element.
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Source s) {
        getWriterSupport().writeEmptyElement(s);
        return true;
    }
    
    /** Visits a correlations element.
     * @param   c   a correlations element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Correlations c) {
        getWriterSupport().writeElement(c);
        return true;
    }
    
    /** Visits a correlation element.
     * @param   c   a correlation element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Correlation c) {
        getWriterSupport().writeEmptyElement(c);
        return true;
    }
  
    /** Visits a sequence element.
     * @param   s   a sequence element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Sequence s) {
        getWriterSupport().writeElement(s);
        return true;
    }
    
    /** Visits a flow element.
     * @param   f   a flow element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Flow f) {
        getWriterSupport().writeElement(f);
        return true;
    }
    
    /** Visits a links element.
     * @param   l   a links element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Links l) {
        getWriterSupport().writeElement(l);
        return true;
    }
     
    /** Visits a link element.
     * @param   l   a link element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Link l) {
        getWriterSupport().writeEmptyElement(l);
        return true;
    }
   
    /** Visits a switch element.
     * @param   s   a switch element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Switch s) {
        getWriterSupport().writeElement(s);
        return true;
    }
    
    /** Visits a case element.
     * @param   c   a case element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Case c) {
        getWriterSupport().writeElement(c);
        return true;
    }
    
    /** Visits a otherwise element.
     * @param   o   a otherwise element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Otherwise o) {
        getWriterSupport().writeElement(o);
        return true;
    }
    
    /** Visits a assign element.
     * @param   a   a assign element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Assign a) {
        getWriterSupport().writeElement(a);
        return true;
    }
    
    /** Visits a copy element.
     * @param   c   a copy element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Copy c) {
        getWriterSupport().writeElement(c);
        return true;
    }
    
    /** Visits a from element.
     * @param   f   a from element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(From f) {
        if (f.hasChildren()) {
            getWriterSupport().writeElement(f);
        } else {
            getWriterSupport().writeEmptyElement(f);
        }
        return true;
    }
    
    /** Visits a text section.
     * @param   t   XML text section
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(XMLText t) {
        getWriterSupport().writeText(t);
        return true;
    }
    
    /** Visits a to element.
     * @param   t   a to element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(To t) {
    	if (t.hasChildren()) {
            getWriterSupport().writeElement(t);
        } else {
            getWriterSupport().writeEmptyElement(t);
        }
        return true;
    }
    
    /** Visits a reply element.
     * @param   r   a reply element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Reply r) {
        getWriterSupport().writeElement(r);
        
        return true;
    }
    
    /** Visits a invoke element.
     * @param   i   a invoke element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Invoke i) {        
        // Convert if necessary
        XMLNode parent = i.getParent();
        Scope scope = getWriterSupport().expandInvoke(i);
        
        // Invoke has been expanded; have parent point to new scope instead
        if (scope != null) {
            if (parent instanceof SingleActivityHolder) {
                ((SingleActivityHolder) parent).setActivity(scope);
            } else if (parent instanceof MultipleActivityHolder) {
                MultipleActivityHolder mah = (MultipleActivityHolder) parent;
                mah.setActivity(mah.indexOfActivity(i), scope);
            } else {
                throw new EInsightModelException("Error while expanding Invoke; parent is not an Activity holder!");
            }
            scope.accept(this);
        } else {
            getWriterSupport().writeElement(i);
        }
        
        return true;
    }
    
    /** Visits a throw element.
     * @param   t   a throw element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Throw t) {
        getWriterSupport().writeElement(t);
        return true;
    }
    
    /** Visits a terminate element.
     * @param   t   a terminate element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Terminate t) {
        getWriterSupport().writeElement(t);
        return true;
    }
    
    /** Visits a wait element.
     * @param   w   a terminate wait.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Wait w) {
        getWriterSupport().writeElement(w);
        return true;
    }
    
    /** Visits a empty element.
     * @param   e   a empty element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Empty e) {
        getWriterSupport().writeElement(e);
        return true;
    }
    
    /** Visits a while element.
     * @param   w   a while element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(While w) {
        getWriterSupport().writeElement(w);
        return true;
    }
    
    /** Visits a pick element.
     * @param   p   a pick element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Pick p) {
        getWriterSupport().writeElement(p);
        return true;
    }
    
    /** Visits a onMessage element.
     * @param   o   a onMessage element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(OnMessage o) {
        getWriterSupport().writeElement(o);
        
        return true;
    }
    
    /** Visits a onAlarm element.
     * @param   o   a onAlarm element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(OnAlarm o) {
        getWriterSupport().writeElement(o);
        return true;
    }
    
    /** Visits a scope element.
     * @param   s   a scope element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Scope s) {
        getWriterSupport().writeElement(s);
        return true;
    }

    /** Visits a compensate element.
     * @param   c   a compensate element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Compensate c) {
        getWriterSupport().writeElement(c);
        return true;
    }
    

    /**
     * Visits a ForEach Element
     *
     * @param f a <code>ForEach</code> element
     * @return a <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(ForEach f) {
        getWriterSupport().writeElement(f);
        return true;
    }
    
    /**
     * Visits a Trace Element
     *
     * @param t a <code>Trace</code> element
     * @return a <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Trace t) {
        getWriterSupport().writeElement(t);
        return true;
    }
    
    /**
     * Visits a Log Element
     *
     * @param l a <code>Log</code> element
     * @return a <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Log l) {
        getWriterSupport().writeElement(l);
        return true;
    }
    
    /**
     * Visits a Alert Element
     *
     * @param a a <code>Alert</code> element
     * @return a <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Alert a) {
        getWriterSupport().writeElement(a);
        return true;
    }

    /**
     * Visits a Choose Element
     *
     * @param f a <code>Choose</code> element
     * @return a <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Choose f) {
        getWriterSupport().writeElement(f);
        return true;
    }

    /**
     * Visits a When Element
     *
     * @param f a <code>When</code> element
     * @return a <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(When f) {
        getWriterSupport().writeElement(f);
        return true;
    }

    /**
     * Visits a Default Element
     *
     * @param f a <code>Default</code> element
     * @return a <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Default f) {
        getWriterSupport().writeElement(f);
        return true;
    }
    
    /**
     * @see BPELVisitor#visit(ExtensibilityElement)
     */
    public boolean visit(ExtensibilityElement ext) {
        getWriterSupport().writeElement(ext);
        return true;
    }
    
    /**
     * @see DocumentationVisitor#visit(Documentation)
     */
    public boolean visit(Documentation doc) {
        try {
                getWriterSupport().writeElement(doc);
                if (doc.getValue() != null) {
	                char[] chs = doc.getValue().toCharArray();
	                if (doc.isCDATAForm()) {
	                    getWriterSupport().getXmlWriter().cdataSection(chs, 0, chs.length);
	                } else {
	                    getWriterSupport().getXmlWriter().characters(chs, 0, chs.length);
	                }
                }
                
                getWriterSupport().writeElement(doc);
            
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException("Cannot serialize documentation element", trw);
        }
        return true;
    }
    
    
    /**
     * @see BPELVisitor#visit(Condition)
     */
    public boolean visit(Condition condition) {
    	try {
    	 	getWriterSupport().writeElement(condition);
            if (getWriterSupport().isCurrentElement(condition) && condition.getValue() != null) {
                char[] chs = condition.getValue().toCharArray();
                if (condition.isCDATAForm()) {
                    getWriterSupport().getXmlWriter().cdataSection(chs, 0, chs.length);
                } else {
                    getWriterSupport().getXmlWriter().characters(chs, 0, chs.length);
                }
            }
            
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException("Cannot serialize condition element", trw);
        }
        return true;
    
    }
    
    /**
     * @see BPELVisitor#visit(Literal)
     */
    public boolean visit(Literal literal) {
    	try {
    	 	getWriterSupport().writeElement(literal);
            String value = "";
            if (getWriterSupport().isCurrentElement(literal)) {
                if(!Utility.isEmpty(literal.getValue())) {
                    value = literal.getValue();
                } else {
                    value = createXmlString(literal.getEII());
                }
            }
            char[] chs = value.toCharArray();
            if (literal.isCDATAForm()) {
                getWriterSupport().getXmlWriter().cdataSection(chs, 0, chs.length);
            } else {
                getWriterSupport().getXmlWriter().characters(chs, 0, chs.length);
            }
            //also write end tag
            getWriterSupport().writeElement(literal);
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException("Cannot serialize literal element", trw);
        }
        return true;
    
    }
    
    
    /**
     * @see BPELVisitor#visit(For)
     */
    public boolean visit(For f) {
    	try {
    	 	getWriterSupport().writeElement(f);
            if (getWriterSupport().isCurrentElement(f) && f.getValue() != null) {
                char[] chs = f.getValue().toCharArray();
                if (f.isCDATAForm()) {
                    getWriterSupport().getXmlWriter().cdataSection(chs, 0, chs.length);
                } else {
                    getWriterSupport().getXmlWriter().characters(chs, 0, chs.length);
                }
            }
            
            //also write end tag see accept method in ForImpl
            getWriterSupport().writeElement(f);
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException("Cannot serialize for element", trw);
        }
        return true;
    }
    
    /**
     * @see BPELVisitor#visit(Until)
     */
    public boolean visit(Until u) {
    	try {
    	 	getWriterSupport().writeElement(u);
            if (getWriterSupport().isCurrentElement(u) && u.getValue() != null) {
                char[] chs = u.getValue().toCharArray();
                if (u.isCDATAForm()) {
                    getWriterSupport().getXmlWriter().cdataSection(chs, 0, chs.length);
                } else {
                    getWriterSupport().getXmlWriter().characters(chs, 0, chs.length);
                }
            }
            
            //also write end tag see accept method in UntilImpl
            getWriterSupport().writeElement(u);
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException("Cannot serialize until element", trw);
        }
        return true;
    }
    
    
    public boolean visit(FromPart d) {
    	getWriterSupport().writeElement(d);
        return true;
	}

    public boolean visit(RepeatUntil d) {
        getWriterSupport().writeElement(d);
        return true;
    }
    
    public boolean visit(If d) {
        getWriterSupport().writeElement(d);
        return true;
    }

	public boolean visit(Rethrow d) {
		getWriterSupport().writeElement(d);
        return true;
	}

	public boolean visit(ToPart d) {
		getWriterSupport().writeElement(d);
        return true;
	}

	public boolean visit(Validate d) {
		getWriterSupport().writeElement(d);
        return true;
	}
	
	public boolean visit(Branches d) {
		getWriterSupport().writeElement(d);
        return true;
	}

	public boolean visit(CompletionCondition d) {
		getWriterSupport().writeElement(d);
        return true;
	}

	public boolean visit(FinalCounterValue d) {
		getWriterSupport().writeElement(d);
        return true;
	}

	public boolean visit(com.sun.bpel.model.ForEach f) {
		getWriterSupport().writeElement(f);
        return true;
	}

	public boolean visit(Iterator d) {
		getWriterSupport().writeElement(d);
        return true;
	}

	public boolean visit(StartCounterValue d) {
		getWriterSupport().writeElement(d);
        return true;
	}


    public boolean visit(ElseIf d) {
        getWriterSupport().writeElement(d);
        return true;
    }

    public boolean visit(Else d) {
        getWriterSupport().writeElement(d);
        return true;
    }

    public boolean visit(EventHandlersOnEvent d) {
        // TODO Auto-generated method stub
        getWriterSupport().writeElement(d);
        return true;
    }
    
    public boolean visit(EventHandlersOnAlarm d) {
        // TODO Auto-generated method stub
        getWriterSupport().writeElement(d);
        return true;
    }   
    
    public boolean visit(EventHandlers d) {        
        // TODO Auto-generated method stub
        getWriterSupport().writeElement(d);
        return true;
    }    
    
    public boolean visit(RepeatEvery d) {
        try {
            getWriterSupport().writeElement(d);
            if (getWriterSupport().isCurrentElement(d) && d.getValue() != null) {
                char[] chars = d.getValue().toCharArray();
                if (d.isCDATAForm()) {
                    getWriterSupport().getXmlWriter().cdataSection(chars, 0, chars.length);
                } else {
                    getWriterSupport().getXmlWriter().characters(chars, 0, chars.length);
                }
            }
            // write out the end tag for the </RepeatEvery> element
            getWriterSupport().writeElement(d);
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException("Cannot serialize for element", trw);
        }
        return true;
    }

    /**
     * Convert the Element to a string representing XML  
     * @param node The DOM Node
     * @return The string representing XML
     */
    private static String createXmlString(Node node) {
        DOMSource source = new DOMSource(node);
        try {
            StringWriter writer = new StringWriter();
            StreamResult result = new StreamResult(writer);
            Transformer transformer = TransformerFactory.newInstance().newTransformer();

            try {
                transformer.transform(source, result);
            } catch (TransformerException ex) {
                throw ex;
            } finally {
                transformer = null;
            }

            String xmlString = writer.toString();
            return xmlString;

        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public boolean visit(ExtensionAssignOperation extensionAssignOperation) {
        // TODO Auto-generated method stub
        return false;
    }

    public boolean visit(SunExtExpression expression) {
        // TODO Auto-generated method stub
        return false;
    }  
}
