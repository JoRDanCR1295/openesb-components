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
 * @(#)CloneVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.visitor;

import com.sun.bpel.model.Assign;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Case;
import com.sun.bpel.model.Catch;
import com.sun.bpel.model.CatchAll;
import com.sun.bpel.model.Compensate;
import com.sun.bpel.model.CompensationHandler;
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
import com.sun.bpel.model.Flow;
import com.sun.bpel.model.From;
import com.sun.bpel.model.FromPart;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.Link;
import com.sun.bpel.model.Links;
import com.sun.bpel.model.OnAlarm;
import com.sun.bpel.model.OnMessage;
import com.sun.bpel.model.Otherwise;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.PartnerLinks;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.RepeatEvery;
import com.sun.bpel.model.Reply;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.Source;
import com.sun.bpel.model.SunExtExpression;
import com.sun.bpel.model.Switch;
import com.sun.bpel.model.Target;
import com.sun.bpel.model.Terminate;
import com.sun.bpel.model.TerminationHandler;
import com.sun.bpel.model.Throw;
import com.sun.bpel.model.To;
import com.sun.bpel.model.ToPart;
import com.sun.bpel.model.Validate;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.Wait;
import com.sun.bpel.model.While;
import com.sun.bpel.model.common.visitor.CloneSupport;
import com.sun.bpel.model.common.visitor.XMLCloneVisitorException;
import com.sun.bpel.model.common.visitor.CloneSupport.Instantiable;
import com.sun.bpel.model.extensions.Alert;
import com.sun.bpel.model.extensions.Choose;
import com.sun.bpel.model.extensions.Default;
import com.sun.bpel.model.extensions.ExtensibilityElement;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.model.extensions.Log;
import com.sun.bpel.model.extensions.Trace;
import com.sun.bpel.model.extensions.When;
import com.sun.bpel.model.impl.BPELDocumentImpl;
import com.sun.bpel.xml.common.model.Documentation;
import com.sun.bpel.xml.common.model.XMLComment;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.model.XMLProcessingInstruction;
import com.sun.bpel.xml.common.model.XMLText;
import com.sun.bpel.xml.common.visitor.DocumentationVisitor;
import com.sun.bpel.xml.common.visitor.ParentChildrenParentVisitor;


/**
 * Visits all the model nodes and clones them.
 *
 * @author Sun Microsystems
 * @version 
 */
public class CloneVisitor extends AbstractVisitor implements ParentChildrenParentVisitor {
    
    /** Creates a new instance of CloneVisitor */
    public CloneVisitor() {
    }
    
    /** Gets the clone visitor support.
     * @return  Visitor support.
     */
    public CloneSupport getCloneSupport() {
        if (null == getVisitorSupport()) {
            setVisitorSupport(new CloneSupport());
        }
        return (CloneSupport) getVisitorSupport();
    }
    
    /** Prepares the visitor for use.
     * @param   v   Values to use.
     *              <ol start="0">
     *              <li><code>com.sun.bpel.model.BPELDocument</code> Destination document.</li>
     *              </ol>
     */
    public void prepare(Object[] v) {
        getCloneSupport().setInstantiatingDocument((BPELDocument) v[0]);
        getCloneSupport().setResults((XMLNode[]) v[1]);
    }
    
    /** Visits a BPEL document.
     * @param   d   a BPEL document.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(final BPELDocument d) {
        try {
            getCloneSupport().cloneElement(d, new Instantiable() {
                /** @see CloneSupport.Instantiable#create
                 */
                public XMLElement create() {
                    return new BPELDocumentImpl();
                }
                
                /** @see CloneSupport.Instantiable#postCloneRun
                 */
                public void postCloneRun(XMLElement clone) {
                }
            });
        } catch (Throwable trw) {
            throw new XMLCloneVisitorException(
                "CloneVisitor.visit(BPELDocument)", trw);
        }
        return true;
    }
    
    /** Visits a comment section.
     * @param   c   XML comment section
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(XMLComment c) {
        try {
            XMLComment clone = getCloneSupport().getInstantiatingDocument().createXmlComment();
            clone.setValue(c.getValue());
            if (getCloneSupport().peekCurrentClone() != null) {
                getCloneSupport().peekCurrentClone().addChild(clone);
            }
            getCloneSupport().setCloned(clone);
        } catch (Throwable trw) {
            throw new XMLCloneVisitorException(
                "CloneVisitor.visit(XMLComment)", trw);
        }
        return true;
    }
    
    /** Visits a XML processing instruction section.
     * @param   p   XML processing instruction.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(XMLProcessingInstruction p) {
        try {
            XMLProcessingInstruction clone = getCloneSupport().getInstantiatingDocument()
                .createProcessingInstruction();
            clone.setTarget(p.getTarget());
            clone.setData(p.getData());
            if (getCloneSupport().peekCurrentClone() != null) {
                getCloneSupport().peekCurrentClone().addChild(clone);
            }
            getCloneSupport().setCloned(clone);
        } catch (Throwable trw) {
            throw new XMLCloneVisitorException(
                "CloneVisitor.visit(XMLProcessingInstruction)", trw);
        }
        return true;
    }
    
    /** Visits a text section.
     * @param   t   XML text section
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(XMLText t) {
        try {
            XMLText clone = ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createXmlText();
            clone.setValue(t.getValue());
            if (getCloneSupport().peekCurrentClone() != null) {
                getCloneSupport().peekCurrentClone().addChild(clone);
            }
            getCloneSupport().setCloned(clone);
        } catch (Throwable trw) {
            throw new XMLCloneVisitorException(
                "CloneVisitor.visit(XMLComment)", trw);
        }
        return true;
    }
    
    /** Visits a process element.
     * @param   p a process element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(BPELProcess p) {
        getCloneSupport().cloneElement(p, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createProcess();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a partners element.
     * @param   p   a partners element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(PartnerLinks p) {
        getCloneSupport().cloneElement(p, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createPartnerLinks();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a partner element.
     * @param   p   a partner element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(PartnerLink p) {
        getCloneSupport().cloneEmptyElement(p, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createPartnerLink();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a containers element.
     * @param   c   a containers element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Variables c) {
        getCloneSupport().cloneElement(c, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createVariables();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
  
    /** Visits a container element.
     * @param   c   a container element.
     * @return  <code>true</code> if traversal is to continue.
     */
    public boolean visit(Variable c) {
        getCloneSupport().cloneElement(c, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createVariable();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a correlationSets element.
     * @param   c   a correlationSets element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CorrelationSets c) {
        getCloneSupport().cloneElement(c, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createCorrelationSets();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a correlationSet element.
     * @param   c   a correlationSet element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CorrelationSet c) {
        getCloneSupport().cloneEmptyElement(c, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createCorrelationSet();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a faultHandlers element.
     * @param   f   a faultHandlers element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(FaultHandlers f) {
        getCloneSupport().cloneElement(f, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createFaultHandlers();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a catch element.
     * @param   c   a catch element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Catch c) {
        getCloneSupport().cloneElement(c, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createCatch();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a catchAll element.
     * @param   c   a catchAll element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CatchAll c) {
        getCloneSupport().cloneElement(c, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createCatchAll();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a compensationHandler element.
     * @param   c   a compensationHandler element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CompensationHandler c) {
        getCloneSupport().cloneElement(c, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createCompensationHandler();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }

    /** Visits a terminationHandler element.
     * @param   t   a terminationHandler element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(TerminationHandler t) {
        getCloneSupport().cloneElement(t, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createTerminationHandler();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a receive element.
     * @param   r   a receive element.
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(final Receive r) {
        getCloneSupport().cloneElement(r, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createReceive();
            }
            
            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
                //((Receive) clone).setBusinessProcessCallable(r.getBusinessProcessCallable());
            }
        });
        return true;
    }
    
    /** Visits a target element.
     * @param   t   a target element.
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Target t) {
        getCloneSupport().cloneEmptyElement(t, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createTarget();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a source element.
     * @param   s   a source element.
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Source s) {
        getCloneSupport().cloneEmptyElement(s, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createSource();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a correlations element.
     * @param   c   a correlations element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Correlations c) {
        getCloneSupport().cloneElement(c, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createCorrelations();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a correlation element.
     * @param   c   a correlation element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Correlation c) {
        getCloneSupport().cloneEmptyElement(c, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createCorrelation();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }

     /** Visits a ForEach element.
     * @param   fe  a ForEach element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(ForEach fe) {
        getCloneSupport().cloneElement(fe, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createForEachSBYN();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a Trace element.
     * @param   t  a Trace element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Trace t) {
        getCloneSupport().cloneElement(t, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createTraceSunExt();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a Log element.
     * @param   l  a Log element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Log l) {
        getCloneSupport().cloneElement(l, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createLogSunExt();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits an Alert element.
     * @param   a  an Alert element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Alert a) {
        getCloneSupport().cloneElement(a, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createAlertSunExt();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a ForEach element.
     * @param   fe  a ForEach element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(com.sun.bpel.model.ForEach fe) {
        getCloneSupport().cloneElement(fe, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createForEach();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }    

     /** Visits a FromPart element.
     * @param   fe  a FromPart element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(FromPart fe) {
        getCloneSupport().cloneElement(fe, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createFromPart();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
     /** Visits a FromPart element.
     * @param   fe  a FromPart element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(ToPart fe) {
        getCloneSupport().cloneElement(fe, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createToPart();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    /** Visits a Choose element.
     * @param   c  a Choose element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Choose c) {
        getCloneSupport().cloneElement(c, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createChoose();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }

    /** Visits a Default element.
     * @param  d  a Default element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Default d) {
        getCloneSupport().cloneElement(d, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createDefault();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }

    /** Visits a When element.
     * @param   w  a When element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(When w) {
        getCloneSupport().cloneElement(w, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createWhen();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a sequence element.
     * @param   s   a sequence element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Sequence s) {
        getCloneSupport().cloneElement(s, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createSequence();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a flow element.
     * @param   f   a flow element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Flow f) {
        getCloneSupport().cloneElement(f, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createFlow();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a links element.
     * @param   l   a links element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Links l) {
        getCloneSupport().cloneElement(l, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createLinks();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
     
    /** Visits a link element.
     * @param   l   a link element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Link l) {
        getCloneSupport().cloneEmptyElement(l, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createLink();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
   
    /** Visits a switch element.
     * @param   s   a switch element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Switch s) {
        getCloneSupport().cloneElement(s, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createSwitch();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a case element.
     * @param   c   a case element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Case c) {
        getCloneSupport().cloneElement(c, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createCase();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a otherwise element.
     * @param   o   a otherwise element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Otherwise o) {
        getCloneSupport().cloneElement(o, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createOtherwise();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a assign element.
     * @param   a   a assign element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Assign a) {
        getCloneSupport().cloneElement(a, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createAssign();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a copy element.
     * @param   c   a copy element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Copy c) {
        getCloneSupport().cloneElement(c, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createCopy();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a from element.
     * @param   f   a from element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(From f) {
        if (f.hasChildren()) {
            getCloneSupport().cloneElement(f, new Instantiable() {
                /** @see CloneSupport.Instantiable#create
                 */
                public XMLElement create() {
                    return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createFrom();
                }

                /** @see CloneSupport.Instantiable#postCloneRun
                 */
                public void postCloneRun(XMLElement clone) {
                }
            });
        } else {
            getCloneSupport().cloneEmptyElement(f, new Instantiable() {
                /** @see CloneSupport.Instantiable#create
                 */
                public XMLElement create() {
                    return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createFrom();
                }

                /** @see CloneSupport.Instantiable#postCloneRun
                 */
                public void postCloneRun(XMLElement clone) {
                }
            });
        }
        return true;
    }
    
    /** Visits a to element.
     * @param   t   a to element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(To t) {
        getCloneSupport().cloneEmptyElement(t, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createTo();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a reply element.
     * @param   r   a reply element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(final Reply r) {
        getCloneSupport().cloneElement(r, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createReply();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
                //((Reply) clone).setBusinessProcessCallable(r.getBusinessProcessCallable());
            }
        });
        return true;
    }
    
    /** Visits a invoke element.
     * @param   i   a invoke element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(final Invoke i) {
        getCloneSupport().cloneElement(i, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createInvoke();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
                //((Invoke) clone).setBusinessProcessCallable(i.getBusinessProcessCallable());
            }
        });
        return true;
    }
    
    /** Visits a throw element.
     * @param   t   a throw element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Throw t) {
        getCloneSupport().cloneElement(t, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createThrow();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a terminate element.
     * @param   t   a terminate element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Terminate t) {
        getCloneSupport().cloneElement(t, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createTerminate();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a wait element.
     * @param   w   a terminate wait.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Wait w) {
        getCloneSupport().cloneElement(w, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createWait();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a empty element.
     * @param   e   a empty element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Empty e) {
        getCloneSupport().cloneElement(e, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createEmpty();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a while element.
     * @param   w   a while element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(While w) {
        getCloneSupport().cloneElement(w, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createWhile();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    
    /** Visits a while element.
     * @param   e   a while element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Else e) {
        getCloneSupport().cloneElement(e, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createElse();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a while element.
     * @param   eif   a while element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(ElseIf eif) {
        getCloneSupport().cloneElement(eif, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createElseIf();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a pick element.
     * @param   p   a pick element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Pick p) {
        getCloneSupport().cloneElement(p, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createPick();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a onMessage element.
     * @param   o   a onMessage element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(final OnMessage o) {
        getCloneSupport().cloneElement(o, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createOnMessage();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
                //((OnMessage) clone).setBusinessProcessCallable(o.getBusinessProcessCallable());
            }
        });
        return true;
    }
    
    /** Visits a onAlarm element.
     * @param   o   a onAlarm element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(OnAlarm o) {
        getCloneSupport().cloneElement(o, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createOnAlarm();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a scope element.
     * @param   s   a scope element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Scope s) {
        getCloneSupport().cloneElement(s, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createScope();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }

    /** Visits a compensate element.
     * @param   c   a compensate element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Compensate c) {
        getCloneSupport().cloneElement(c, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createCompensate();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see BPELVisitor#visit(ExtensibilityElement)
     */
    public boolean visit(ExtensibilityElement ext) {
        getCloneSupport().cloneElement(ext, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createExtensibilityElement();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see DocumentationVisitor#visit(Documentation)
     */
    public boolean visit(Documentation doc) {
        try {
            if (doc.getValue() != null) {
                Documentation clone = ((BPELDocument) getCloneSupport().getInstantiatingDocument())
                    .createBPELDocumentation();
                clone.setValue(doc.getValue());
                if (getCloneSupport().peekCurrentClone() != null) {
                    getCloneSupport().peekCurrentClone().addChild(clone);
                }
                getCloneSupport().setCloned(clone);
            }
        } catch (Throwable trw) {
            throw new XMLCloneVisitorException(
                "CloneVisitor.visit(Documentation)", trw);
        }
        return true;
    }
    
    /** Visits an EventHandlers element.
     * @param   eh  an EventHandlers element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(EventHandlers eh) {
        getCloneSupport().cloneElement(eh, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createEventHandlers();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }    
    
    /** Visits an EventHandlersOnEvent element.
     * @param   oe  an EventHandlersOnEvent element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(EventHandlersOnEvent oe) {
        getCloneSupport().cloneElement(oe, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createEventHandlersOnEvent();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }    
    
    /** Visits an EventHandlersOnAlarm element.
     * @param   ol  an EventHandlersOnAlarm element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(EventHandlersOnAlarm ol) {
        getCloneSupport().cloneElement(ol, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createEventHandlersOnAlarm();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }    
    
    
    /** Visits a repeatEvery element.
     * @param   re  a repeatEvery element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(RepeatEvery re) {
        getCloneSupport().cloneElement(re, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createRepeatEvery();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }

    /** Visits a validate element.
     * @param   v validate element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Validate v) {
        getCloneSupport().cloneElement(v, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((BPELDocument) getCloneSupport().getInstantiatingDocument()).createValidate();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
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
