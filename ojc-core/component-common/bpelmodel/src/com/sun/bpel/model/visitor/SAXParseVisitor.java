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
 * @(#)SAXParseVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.visitor;

import java.io.Reader;
import java.util.logging.Logger;

import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;

import com.sun.bpel.model.Assign;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELParseContext;
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
import com.sun.bpel.model.common.visitor.SAXParserSupport;
import com.sun.bpel.model.common.visitor.XMLParseVisitorException;
import com.sun.bpel.model.common.visitor.XmlParser;
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
import com.sun.bpel.xml.common.visitor.AutonomousVisitor;
import com.sun.bpel.xml.common.visitor.DocumentationVisitor;



/**
 * Uses the tokens provided by a SAX parser to visit and set the appropriate
 * nodes of a BPEL4WS model.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SAXParseVisitor extends AbstractVisitor
    implements AutonomousVisitor {
    
    /** The logger. */
    private static final Logger mLogger = Logger.getLogger(SAXParseVisitor.class.getName());
    
    /** Holds Reader for input */
    private Reader reader = null;
    
    
    private BPELDocument mDocument = null;
    
    private DefaultXmlParserFactory mParserFactory;
    
    /** Holds XML parser for BPEL document. */
    private XmlParser bpelDocumentXmlParser = null;
    
    /** Holds XML parser for process element. */
    private XmlParser processXmlParser = null;
    
    /** Holds XML parser for partners element. */
    private XmlParser partnersXmlParser = null;
    
    /** Holds XML parser for containers element. */
    private XmlParser containersXmlParser = null;
    
    /** Holds XML parser for container element. */
    private XmlParser containerXmlParser = null;
    
    /** Holds XML parser for correlationSets element. */
    private XmlParser correlationSetsXmlParser = null;
    
    /** Holds XML parser for faultHandlers element. */
    private XmlParser faultHandlersXmlParser = null;
    
    /** Holds XML parser for catch element. */
    private XmlParser catchXmlParser = null;
    
    /** Holds XML parser for catchAll element. */
    private XmlParser catchAllXmlParser = null;
    
    /** Holds XML parser for correlations element. */
    private XmlParser correlationsXmlParser = null;
    
    /** Holds XML parser for receive element. */
    private XmlParser receiveXmlParser = null;
    
    /** Holds XML parser for sequence element. */
    private XmlParser sequenceXmlParser = null;
    
    /** Holds XML parser for flow element. */
    private XmlParser flowXmlParser = null;
    
    /** Holds XML parser for switch element. */
    private XmlParser switchXmlParser = null;
    
    /** Holds XML parser for assign element. */
    private XmlParser assignXmlParser = null;
    
    /** Holds XML parser for reply element. */
    private XmlParser replyXmlParser = null;
    
    /** Holds XML parser for links element. */
    private XmlParser linksXmlParser = null;
    
    /** Holds XML parser for case element. */
    private XmlParser caseXmlParser = null;
    
    /** Holds XML parser for otherwise element. */
    private XmlParser otherwiseXmlParser = null;
    
    /** Holds XML parser for copy element. */
    private XmlParser copyXmlParser = null;
    
    /** Holds XML parser for from element. */
    private XmlParser fromXmlParser = null;
    
    /** Holds XML parser for to element. */
    private XmlParser toXmlParser = null;
    
	
    /** Holds XML parser for compensationHandler element. */
    private XmlParser compensationHandlerXmlParser = null;
    
    /** Holds XML parser for terminationHandler element. */
    private XmlParser terminationHandlerXmlParser = null;
    
    /** Holds XML parser for invoke element. */
    private XmlParser invokeXmlParser = null;
    
    /** Holds XML parser for throw element. */
    private XmlParser throwXmlParser = null;
    
    /** Holds XML parser for terminate element. */
    private XmlParser terminateXmlParser = null;
    
    /** Holds XML parser for wait element. */
    private XmlParser waitXmlParser = null;
    
    /** Holds XML parser for empty element. */
    private XmlParser emptyXmlParser = null;
    
    /** Holds XML parser for while element. */
    private XmlParser whileXmlParser = null;
    
    /** Holds XML parser for pick element. */
    private XmlParser pickXmlParser = null;
    
    /** Holds XML parser for onMessage element. */
    private XmlParser onMessageXmlParser = null;
    
    /** Holds XML parser for onAlarm element. */
    private XmlParser onAlarmXmlParser = null;
    
    /** Holds XML parser for scope element. */
    private XmlParser scopeXmlParser = null;
    
    /** Holds XML parser for compensate element. */
    private XmlParser compensateXmlParser = null;
    
    /** Holds XML parser for compensate element. */
    private XmlParser compensateScopeXmlParser = null;

    /** Holds XML parser for forEach element. */
    private XmlParser forEachXmlParser = null;
    private XmlParser fromPartXmlParser = null;    

    private XmlParser toPartXmlParser = null;  
    /** Holds XML parser for choose element. */
    private XmlParser chooseXmlParser = null;

    /** Holds XML parser for when element. */
    private XmlParser whenXmlParser = null;

    /** Holds XML parser for default element. */
    private XmlParser defaultXmlParser = null;

    /** Holds the XML parser for an extensibility element. */
    private XmlParser mExtensibilityElementXmlParser = null;

    /** Holds the XML parser for the documentation element. */
    private XmlParser mDocumentationXmlParser = null;
    
    /** Holds XML parser for Condition element. */
    private XmlParser conditionXmlParser = null;
    
    /** Holds XML parser for Literal element. */
    private XmlParser literalXmlParser = null;

    /** Holds XML parser for extensionAssignOperation.Expression. */
    private XmlParser sunExtExpressionXmlParser = null;

    /** Holds XML parser for for element. */
    private XmlParser forXmlParser = null;
    
    /** Holds XML parser until for element. */
    private XmlParser untilXmlParser = null;
    
    /** Holds XML parser Rethrow for element. */
    private XmlParser rethrowXmlParser = null;
    
    
    /** Holds XML parser RepeatUntil for element. */
    private XmlParser repeatUntilXmlParser = null;
    
        
    /** Holds XML parser If for element. */
    private XmlParser ifXmlParser = null;

    /** Holds XML parser If for element. */
    private XmlParser thenXmlParser = null;
    
    /** Holds XML parser If for element. */
    private XmlParser elseXmlParser = null;
    
    /** Holds XML parser If for element. */
    private XmlParser elseIfXmlParser = null;
    
    /** Holds XML parser Validate for element. */
    private XmlParser validateXmlParser = null;
    
    /**Holds XML parser ForEach for element. */
    private XmlParser forEachParser = null;
    
    /**Holds XML parser startCounterValue for element. */
    private XmlParser startCounterValueParser = null;
    
    /**Holds XML parser finalCounterValue for element. */
    private XmlParser finalCounterValueParser = null;
    
    /**Holds XML parser completCondition for element. */
    private XmlParser completConditionParser = null;
    
    /**Holds XML parser branches for element. */
    private XmlParser branchesParser = null;
    
    /**Holds XML parser EventHandlers for element. */
    private XmlParser eventHandlersParser = null;

    
    /**Holds XML parser EventHandlersOnEvent for element. */
    private XmlParser eventHandlersOnEventParser = null;

    
    /**Holds XML parser EventHandlersOnAlarm for element. */
    private XmlParser eventHandlersOnAlarmParser = null;

    
    /**Holds XML parser RepeatEvery for element. */
    private XmlParser repeatEveryParser = null;
    
    /**Holds XML parser Trace for element. */
    private XmlParser traceSunExtParser = null;
    
    /**Holds XML parser Log for element. */
    private XmlParser logSunExtParser = null;
    
    /**Holds XML parser Alert for element. */
    private XmlParser alertSunExtParser = null;
    
    
    private XmlParser extensionAssignOperationParser = null;
    
    /** Creates a new instance of SAXParseVisitor. */
    public SAXParseVisitor() {
        super();
    }
    
    /** Getter for property reader.
     * @return Value of property reader.
     *
     */
    public Reader getReader() {
        return reader;
    }
    
    /** Setter for property reader.
     * @param reader New value of property reader.
     *
     */
    public void setReader(Reader reader) {
        this.reader = reader;
    }
    
    /** Getter for the outermost XML document.
     * @return  XML document.
     */
    public BPELDocument getXmlDocument() {
        return this.mDocument;
    }

    public void setXmlDocument(BPELDocument document) {
		this.mDocument = document;
	}
    
    /** Getter for the XML parser support.
     * @return  XML parser support.
     */
    public SAXParserSupport getParserSupport() {
        if (null == getVisitorSupport()) {
            setVisitorSupport(new SAXParserSupport());
        }
        return (SAXParserSupport) getVisitorSupport();
    }
    
    /** Prepares the visitor for use.
     * @param   v   Values to use.
     *              <ol start="0">
     *              <li><code>java.io.Reader</code> implementing object.</li>
     *              </ol>
     */
    public void prepare(Object[] v) {
        setReader((Reader) v[0]);
    }
    
    
    /** Visits a BPEL document.
     * @param   d   a BPEL document.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(BPELDocument d) {
        try {
        	this.setXmlDocument(d);
        	
        	SAXParseVisitorService vService = (SAXParseVisitorService) this.getVisitorService();
        	BPELParseContext context = vService.getBPELParseContext();
        	if(context == null) {
        		throw new IllegalArgumentException("BPELParseContext should not be null");
        	}
			ErrorHandler errorHandler = context.getErrorHandler();
			DefaultXmlParserFactory parseFactory = context.getXMLParserFactory();
			if(!parseFactory.equals(mParserFactory)) {
				cleanXmlParsers();
				parseFactory.setVisitor(this);
				mParserFactory = parseFactory;
			}
			
			boolean validate = errorHandler != null ? true : false;
			XMLReader reader = 
            	getParserSupport().createXmlReader(validate);
            reader.setErrorHandler(errorHandler);
            
            if (null == bpelDocumentXmlParser) {
                bpelDocumentXmlParser = this.mParserFactory.createBPELDocumentXmlParser();
            }
            getParserSupport().pushXmlParser(bpelDocumentXmlParser, d);
            
            InputSource in = new InputSource(getReader());
            in.setSystemId(d.getBaseURI());
            reader.parse(in);
            
            getParserSupport().popXmlParser();
            this.setXmlDocument(null);
            
        } catch (Throwable trw) {
            throw new XMLParseVisitorException(
                "Cannot parse BPEL", trw);
        }
        return false;
    }
    
        
    /** Visits a XML processing instruction section.
     * @param   p   XML processing instruction.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(XMLProcessingInstruction p) {
        getXmlDocument().addProcessingInstruction(p);
        return false;
    }
    
    /** Visits a comment section.
     * @param   c   XML comment section
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(XMLComment c) {
        return false;
    }
    
    /** Visits a process element.
     * @param   p   a process element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(BPELProcess p) {
        if (null == processXmlParser) {
            processXmlParser = this.mParserFactory.createProcessXmlParser();
        }
         getParserSupport().pushXmlParser(processXmlParser, p);
        return false;
    }
        
    /** Visits a partners element.
     * @param   p   a partners element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(PartnerLinks p) {
        if (null == partnersXmlParser) {
            partnersXmlParser = this.mParserFactory.createPartnerLinksXmlParser();
        }
         getParserSupport().pushXmlParser(partnersXmlParser, p);
        return false;
    }
    
    /** Visits a partner element.
     * @param   p   a partner element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(PartnerLink p) {
        return false;
    }
    
    
    /** Visits a containers element.
     * @param   c   a containers element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Variables c) {
        if (null == containersXmlParser) {
            containersXmlParser = this.mParserFactory.createVariablesXmlParser();
        }
         getParserSupport().pushXmlParser(containersXmlParser,
                                                         c);
        return false;
    }
    
    /** Visits a container element.
     * @param   c   a container element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Variable c) {
        if (null == containerXmlParser) {
            containerXmlParser = this.mParserFactory.createVariableXmlParser();
        }
         getParserSupport().pushXmlParser(containerXmlParser, c);
        return false;
    }
    
    
    /** Visits a correlationSets element.
     * @param   c   a correlationSets element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CorrelationSets c) {
        if (null == correlationSetsXmlParser) {
            correlationSetsXmlParser = this.mParserFactory.createCorrelationSetsXmlParser();
        }
         getParserSupport()
            .pushXmlParser(correlationSetsXmlParser, c);
        return false;
    }
    
    /** Visits a correlationSet element.
     * @param   c   a correlationSet element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CorrelationSet c) {
        return false;
    }
    
       
    /** Visits a faultHandlers element.
     * @param   f   a faultHandlers element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(FaultHandlers f) {
        if (null == faultHandlersXmlParser) {
            faultHandlersXmlParser = this.mParserFactory.createFaultHandlersXmlParser();
        }
         getParserSupport().pushXmlParser(faultHandlersXmlParser,
                                                         f);
        return false;
    }
    
    
    /** Visits a catch element.
     * @param   c   a catch element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Catch c) {
        if (null == catchXmlParser) {
            catchXmlParser = this.mParserFactory.createCatchXmlParser();
        }
         getParserSupport().pushXmlParser(catchXmlParser, c);
        return false;
    }
    
    
    /** Visits a catchAll element.
     * @param   c   a catchAll element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CatchAll c) {
        if (null == catchAllXmlParser) {
            catchAllXmlParser = this.mParserFactory.createCatchAllXmlParser();
        }
         getParserSupport().pushXmlParser(catchAllXmlParser, c);
        return false;
    }
    
    
    /** Visits a compensationHandler element.
     * @param   c   a compensationHandler element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CompensationHandler c) {
        if (null == compensationHandlerXmlParser) {
            compensationHandlerXmlParser = this.mParserFactory.createCompensationHandlerXmlParser();
        }
         getParserSupport()
            .pushXmlParser(compensationHandlerXmlParser, c);
        return false;
    }
    
    
    /** Visits a terminationHandler element.
     * @param   t   a terminationHandler element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(TerminationHandler t) {
        if (null == terminationHandlerXmlParser) {
            terminationHandlerXmlParser = this.mParserFactory.createTerminationHandlerXmlParser();
        }
         getParserSupport()
            .pushXmlParser(terminationHandlerXmlParser, t);
        return false;
    }
    
    
    /** Visits a receive element.
     * @param   r   a receive element.
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Receive r) {
        //r.getBusinessProcessCallable();
        if (null == receiveXmlParser) {
            receiveXmlParser = this.mParserFactory.createReceiveXmlParser();
        }
         getParserSupport().pushXmlParser(receiveXmlParser, r);
        return false;
    }
    
    
    /** Visits a target element.
     * @param   t   a target element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Target t) {
        return false;
    }
    
    /** Visits a source element.
     * @param   s   a source element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Source s) {
        return false;
    }
    
    /** Visits a correlations element.
     * @param   c   a correlations element.
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Correlations c) {
        if (null == correlationsXmlParser) {
            correlationsXmlParser = this.mParserFactory.createCorrelationsXmlParser();
        }
         getParserSupport().pushXmlParser(correlationsXmlParser,
                                                         c);
        return false;
    }
    
    /** Visits a correlation element.
     * @param   c   a correlation element.
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Correlation c) {
        return false;
    }
    
  
    /** Visits a sequence element.
     * @param   s   a sequence element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Sequence s) {
        if (null == sequenceXmlParser) {
            sequenceXmlParser = this.mParserFactory.createSequenceXmlParser();
        }
         getParserSupport().pushXmlParser(sequenceXmlParser, s);
        return false;
    }
    
    
    /** Visits a flow element.
     * @param   f   a flow element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Flow f) {
        if (null == flowXmlParser) {
            flowXmlParser = this.mParserFactory.createFlowXmlParser();
        }
         getParserSupport().pushXmlParser(flowXmlParser, f);
        return false;
    }
    
    
    /** Visits a links element.
     * @param   l   a links element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Links l) {
        if (null == linksXmlParser) {
            linksXmlParser = this.mParserFactory.createLinksXmlParser();
        }
         getParserSupport().pushXmlParser(linksXmlParser, l);
        return false;
    }
    
     
    /** Visits a link element.
     * @param   l   a link element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Link l) {
        return false;
    }
   
    /** Visits a switch element.
     * @param   s   a switch element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Switch s) {
        if (null == switchXmlParser) {
            switchXmlParser = this.mParserFactory.createSwitchXmlParser();
        }
         getParserSupport().pushXmlParser(switchXmlParser, s);
        return false;
    }
    
   
    
    /** Visits a case element.
     * @param   c   a case element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Case c) {
        if (null == caseXmlParser) {
            caseXmlParser = this.mParserFactory.createCaseXmlParser();
        }
         getParserSupport().pushXmlParser(caseXmlParser, c);
        return false;
    }
        
    /** Visits a otherwise element.
     * @param   o   a otherwise element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Otherwise o) {
        if (null == otherwiseXmlParser) {
            otherwiseXmlParser = this.mParserFactory.createOtherwiseXmlParser();
        }
         getParserSupport().pushXmlParser(otherwiseXmlParser, o);
        return false;
    }
    
    
    
    /** Visits a assign element.
     * @param   a   a assign element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Assign a) {
        if (null == assignXmlParser) {
            assignXmlParser = this.mParserFactory.createAssignXmlParser();
        }
         getParserSupport().pushXmlParser(assignXmlParser, a);
        return false;
    }
    
    /** Visits a copy element.
     * @param   c   a copy element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Copy c) {
        if (null == copyXmlParser) {
            copyXmlParser = this.mParserFactory.createCopyXmlParser();
        }
         getParserSupport().pushXmlParser(copyXmlParser, c);
        return false;
    }
    
    
    /** Visits a from element.
     * @param   f   a from element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(From f) {
        if (null == fromXmlParser) {
            fromXmlParser = this.mParserFactory.createFromXmlParser();
        }
         getParserSupport().pushXmlParser(fromXmlParser, f);
        return false;
    }
    
    /** Visits a text section.
     * @param   t   XML text section
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(XMLText t) {
        return false;
    }
    
    /** Visits a to element.
     * @param   t   a to element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(To t) {
    	if (null == toXmlParser) {
            toXmlParser = this.mParserFactory.createToXmlParser();
        }
         getParserSupport().pushXmlParser(toXmlParser, t);
        return false;
    }
    
    
    /** Visits a reply element.
     * @param   r   a reply element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Reply r) {
        //r.getBusinessProcessCallable();
        if (null == replyXmlParser) {
            replyXmlParser = this.mParserFactory.createReplyXmlParser();
        }
         getParserSupport().pushXmlParser(replyXmlParser, r);
        return false;
    }
    
    
    /** Visits a invoke element.
     * @param   i   a invoke element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Invoke i) {
        //i.getBusinessProcessCallable();
        if (null == invokeXmlParser) {
            invokeXmlParser = this.mParserFactory.createInvokeXmlParser();
        }
         getParserSupport().pushXmlParser(invokeXmlParser, i);
        return false;
    }
    
    /** Visits a throw element.
     * @param   t   a throw element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Throw t) {
        if (null == throwXmlParser) {
            throwXmlParser = this.mParserFactory.createThrowXmlParser();
        }
         getParserSupport().pushXmlParser(throwXmlParser, t);
        return false;
    }
    
    /** Visits a terminate element.
     * @param   t   a terminate element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Terminate t) {
        if (null == terminateXmlParser) {
            terminateXmlParser = this.mParserFactory.createExitXmlParser();
        }
         getParserSupport().pushXmlParser(terminateXmlParser, t);
        return false;
    }
    
    /** Visits a wait element.
     * @param   w   a wait element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Wait w) {
        if (null == waitXmlParser) {
            waitXmlParser = this.mParserFactory.createWaitXmlParser();
        }
         getParserSupport().pushXmlParser(waitXmlParser, w);
        return false;
    }
    
    
    /** Visits a empty element.
     * @param   e   a empty element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Empty e) {
        if (null == emptyXmlParser) {
            emptyXmlParser = this.mParserFactory.createEmptyXmlParser();
        }
         getParserSupport().pushXmlParser(emptyXmlParser, e);
        return false;
    }
    
    /** Visits a while element.
     * @param   w   a while element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(While w) {
        if (null == whileXmlParser) {
            whileXmlParser = this.mParserFactory.createWhileXmlParser();
        }
         getParserSupport().pushXmlParser(whileXmlParser, w);
        return false;
    }
    
    
    /** Visits a pick element.
     * @param   p   a pick element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Pick p) {
        if (null == pickXmlParser) {
            pickXmlParser = this.mParserFactory.createPickXmlParser();
        }
         getParserSupport().pushXmlParser(pickXmlParser, p);
        return false;
    }
    
    
    /** Visits a onMessage element.
     * @param   o   a onMessage element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(OnMessage o) {
        //        o.getBusinessProcessCallable();
        if (null == onMessageXmlParser) {
            onMessageXmlParser = this.mParserFactory.createOnMessageXmlParser();
        }
         getParserSupport().pushXmlParser(onMessageXmlParser, o);
        return false;
    }
    
    
    
    /** Visits a onAlarm element.
     * @param   o   a onAlarm element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(OnAlarm o) {
        if (null == onAlarmXmlParser) {
            onAlarmXmlParser = this.mParserFactory.createOnAlarmXmlParser();
        }
         getParserSupport().pushXmlParser(onAlarmXmlParser, o);
        return false;
    }
    
    
    /** Visits a scope element.
     * @param   s   a scope element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Scope s) {
        if (null == scopeXmlParser) {
            scopeXmlParser = this.mParserFactory.createScopeXmlParser();
        }
         getParserSupport().pushXmlParser(scopeXmlParser, s);
        return false;
    }

    /** Visits a compensate element.
     * @param   c   a compensate element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Compensate c) {
        if (null == compensateXmlParser) {
            compensateXmlParser = this.mParserFactory.createCompensateXmlParser();
        }
         getParserSupport().pushXmlParser(compensateXmlParser, c);
        return false;
    }
    
    /** Visits a compensateScope element.
     * @param   c   a compensateScope element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(CompensateScope c) {
        if (null == compensateScopeXmlParser) {
        	compensateScopeXmlParser = this.mParserFactory.createCompensateScopeXmlParser();
        }
         getParserSupport().pushXmlParser(compensateScopeXmlParser, c);
        return false;
    }
    
    /** Visits a forEach element.
     * @param   f  forEach  element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(ForEach f) {
        if (null == forEachXmlParser) {
            forEachXmlParser = this.mParserFactory.createForEachSBYNXmlParser();
        }
         getParserSupport().pushXmlParser(forEachXmlParser, f);
        return false;
    }
    
    /** Visits a forEach element.
     * @param   f  forEach  element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(com.sun.bpel.model.ForEach f) {
        if (null == forEachXmlParser) {
            forEachXmlParser = this.mParserFactory.createForEachXmlParser();
        }
         getParserSupport().pushXmlParser(forEachXmlParser, f);
        return false;
    }
    /** Visits a fromPart element.
     * @param   f  fromPart  element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit( FromPart f) {
        if (null == fromPartXmlParser) {
            fromPartXmlParser = this.mParserFactory.createFromPartXMLParser();
        }
         getParserSupport().pushXmlParser(fromPartXmlParser, f);
        return false;
    }

    
    /** Visits a fromPart element.
     * @param   f  fromPart  element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit( ToPart f) {
        if (null == toPartXmlParser) {
            toPartXmlParser = this.mParserFactory.createToPartXMLParser();
        }
         getParserSupport().pushXmlParser(toPartXmlParser, f);
        return false;
    }
    
    /** Visits a startCounterValue element.
     * @param   startCounterValue startCounterValue  element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(StartCounterValue startCounterValue) {
        if (null == startCounterValueParser) {
            startCounterValueParser = this.mParserFactory.createStartCounterValueParser();
        }
         getParserSupport().pushXmlParser(startCounterValueParser, startCounterValue);
        return false;
    }    
    
    /** Visits a finalCounterValue element.
     * @param   finalCounterValue finalCounterValue  element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(FinalCounterValue finalCounterValue) {
        if (null == finalCounterValueParser) {
            finalCounterValueParser = this.mParserFactory.createFinalCounterValueParser();
        }
         getParserSupport().pushXmlParser(finalCounterValueParser, finalCounterValue);
        return false;
    }        
    
    /** Visits a branches element.
     * @param   branches branches  element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Branches branches) {
        if (null == branchesParser) {
            branchesParser = this.mParserFactory.createBranchesParser();
        }
         getParserSupport().pushXmlParser(branchesParser, branches);
        return false;
    }        
    
    /** Visits a completeCondition element.
     * @param   branches branches  element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(CompletionCondition completeCondition) {
        if (null == completConditionParser) {
            completConditionParser = this.mParserFactory.createCompleteConditionParser();
        }
         getParserSupport().pushXmlParser(completConditionParser, completeCondition);
        return false;
    }       

    /** Visits a Choose element.
     * @param  c  Choose  element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Choose c) {
        if (null == chooseXmlParser) {
            chooseXmlParser = this.mParserFactory.createChooseXmlParser();
        }
         getParserSupport().pushXmlParser(chooseXmlParser, c);
        return false;
    }

    /** Visits a When element.
     * @param   w  When  element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(When w) {
        if (null == whenXmlParser) {
            whenXmlParser = this.mParserFactory.createWhenXmlParser();
        }
         getParserSupport().pushXmlParser(whenXmlParser, w);
        return false;
    }

    /** Visits a Default element.
     * @param   d  Default  element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Default d) {
        if (null == defaultXmlParser) {
            defaultXmlParser = this.mParserFactory.createDefaultXmlParser();
        }
         getParserSupport().pushXmlParser(defaultXmlParser, d);
        return false;
    }
    
    /**
     * @see BPELVisitor#visit(ExtensibilityElement)
     */
    public boolean visit(ExtensibilityElement ext) {
        if (null == mExtensibilityElementXmlParser) {
            mExtensibilityElementXmlParser = this.mParserFactory.createExtensibilityElementXmlParser();
        }
         getParserSupport().pushXmlParser(mExtensibilityElementXmlParser, ext);
        return false;
    }
    
    /**
     * @see DocumentationVisitor#visit(Documentation)
     */
    public boolean visit(Documentation doc) {
        if (null == mDocumentationXmlParser) {
            mDocumentationXmlParser = this.mParserFactory.createDocumentationXmlParser();
        }
         getParserSupport().pushXmlParser(mDocumentationXmlParser, doc);
        return false;
    }


    /** Visits a Condition element.
     * @param   c   a Condition element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Condition c) {
        if (null == conditionXmlParser) {
            conditionXmlParser = this.mParserFactory.createConditionXmlParser();
        }
         getParserSupport().pushXmlParser(conditionXmlParser, c);
        return false;
    }
    

    
    /** Visits a Literal element.
     * @param   c   a Literal element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Literal c) {
        if (null == literalXmlParser) {
        	literalXmlParser = this.mParserFactory.createLiteralXmlParser();
        }
         getParserSupport().pushXmlParser(literalXmlParser, c);
        return false;
    }
    
       
    public boolean visit(For f) {
    	if (null == forXmlParser) {
    		forXmlParser = this.mParserFactory.createForXmlParser();
        }
         getParserSupport().pushXmlParser(forXmlParser, f);
        return false;
    }

    public boolean visit(Until u) {
    	if (null == untilXmlParser) {
    		untilXmlParser = this.mParserFactory.createUntilXmlParser();
        }
         getParserSupport().pushXmlParser(untilXmlParser, u);
        return false;
    }
    
    public boolean visit(RepeatUntil d) {
        if (null == repeatUntilXmlParser) {
            repeatUntilXmlParser = this.mParserFactory.createRepeatUntilXmlParser();
        }
        getParserSupport().pushXmlParser(repeatUntilXmlParser, d);
        return false;
    }
    
    public boolean visit(If d) {
        if (null == ifXmlParser) {
            ifXmlParser = this.mParserFactory.createIfXmlParser();
        }
        getParserSupport().pushXmlParser(ifXmlParser, d);
        return false;
    }

    
    
    
    public boolean visit(ElseIf d) {
        if (null == elseIfXmlParser) {
            elseIfXmlParser = this.mParserFactory.createElseIfXmlParser();
        }
        getParserSupport().pushXmlParser(elseIfXmlParser, d);
        return false;
    }
    
    
    public boolean visit(Else d) {
        if (null == elseXmlParser) {
            elseXmlParser = this.mParserFactory.createElseXmlParser();
        }
        getParserSupport().pushXmlParser(elseXmlParser, d);
        return false;
    }
    
    
	public boolean visit(Rethrow d) {
		if (null == rethrowXmlParser) {
    		rethrowXmlParser = this.mParserFactory.createRethrowXmlParser();
        }
         getParserSupport().pushXmlParser(rethrowXmlParser, d);
        return false;
	}
	

	public boolean visit(Validate d) {
		if (null == validateXmlParser) {
    		validateXmlParser = this.mParserFactory.createValidateXmlParser();
        }
         getParserSupport().pushXmlParser(validateXmlParser, d);
        return false;
	}
    

    private void cleanXmlParsers() {
    	bpelDocumentXmlParser = null;
        
    	processXmlParser = null;
        
        partnersXmlParser = null;
        
        containersXmlParser = null;
        
        containerXmlParser = null;
        
        correlationSetsXmlParser = null;
        
        faultHandlersXmlParser = null;
        
        catchXmlParser = null;
        
        catchAllXmlParser = null;
        
        correlationsXmlParser = null;
        
        receiveXmlParser = null;
        
        sequenceXmlParser = null;
        
        flowXmlParser = null;
        
        switchXmlParser = null;
        
        assignXmlParser = null;
        
        replyXmlParser = null;
        
        linksXmlParser = null;
        
        caseXmlParser = null;
        
        otherwiseXmlParser = null;
        
        copyXmlParser = null;
        
        fromXmlParser = null;
        
        toXmlParser = null;
        
    	compensationHandlerXmlParser = null;
    	
    	terminationHandlerXmlParser = null;
        
        invokeXmlParser = null;
        
        throwXmlParser = null;
        
        terminateXmlParser = null;
        
        waitXmlParser = null;
        
        emptyXmlParser = null;
        
        whileXmlParser = null;
        
        pickXmlParser = null;
        
        onMessageXmlParser = null;
        
        onAlarmXmlParser = null;
        
        scopeXmlParser = null;
        
        compensateXmlParser = null;
        compensateScopeXmlParser = null;
        
        forEachXmlParser = null;
        fromPartXmlParser = null;
        toPartXmlParser = null;

        chooseXmlParser = null;

        whenXmlParser = null;

        defaultXmlParser = null;

        mExtensibilityElementXmlParser = null;

        mDocumentationXmlParser = null;
        
        conditionXmlParser = null;
        
        literalXmlParser = null;
        
        forXmlParser = null;
        
        untilXmlParser = null;

        rethrowXmlParser = null;
        
        repeatUntilXmlParser = null;
        
        validateXmlParser = null;
    }

    public boolean visit(EventHandlers eventHandlers) {
        // TODO Auto-generated method stub
        if (null == eventHandlersParser) {
            eventHandlersParser = this.mParserFactory.createEventHandlersXmlParser();
        }
         getParserSupport().pushXmlParser(eventHandlersParser, eventHandlers);
        return false;

    }

    public boolean visit(EventHandlersOnAlarm onAlarm) {
        // TODO Auto-generated method stub
        if (null == eventHandlersOnAlarmParser) {
            eventHandlersOnAlarmParser = this.mParserFactory.createEventHandlersOnAlarmXmlParser();
        }
         getParserSupport().pushXmlParser(eventHandlersOnAlarmParser, onAlarm);
        return false;

    }

    public boolean visit(EventHandlersOnEvent onEvent) {
        // TODO Auto-generated method stub
        if (null == eventHandlersOnEventParser) {
            eventHandlersOnEventParser = this.mParserFactory.createEventHandlersOnEventXmlParser();
        }
         getParserSupport().pushXmlParser(eventHandlersOnEventParser, onEvent);
        return false;

    }

    public boolean visit(RepeatEvery repeatEvery) {
        // TODO Auto-generated method stub
        if (null == repeatEveryParser) {
            repeatEveryParser = this.mParserFactory.createRepeatEveryXmlParser();
        }
         getParserSupport().pushXmlParser(repeatEveryParser, repeatEvery);
        return false;

    }
    
    public boolean visit(Trace trace) {
        if (null == traceSunExtParser) {
        	traceSunExtParser = this.mParserFactory.createTraceSunExtXmlParser();
        }
         getParserSupport().pushXmlParser(traceSunExtParser, trace);
        return false;

    }
    
    public boolean visit(Log log) {
        if (null == logSunExtParser) {
        	logSunExtParser = this.mParserFactory.createLogSunExtXmlParser();
        }
         getParserSupport().pushXmlParser(logSunExtParser, log);
        return false;

    }
    
    public boolean visit(Alert alert) {
        if (null == alertSunExtParser) {
        	alertSunExtParser = this.mParserFactory.createAlertSunExtXmlParser();
        }
         getParserSupport().pushXmlParser(alertSunExtParser, alert);
        return false;

    }

    public boolean visit(ExtensionAssignOperation extensionAssignOperation) {
        if (null == extensionAssignOperationParser) {
            extensionAssignOperationParser = mParserFactory.createExtensionAssignOperationElementParser();
        }
        getParserSupport().pushXmlParser(extensionAssignOperationParser, extensionAssignOperation);
        return false;

    }

    /** Visits a Literal element.
     * @param   c   a extensionAssignOperation.Expression element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(SunExtExpression c) {
        if (null == sunExtExpressionXmlParser) {
            sunExtExpressionXmlParser = mParserFactory.createSunExtExpressionElementParser();
        }
         getParserSupport().pushXmlParser(sunExtExpressionXmlParser, c);
        return false;
    }
}
