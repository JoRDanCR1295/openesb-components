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
 * @(#)RXmlParserFactoryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.parser.impl;

import java.util.logging.Logger;

import org.xml.sax.SAXException;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.Case;
import com.sun.bpel.model.CompletionCondition;
import com.sun.bpel.model.Correlation;
import com.sun.bpel.model.CorrelationSet;
import com.sun.bpel.model.Correlations;
import com.sun.bpel.model.ElseIf;
import com.sun.bpel.model.EventHandlers;
import com.sun.bpel.model.EventHandlersOnAlarm;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.bpel.model.Flow;
import com.sun.bpel.model.ForEach;
import com.sun.bpel.model.ForOrUntilReference;
import com.sun.bpel.model.From;
import com.sun.bpel.model.If;
import com.sun.bpel.model.OnMessage;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.RepeatUntil;
import com.sun.bpel.model.Reply;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.Throw;
import com.sun.bpel.model.To;
import com.sun.bpel.model.Validate;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.While;
import com.sun.bpel.model.common.MessageManager;
import com.sun.bpel.model.common.visitor.XmlParser;
import com.sun.bpel.model.extensions.When;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RCorrelation;
import com.sun.bpel.model.meta.RCorrelationSet;
import com.sun.bpel.model.meta.RExpressionElement;
import com.sun.bpel.model.meta.RInvoke;
import com.sun.bpel.model.meta.RMutableExpressionElement;
import com.sun.bpel.model.meta.ROnMessage;
import com.sun.bpel.model.meta.RReceive;
import com.sun.bpel.model.meta.RReply;
import com.sun.bpel.model.meta.RValidate;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.RVariableElement;
import com.sun.bpel.model.meta.ScopingElement;
import com.sun.bpel.model.meta.impl.REventHandlersOnEventImpl;
import com.sun.bpel.model.meta.impl.ROnMessageImpl;
import com.sun.bpel.model.util.Utility;
import com.sun.bpel.model.visitor.DefaultXmlParserFactory;
import com.sun.bpel.xml.common.model.XMLNode;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;

/**
 * Runtime XML parser factory implementation
 * 
 * @author Sun Microsystems
 */
public class RXmlParserFactoryImpl extends DefaultXmlParserFactory {
    
    /** The logger. */
    private static final Logger LOGGER = Logger.getLogger(RXmlParserFactoryImpl.class.getName());
    
    /** MessageManager for localized strings. */    
    private static MessageManager MESSAGES = MessageManager.getManager(RXmlParserFactoryImpl.class);
    
    /**
     * @see DefaultXmlParserFactory#createVariableXmlParser()
     */
    public XmlParser createVariableXmlParser() {
        return new RVariableXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createCorrelationsXmlParser()
     */
    public XmlParser createCorrelationsXmlParser() {
        return new RCorrelationsXmlParser();
    }

    /**
     * @see DefaultXmlParserFactory#createCorrelationSetXmlParser()
     */
    public XmlParser createCorrelationSetXmlParser() {
        return super.createCorrelationSetXmlParser();
    }

    /**
     * @see DefaultXmlParserFactory#createSequenceXmlParser()
     */
    public XmlParser createSequenceXmlParser() {
        return new RSequenceXmlParser();
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.common.model.bpel.visitor.DefaultXmlParserFactory#createFlowXmlParser()
     */
    public XmlParser createFlowXmlParser() {
        return new RFlowXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createThrowXmlParser()
     */
    public XmlParser createThrowXmlParser() {
        return new RThrowXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createReceiveXmlParser()
     */
    public XmlParser createReceiveXmlParser() {
        return new RReceiveXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createReplyXmlParser()
     */
    public XmlParser createReplyXmlParser() {
        return new RReplyXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createFromXmlParser()
     */
    public XmlParser createFromXmlParser() {
        return new RFromXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createToXmlParser()
     */
    public XmlParser createToXmlParser() {
        return new RToXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createPickXmlParser()
     */
    public XmlParser createPickXmlParser() {
        return new RPickXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createOnMessageXmlParser()
     */
    public XmlParser createOnMessageXmlParser() {
        return new ROnMessageXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createInvokeXmlParser()
     */
    public XmlParser createInvokeXmlParser() {
        return new RInvokeXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createCaseXmlParser()
     */
    public XmlParser createCaseXmlParser() {
        return new RCaseXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createElseIfXmlParser()
     */
    public XmlParser createElseIfXmlParser() {
        return new RElseIfXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createIfXmlParser()
     */
    public XmlParser createIfXmlParser() {
        return new RIfXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createRepeatUntilXmlParser()
     */
    public XmlParser createRepeatUntilXmlParser() {
        return new RRepeatUntilXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createWhenParser()
     */
    public XmlParser createWhenXmlParser() {
        return new RWhenXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createWhileParser()
     */
    public XmlParser createWhileXmlParser() {
        return new RWhileXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createProcessXmlParser()
     */
    public XmlParser createProcessXmlParser() {
        return new RProcessXmlParser();
    }

    /**
     * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory#createForEachXmlParser()
     */
    public XmlParser createForEachXmlParser() {
        return new RForEachParser();
    }

    public XmlParser createEventHandlersXmlParser() {
        return new REventHandlersXmlParser();
    }
    
    public XmlParser createEventHandlersOnAlarmXmlParser() {
        return new REventHandlersOnAlarmXmlParser();
    }

    public XmlParser createEventHandlersOnEventXmlParser() {
        return new REventHandlersOnEventXmlParser();
    }

    public XmlParser createWaitXmlParser() {
        return new RWaitXmlParser();
    }

    public XmlParser createOnAlarmXmlParser() {
        return new ROnAlarmXmlParser();
    }

    @Override
    public XmlParser createValidateXmlParser() {
        return new RValidateXmlParser();
    }

    /**
     * Runtime sequence xml parser
     * 
     * @author Sun Microsystems
     * @version 
     */
    protected class RSequenceXmlParser extends SequenceXmlParser {
        /**
         * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String,
         *      java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {

            Sequence sequence = (Sequence) getCurrentXMLNode();
            int size = sequence.getActivitySize();
            RActivity prevAct = null;

            if (size > 0) {
                RActivity child = (RActivity) sequence.getActivity(0);
                ((RActivityHolder) sequence).setChildActivity(child);
                prevAct = child;
            }

            RActivity currAct = null;

            for (int i = 1; i < size; i++) {
                currAct = (RActivity) sequence.getActivity(i);
                prevAct.setNextActivity(currAct);
                prevAct = currAct;
            }

            //Call the super to finish the processing for this element
            super.endElement(uri, localName, qName);
        }
    }

    /**
     * Runtime flow xml parser
     * 
     * @author Sun Microsystems
     * @version 
     */
    protected class RFlowXmlParser extends FlowXmlParser {
        /**
         * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String,
         *      java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {

            Flow flow = (Flow) getCurrentXMLNode();
            int size = flow.getActivitySize();
            RActivity prevAct = null;

            if (size > 0) {
                RActivity child = (RActivity) flow.getActivity(0);
                ((RActivityHolder) flow).setChildActivity(child);
                prevAct = child;
            }

            RActivity currAct = null;

            for (int i = 1; i < size; i++) {
                currAct = (RActivity) flow.getActivity(i);
                prevAct.setNextActivity(currAct);
                prevAct = currAct;
            }

            //Call the super to finish the processing for this element
            super.endElement(uri, localName, qName);
        }
    }

    /**
     * Runtime variable xml parser
     * 
     * @author Sun Microsystems
     * @version 
     */
    protected class RVariableXmlParser extends VariableXmlParser {
        /**
         * endElement
         * 
         * @param uri URI
         * @param localName local name
         * @param qName qualified name
         * @throws SAXException SAXException
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            RVariable var = (RVariable) getCurrentXMLNode();
            long scopeId = ((ScopingElement) var.getParent().getParent()).getScopeId();
            var.setScopeId(scopeId);

            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * Runtime Receive xml parser class
     */
    protected class RReceiveXmlParser extends ReceiveXmlParser {
        /**
         * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String,
         *      java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            Receive rec = (Receive) getCurrentXMLNode();
            RReceive rRec = (RReceive) getCurrentXMLNode();
            RVariable var = (RVariable) BPELHelper.getMatchingVariable(rec.getVariable(), rec);
            rRec.setRVariable(var);
            rRec.setCreateInstance(Utility.getCreateInstanceValue(rec.getCreateInstance()));

            BPELDocument doc = (BPELDocument) rec.getOwnerDocument();
            RBPELProcess proc = (RBPELProcess) doc.getDocumentProcess();
            ParseCorrelationHelper.registerCorrelations(rRec, proc);
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * Runtime Reply xml parser class
     */
    protected class RReplyXmlParser extends ReplyXmlParser {
        /**
         * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String,
         *      java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            Reply rep = (Reply) getCurrentXMLNode();
            if(rep.getWSDLOperation().getOutput() == null) {
                throw new RuntimeException(MESSAGES.getString("RXmlParserFactoryImpl_REPLY_NOT_VALID_FOR_ONE_WAY_OPER", rep.getName()));
            }            
            
            RReply rRep = (RReply) getCurrentXMLNode();
            RVariable var = (RVariable) BPELHelper.getMatchingVariable(rep.getVariable(), rep);
            
            if(var == null) {
                throw new RuntimeException(MESSAGES.getString("RXmlParserFactoryImpl_VAR_NOT_DEFINED_FOR_REPLY", rep.getName()));
            }
            
            rRep.setRVariable(var);

            BPELDocument doc = (BPELDocument) rep.getOwnerDocument();
            RBPELProcess proc = (RBPELProcess) doc.getDocumentProcess();
            ParseCorrelationHelper.registerCorrelations(rRep, proc);
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * Runtime pick xml parser
     * 
     * @author Sun Microsystems
     * @version 
     */
    protected class RPickXmlParser extends PickXmlParser {
        /**
         * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String,
         *      java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            Pick pick = (Pick) getCurrentXMLNode();

            for (int i = 0, size = pick.getOnMessageSize(); i < size; i++) {
                ROnMessageImpl startElem = (ROnMessageImpl) pick.getOnMessage(i);
                startElem.setCreateInstance(Utility.getCreateInstanceValue(pick.getCreateInstance()));
            }
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * Runtime OnMessage parser
     * 
     * @author Sun Microsystems
     * @version 
     */
    protected class ROnMessageXmlParser extends OnMessageXmlParser {
        /**
         * @see DefaultXmlParserFactory.OnMessageXmlParser#endElement(java.lang.String,
         *      java.lang.String, java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            OnMessage onMsg = (OnMessage) getCurrentXMLNode();
            ROnMessage rOnMsg = (ROnMessage) getCurrentXMLNode();
            RVariable var = (RVariable) BPELHelper.getMatchingVariable(onMsg.getVariable(), onMsg);
            rOnMsg.setRVariable(var);

            BPELDocument doc = (BPELDocument) onMsg.getOwnerDocument();
            RBPELProcess proc = (RBPELProcess) doc.getDocumentProcess();
            ParseCorrelationHelper.registerCorrelations(rOnMsg, proc);
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * Runtime throw xml parser
     * 
     * @author Sun Microsystems
     * @version 
     */
    protected class RThrowXmlParser extends ThrowXmlParser {
        /**
         * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String,
         *      java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            Throw t = (Throw) getCurrentXMLNode();
            RVariableElement rVarElem = (RVariableElement) getCurrentXMLNode();
            RVariable var = (RVariable) BPELHelper.getMatchingVariable(t.getFaultVariable(), t);
            rVarElem.setRVariable(var);
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * Runtime from xml parser
     * 
     * @author Sun Microsystems
     * @version 
     */
    protected class RFromXmlParser extends FromXmlParser {
        /**
         * @see org.xml.sax.ContentHandler#endElement(java.lang.String,
         *      java.lang.String, java.lang.String)
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            From from = (From) getCurrentXMLNode();
            //if expression is empty spaces, reset it
            if (Utility.isEmpty(from.getExpression())) {
                from.setExpression(null);
            }

            if (!Utility.isEmpty(from.getVariable())) {
                String variable = from.getVariable();
                String property = from.getProperty();
                if (!Utility.isEmpty(property)) {
                    String propExpression = getPropertyExpression(variable,
                            property);
                    try {
                        ((RExpressionElement) from)
                        .setXPathExpression(propExpression);
                    } catch (Exception e) {
                        throw new SAXException(
                                "Exception while parsing the From-spec property variant : "
                                + from, e);
                    }
                }
                RVariable var = (RVariable) BPELHelper.getMatchingVariable(
                        variable, from);
                RVariableElement rFrom = (RVariableElement) from;
                rFrom.setRVariable(var);
            } else {
                String expr = from.getExpression();
                if (!Utility.isEmpty(expr) && !expr.equals(".")) { //$NON-NLS-1$
                    try {
                        ((RExpressionElement) getCurrentXMLNode())
                        .setXPathExpression(expr);
                    } catch (Exception e) {
                        throw new SAXException(
                                "Exception while parsing the expression : "
                                + expr, e);
                    }
                }
            }

            // Call the super to finish the processing for this element
            super.endElement(uri, localName, qName);
        }
    }

    /**
         * Runtime to xml parser
         * 
         * @author Sun Microsystems
         */
    protected class RToXmlParser extends ToXmlParser {
	/**
         * @see org.xml.sax.ContentHandler#endElement(java.lang.String,
         *      java.lang.String, java.lang.String)
         */
	public void endElement(String uri, String localName, String qName)
		throws SAXException {

	    To to = (To) getCurrentXMLNode();
            
            //if expression is empty spaces, reset it
            if (Utility.isEmpty(to.getQuery())) {
                to.setQuery(null);
            }
	    if (!Utility.isEmpty(to.getVariable())) {
		String variable = to.getVariable();
		String property = to.getProperty();
		if (!Utility.isEmpty(property)) {
		    String propExpression = getPropertyExpression(variable,
			    property);
		    try {
			((RExpressionElement) to)
				.setXPathExpression(propExpression);
		    } catch (Exception e) {
			throw new SAXException(
				"Exception while parsing the To-spec property variant : "
					+ to, e);
		    }
		}

		RVariable var = (RVariable) BPELHelper.getMatchingVariable(
			variable, to);
		RVariableElement rTo = (RVariableElement) to;
		rTo.setRVariable(var);
	    } else {

		String query = to.getQuery();
		if (!Utility.isEmpty(query) && !query.equals(".")) { //$NON-NLS-1$

		    String args[] = Utility.getArgs2(query);
		    String varName = args[0];
		    String partName = args[1];
		    String expr = args[2];

		    RVariable var = (RVariable) BPELHelper.getMatchingVariable(
			    varName, to);
		    RVariableElement rTo = (RVariableElement) to;
		    rTo.setRVariable(var);
		    to.setPart(partName);

		    if (!Utility.isEmpty(expr)) {
			((RMutableExpressionElement) getCurrentXMLNode())
				.setModifiesDocStructure(true);
		    }
		    try {
			((RExpressionElement) getCurrentXMLNode())
				.setXPathExpression(query);
		    } catch (Exception e) {
			throw new SAXException(
				"Exception while parsing the expression : "
					+ query, e);
		    }
		}
	    }
	    // Call the super to finish the processing for this element
	    super.endElement(uri, localName, qName);
	}
    }

    /**
         * DOCUMENT ME!
         * 
         * @author Sun Microsystems
         * @version
         */
    class RCorrelationsXmlParser extends CorrelationsXmlParser {
        /**
         * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory.CorrelationsXmlParser#endElement(java.lang.String,
         *      java.lang.String, java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            Correlations corrs = (Correlations) getCurrentXMLNode();
            int corrSize = corrs.getCorrelationSize();
            Correlation rCorr = null;

            for (int i = 0; i < corrSize; i++) {
                rCorr = (Correlation) corrs.getCorrelation(i);
                ((RCorrelation) rCorr).setCorrelationSet((RCorrelationSet) rCorr.getBPELCorrelationSet());
            }

            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * @author Sun Microsystems
     */
    class RCaseXmlParser extends CaseXmlParser {

        public void endElement(String uri, String localName, String qName) throws SAXException {

            String expr = ((Case) getCurrentXMLNode()).getCondition();
            if (!Utility.isEmpty(expr) && !expr.equals(".")) {
                try {
                    ((RExpressionElement) getCurrentXMLNode()).setXPathExpression(expr);
                } catch (Exception e) {
                    throw new SAXException("Exception while parsing the expression :" + expr, e);
                }
            }
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @author Sun Microsystems
     */
    class RElseIfXmlParser extends ElseIfXmlParser {

        public void endElement(String uri, String localName, String qName) throws SAXException {

            ElseIf elseIf = (ElseIf) getCurrentXMLNode();
            String expr = elseIf.getCondition();

            if (!Utility.isEmpty(expr) && !expr.equals(".")) {
                try {
                    ((RExpressionElement) elseIf).setXPathExpression(expr);
                } catch (Exception e) {
                    throw new SAXException("Exception while parsing the expression :" + expr, e);
                }
            }
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @author Sun Microsystems
     */
    class RIfXmlParser extends IfXmlParser {

        public void endElement(String uri, String localName, String qName) throws SAXException {

            String expr = ((If) getCurrentXMLNode()).getCondition();
            if (!Utility.isEmpty(expr) && !expr.equals(".")) {
                try {
                    ((RExpressionElement) getCurrentXMLNode()).setXPathExpression(expr);
                } catch (Exception e) {
                    throw new SAXException("Exception while parsing the expression :" + expr, e);
                }
            }
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @author Sun Microsystems
     */
    class RRepeatUntilXmlParser extends RepeatUntilXmlParser {

        public void endElement(String uri, String localName, String qName) throws SAXException {

            String expr = ((RepeatUntil) getCurrentXMLNode()).getCondition();
            if (!Utility.isEmpty(expr) && !expr.equals(".")) {
                try {
                    ((RExpressionElement) getCurrentXMLNode()).setXPathExpression(expr);
                } catch (Exception e) {
                    throw new SAXException("Exception while parsing the expression :" + expr, e);
                }
            }
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @author Sun Microsystems
     */
    class RWhenXmlParser extends WhenXmlParser {

        public void endElement(String uri, String localName, String qName) throws SAXException {

            String expr = ((When) getCurrentXMLNode()).getCondition();
            if (!Utility.isEmpty(expr) && !expr.equals(".")) {
                try {
                    ((RExpressionElement) getCurrentXMLNode()).setXPathExpression(expr);
                } catch (Exception e) {
                    throw new SAXException("Exception while parsing the expression :" + expr, e);
                }
            }

            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @author Sun Microsystems
     */
    class RWhileXmlParser extends WhileXmlParser {

        public void endElement(String uri, String localName, String qName) throws SAXException {

            String expr = ((While) getCurrentXMLNode()).getCondition();
            if (!Utility.isEmpty(expr) && !expr.equals(".")) {
            	try {
            		((RExpressionElement) getCurrentXMLNode()).setXPathExpression(expr);
            	} catch (Exception e) {
            		throw new SAXException("Exception while parsing the expression :" + expr, e);
            	}
            }
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @author Sun Microsystems
     */
    class RForEachParser extends ForEachXmlParser {

        public void endElement(String uri, String localName, String qName) throws SAXException {
        	
        	/* bug: 1250 <documentation> may be the first child element of this <forEach>, we ignore documentation
        	 * tag and hence the SAXParser will call the endElement of the <forEach> here before going to the 
        	 * other child elements of the <forEach> hence we should ignore the call for the <documenation> child 
        	 * here .
        	*/
        	if ("documentation".equalsIgnoreCase(localName)){
        		return;
        	}
            String startCounterExpr = ((ForEach) getCurrentXMLNode()).getIterator().getStartCounterValue().getValue();
            String finalCounterExpr = ((ForEach) getCurrentXMLNode()).getIterator().getFinalCounterValue().getValue();
            String branchesExpr = null;

            if (((ForEach) getCurrentXMLNode()).getCompletionCondition() != null) {
                CompletionCondition completeCondition = ((ForEach) getCurrentXMLNode()).getCompletionCondition();
                branchesExpr = completeCondition.getBranches().getValue();
            }
            String expr = "( " + startCounterExpr + " ) or ( " + finalCounterExpr + " ) or ( "
                    + branchesExpr + " )";

            try {
                try {
                    ((RExpressionElement) getCurrentXMLNode()).setXPathExpression(expr);
                } catch (Exception e) {
                    throw new SAXException("Exception while parsing the expression :", e);
                }
            } catch (SAXException e) {
                throw e;
            }
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @author Sun Microsystems
     */
    protected class RInvokeXmlParser extends InvokeXmlParser {
        /**
         * DOCUMENT ME!
         * 
         * @param uri DOCUMENT ME!
         * @param localName DOCUMENT ME!
         * @param qName DOCUMENT ME!
         * @throws SAXException DOCUMENT ME!
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            RInvoke rInvoke = (RInvoke) getCurrentXMLNode();
            BPELDocument doc = (BPELDocument) rInvoke.getOwnerDocument();
            RBPELProcess proc = (RBPELProcess) doc.getDocumentProcess();
            ParseCorrelationHelper.registerCorrelations(rInvoke, proc);

            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * @author Sun Inc Apr 27, 2006
     */
    protected class RProcessXmlParser extends ProcessXmlParser {

        /**
         * @see com.sun.bpel.model.visitor.DefaultXmlParserFactory.ProcessXmlParser#endElement(java.lang.String,
         *      java.lang.String, java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            RBPELProcess proc = (RBPELProcess) getCurrentXMLNode();
            ParseCorrelationHelper.registerCorrelations(proc);
            
            proc.setNMPropToPropAliasMap();
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }

    }

    /**
     * Runtime EventHandlersOnEvent parser
     * 
     * @author Sun Microsystems
     * @version 
     */
    protected class REventHandlersOnEventXmlParser extends EventHandlersOnEventXmlParser {
        /**
         * @see DefaultXmlParserFactory.EventHandlersOnEventXmlParser#endElement(java.lang.String,
         *      java.lang.String, java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            EventHandlersOnEvent onMsg = (EventHandlersOnEvent) getCurrentXMLNode();
            REventHandlersOnEventImpl rOnMsg = (REventHandlersOnEventImpl) getCurrentXMLNode();
            RVariable var = (RVariable) rOnMsg.getBPELVariable();
            ScopingElement childScope = (ScopingElement) ((RActivityHolder) onMsg).getChildActivity();
            if (var != null) {
                //var can be null if the input message of the operation has no parts.
                var.setScopeId(childScope.getScopeId());
                Variables variables = ((Scope) childScope).getVariables();
                if (variables == null) {
                    // This is a virtual "Variables" node, hence won't have any locator or anything like that.
                    variables = getXmlDocument().createVariables();
                    ((Scope) childScope).setVariables(variables);
                }
                variables.addVariable(var);
            }
            rOnMsg.setRVariable(var);

            // Reset Correlation because the Correlation should be resolved to the CorrelationSet in
            // enclosed scope first,
            // this essentially gets the correlation
            resetCorrelation();

            BPELDocument doc = (BPELDocument) onMsg.getOwnerDocument();
            RBPELProcess proc = (RBPELProcess) doc.getDocumentProcess();
            ParseCorrelationHelper.registerCorrelations(rOnMsg, proc);
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }

        private void resetCorrelation() {
            EventHandlersOnEvent onEvent = (EventHandlersOnEvent) getCurrentXMLNode();
            Correlations corrs = (Correlations) onEvent.getCorrelations();
            int corrSize = corrs.getCorrelationSize();
            Correlation rCorr = null;

            for (int i = 0; i < corrSize; i++) {
                rCorr = (Correlation) corrs.getCorrelation(i);
                if (Utility.isEmpty(rCorr.getSet())) {
                    return;
                }
                String setName = rCorr.getSet();
                CorrelationSet amCorrSet = BPELHelper.getMatchingCorrelationSetToScope(setName,
                        (Scope) onEvent.getActivity());
                if (amCorrSet != null) {
                    ((RCorrelation) rCorr).setCorrelationSet((RCorrelationSet) amCorrSet);
                }
            }
        }
    }

    /**
     * Runtime Wait activity parser
     * 
     * @author Sun Microsystems
     */
    protected class RWaitXmlParser extends WaitXmlParser {
        /**
         * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String,
         *      java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            parseForUntilExpression(getCurrentXMLNode());
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * Runtime OnAlarm element parser
     * 
     * @author Sun Microsystems
     */
    protected class ROnAlarmXmlParser extends OnAlarmXmlParser {
        /**
         * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String,
         *      java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            parseForUntilExpression(getCurrentXMLNode());
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * Runtime EventHandlersOnAlarm element parser
     * 
     * @author Sun Microsystems
     */
    protected class REventHandlersOnAlarmXmlParser extends EventHandlersOnAlarmXmlParser {
        /**
         * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String,
         *      java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
        	
        	/* bug: 587 <documentation> may be the first child element of this <OnAlarm>, we ignore documentation
        	 * tag and hence the SAXParser will call the endElement of the <forEach> here before going to the 
        	 * other child elements of the <OnAlarm> hence we should ignore the call for the <documentation> child 
        	 * here .
        	*/
        	if ("documentation".equalsIgnoreCase(localName)){
        		return;
        	}
        	
            String query = null;
            ForOrUntilReference forUntilNode = (ForOrUntilReference) getCurrentXMLNode();
            String forUntilQuery = forUntilNode.getFor();
            if (Utility.isEmpty(forUntilQuery)) {
                forUntilQuery = forUntilNode.getUntil();
            }
            EventHandlersOnAlarm onAlarm = (EventHandlersOnAlarm) getCurrentXMLNode();
            String repeatEveryQuery = onAlarm.getRepeatEvery();

            if (Utility.isEmpty(forUntilQuery)) {
                query = repeatEveryQuery;
            } else if (Utility.isEmpty(repeatEveryQuery)) {
                query = forUntilQuery;
            } else {
                query = "( " + repeatEveryQuery + " ) or ( " + forUntilQuery + " )";
            }
            try {
                ((RExpressionElement) getCurrentXMLNode()).setXPathExpression(query);
            } catch (Exception e) {
                throw new SAXException("Exception while parsing the expression : "
                        + repeatEveryQuery, e);
            }
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * Runtime Validate xml parser class
     */
    protected class RValidateXmlParser extends ValidateXmlParser {
        /**
         * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String,
         *      java.lang.String)
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
            Validate validate = (Validate) getCurrentXMLNode();
            RValidate rValidate = (RValidate) getCurrentXMLNode();

            List<Variable> vars = validate.getBPELVariables();
            Set<RVariable> rVariables = new HashSet<RVariable>();
            int i =0;
            for (Variable var : vars) {
                i++;
                RVariable rVar = (RVariable) BPELHelper.getMatchingVariable(
                        var.getName(), validate);
                if (rVar != null) {
                    rVariables.add(rVar);
                }

            }
            rValidate.setRVariables(rVariables);

            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }


    
    /**
     * Currently we do not support Phase 2 Scalability (instance passivation)
     * for processes that have event handlers defined. The following
     * will set the flag on the process definition the fact that 
     * this process has event handlers defined.
     * NOTE: Remove the following code when the Event Handler
     * support for phase 2 passivation is implemented. Also,
     * the apis (setEventHandlersDefined and isEventHandlersDefined)
     * on RBPELProcess should be removed.
     */
    protected class REventHandlersXmlParser extends EventHandlersXmlParser {
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName) throws SAXException {
        	EventHandlers eh = (EventHandlers) getCurrentXMLNode();
            BPELDocument doc = (BPELDocument) eh.getOwnerDocument();
            RBPELProcess proc = (RBPELProcess) doc.getDocumentProcess();
            proc.setEventHandlersDefined();
            
            //Call the super to finish the processing for this element
        	super.endElement(uri, localName, qName);
        }
    }

    /**
     * Runtime parser for for/until element of Wait, OnAlarm and EventHandlerOnAlarm's activity
     * 
     * @author Sun Microsystems
     */
    private static void parseForUntilExpression(XMLNode node) throws SAXException {
        ForOrUntilReference forUntilNode = (ForOrUntilReference) node;
        String query = forUntilNode.getFor();
        if (Utility.isEmpty(query)) {
            query = forUntilNode.getUntil();
        }
        if (Utility.isEmpty(query)) {
            return;
        }

        //todovb m
        if (Utility.isAppVar(query)) {
            // application variables string have to be treated as a string constant
            query = ApplicationVariablesHelper.wrapInQuotes(query);
        }
        
        try {
            ((RExpressionElement) node).setXPathExpression(query);
        } catch (Exception e) {
            throw new SAXException("Exception while parsing the expression : " + query, e);
        }

    }
    private static final String GET_VARIABLE_PROPERTY = "getVariableProperty(";
    private static final String COMMA = ",";
    private static final String END_BRACKET = ")";
    private static final String SINGLE_QUOTE = "'";
    
    private static String getPropertyExpression(String variable, String property) {
    	StringBuffer buffer = new StringBuffer();
    	buffer.append(GET_VARIABLE_PROPERTY);
    	buffer.append(SINGLE_QUOTE);
    	buffer.append(variable);
    	buffer.append(SINGLE_QUOTE);
    	buffer.append(COMMA);
    	buffer.append(SINGLE_QUOTE);
    	buffer.append(property);
    	buffer.append(SINGLE_QUOTE);
    	buffer.append(END_BRACKET);
    	return buffer.toString();
    }
}
