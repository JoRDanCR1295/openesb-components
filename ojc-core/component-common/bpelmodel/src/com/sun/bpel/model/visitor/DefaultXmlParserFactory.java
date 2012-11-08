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
 * @(#)DefaultXmlParserFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.visitor;

import java.io.CharArrayWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import com.sun.bpel.model.Activity;
import com.sun.bpel.model.Assign;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.BPELProcessOrScope;
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
import com.sun.bpel.model.common.MessageManager;
import com.sun.bpel.model.common.visitor.SAXParserSupport;
import com.sun.bpel.model.common.visitor.XmlParser;
import com.sun.bpel.model.extensions.Alert;
import com.sun.bpel.model.extensions.Choose;
import com.sun.bpel.model.extensions.Default;
import com.sun.bpel.model.extensions.ExtensibilityElement;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.model.extensions.Log;
import com.sun.bpel.model.extensions.Trace;
import com.sun.bpel.model.extensions.When;
import com.sun.bpel.model.impl.BaseElementImpl;
import com.sun.bpel.model.meta.impl.RPartnerLinkImpl;
import com.sun.bpel.model.util.I18n;
import com.sun.bpel.model.util.Namespaces;
import com.sun.bpel.model.util.ParsingCaches;
import com.sun.bpel.model.util.Utility;
import com.sun.bpel.xml.common.model.Documentation;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.model.XMLProcessingInstruction;
import com.sun.bpel.xml.common.model.XMLText;
import com.sun.bpel.xml.common.visitor.VisitorService;

public class DefaultXmlParserFactory {
    
    
    /** Message key for UNRECOGNIZED_START_ELEMENT */
    private static final String UNRECOGNIZED_START_ELEMENT = "UNRECOGNIZED_START_ELEMENT";  // Not I18N
    
    /** Message key for UNRECOGNIZED_END_ELEMENT */
    private static final String UNRECOGNIZED_END_ELEMENT = "UNRECOGNIZED_END_ELEMENT";  // Not I18N
    
    /** Message key for ILLEGAL_ELEMENT_ENCOUNTERED */
    private static final String ILLEGAL_ELEMENT_ENCOUNTERED = "ILLEGAL_ELEMENT_ENCOUNTERED";  // Not I18N
    
    /** MessageManager for localized strings. */
    private MessageManager mMsg = MessageManager.getManager(DefaultXmlParserFactory.class);
    
    private static final Logger mLogger = Logger.getLogger(DefaultXmlParserFactory.class.getName());
    
    
    private SAXParseVisitor mVisitor;
    
    private SAXParserSupport mVisitorSupport;
    
    public XmlParser createImportXmlParser() {
        return null;
    }
    
    public XmlParser createAssignXmlParser() {
        return new AssignXmlParser();
    }
    
    public XmlParser createFromPartXMLParser() {
        return new FromPartXMLParser();
    }
    
    public XmlParser createToPartXMLParser() {
        return new ToPartXMLParser();
    }
    
    public XmlParser createCatchAllXmlParser() {
        return new CatchAllXmlParser();
    }
    
    public XmlParser createCatchXmlParser() {
        return new CatchXmlParser();
    }
    
    public XmlParser createChooseXmlParser() {
        return new ChooseXmlParser();
    }
    
    public XmlParser createCompensateXmlParser() {
        return new CompensateXmlParser();
    }
    
    public XmlParser createCompensateScopeXmlParser() {
        return new CompensateScopeXmlParser();
    }

    public XmlParser createConditionXmlParser() {
        return new ConditionXmlParser();
    }
    
    public XmlParser createLiteralXmlParser() {
        return new LiteralXmlParser();
    }

    public XmlParser createExtensionAssignOperationElementParser() {
        return new ExtensionAssignOperationElementParser();
    }

    public XmlParser createSunExtExpressionElementParser() {
        return new SunExtExpressionElementParser();
    }
    
    public XmlParser createCopyXmlParser() {
        return new CopyXmlParser();
    }
    
    public XmlParser createCorrelationSetsXmlParser() {
        return new CorrelationSetsXmlParser();
    }
    
    public XmlParser createCorrelationSetXmlParser() {
        return null;
    }
    
    public XmlParser createCorrelationXmlParser() {
        return null;
    }
    
    public XmlParser createCorrelationsXmlParser() {
        return new CorrelationsXmlParser();
    }
    
    
    public XmlParser createDefaultXmlParser() {
        return new DefaultXmlParser();
    }
    
    public XmlParser createDocumentationXmlParser() {
        return new DocumentationXmlParser();
    }
    
    public XmlParser createEmptyXmlParser() {
        return new EmptyXmlParser();
    }
    
    public XmlParser createExitXmlParser() {
        return new ExitXmlParser();
    }
    
    public XmlParser createExtensibilityElementXmlParser() {
        return new ExtensibilityElementXmlParser();
    }
    
    public XmlParser createFaultHandlersXmlParser() {
        return new FaultHandlersXmlParser();
    }
    
    public XmlParser createCompensationHandlerXmlParser() {
        return new CompensationHandlerXmlParser();
    }
    
    public XmlParser createTerminationHandlerXmlParser() {
        return new TerminationHandlerXmlParser();
    }
    
    public XmlParser createFlowXmlParser() {
        return new FlowXmlParser();
    }
    
    public XmlParser createForEachSBYNXmlParser() {
        return new ForEachSBYNXmlParser();
    }
    
    public XmlParser createForEachXmlParser() {
        return new ForEachXmlParser();
    }    
    public XmlParser createForXmlParser() {
        return new ForXmlParser();
    }
    
    public XmlParser createFromXmlParser() {
        return new FromXmlParser();
    }
    
    public XmlParser createInvokeXmlParser() {
        return new InvokeXmlParser();
    }
    
    public XmlParser createLinksXmlParser() {
        return new LinksXmlParser();
    }
    
    public XmlParser createOnMessageXmlParser() {
        return new OnMessageXmlParser();
    }
    
    public XmlParser createOnAlarmXmlParser() {
        return new OnAlarmXmlParser();
    }
    
    public XmlParser createOtherwiseXmlParser() {
        return new OtherwiseXmlParser();
    }
    
    public XmlParser createPartnerLinksXmlParser() {
        return new PartnerLinksXmlParser();
    }
    
    public XmlParser createPartnerLinkXmlParser() {
        return null;
    }
    
    public XmlParser createPickXmlParser() {
        return new PickXmlParser();
    }
    
    public XmlParser createBPELDocumentXmlParser() {
        return new BPELDocumentXmlParser();
    }
    
    public XmlParser createProcessXmlParser() {
        return new ProcessXmlParser();
    }
    
    public XmlParser createReceiveXmlParser() {
        return new ReceiveXmlParser();
    }
    
    public XmlParser createRepeatUntilXmlParser() {
        return new RepeatUntilXmlParser();
    }
    
    public XmlParser createIfXmlParser() {
        return new IfXmlParser();
    }
    
    
    public XmlParser createElseIfXmlParser() {
        return new ElseIfXmlParser();
    }
    
    public XmlParser createElseXmlParser() {
        return new ElseXmlParser();
    }
    
    public XmlParser createReplyXmlParser() {
        return new ReplyXmlParser();
    }
    
    public XmlParser createRethrowXmlParser() {
        return new RethrowXmlParser();
    }
    
    public XmlParser createScopeXmlParser() {
        return new ScopeXmlParser();
    }
    
    public XmlParser createSequenceXmlParser() {
        return new SequenceXmlParser();
    }
    
    public XmlParser createSwitchXmlParser() {
        return new SwitchXmlParser();
    }
    
    public XmlParser createCaseXmlParser() {
        return new CaseXmlParser();
    }
    
    public XmlParser createThrowXmlParser() {
        return new ThrowXmlParser();
    }
    
    public XmlParser createToXmlParser() {
        return new ToXmlParser();
    }
    
    public XmlParser createUntilXmlParser() {
        return new UntilXmlParser();
    }
    
    public XmlParser createValidateXmlParser() {
        return new ValidateXmlParser();
    }
    
    public XmlParser createVariablesXmlParser() {
        return new VariablesXmlParser();
    }
    
    public XmlParser createVariableXmlParser() {
        return new VariableXmlParser();
    }
    
    public XmlParser createWaitXmlParser() {
        return new WaitXmlParser();
    }
    
    public XmlParser createWhenXmlParser() {
        return new WhenXmlParser();
    }
    
    public XmlParser createWhileXmlParser() {
        return new WhileXmlParser();
    }
    
    public XmlParser createStartCounterValueParser () {
        return new StartCounterValueParser();
    }
    
    public XmlParser createFinalCounterValueParser () {
        return new FinalCounterValueParser ();
    }
    
    public XmlParser createCompleteConditionParser () {
        return new CompleteConditionParser ();
    }
    
    public XmlParser createBranchesParser () {
        return new BranchesParser ();
    }
    
    public XmlParser createEventHandlersOnEventXmlParser () {
        return new EventHandlersOnEventXmlParser ();
    }

    public XmlParser createEventHandlersOnAlarmXmlParser () {
        return new EventHandlersOnAlarmXmlParser ();
    }

    public XmlParser createRepeatEveryXmlParser () {
        return new RepeatEveryXmlParser ();
    }
    
    public XmlParser createEventHandlersXmlParser () {
        return new EventHandlersXmlParser ();
    }
    
    public XmlParser createTraceSunExtXmlParser() {
    	return new TraceSunExtXmlParser();
    }
    
    public XmlParser createLogSunExtXmlParser() {
    	return new LogSunExtXmlParser();
    }
    
    public XmlParser createAlertSunExtXmlParser() {
    	return new AlertSunExtXmlParser();
    }
    
    
    public SAXParseVisitor getVisitor() {
        return this.mVisitor;
    }
    
    public void setVisitor(SAXParseVisitor visitor) {
        this.mVisitor = visitor;
    }
    
    public BPELDocument getXmlDocument() {
        return this.mVisitor.getXmlDocument();
    }
    
    
    
    /** Getter for the XML parser support.
     * @return  XML parser support.
     */
    public SAXParserSupport getParserSupport() {
        return getVisitor().getParserSupport();
    }
    
    /** Gets the service that provided this visitor as well as any other
     * needed to traverse other XML documents imported.
     *
     * @return  The visitor service.
     */
    public VisitorService getVisitorService() {
        return getVisitor().getVisitorService();
    }
    
    /** Implements XML parser for BPEL document.
     */
    protected class BPELDocumentXmlParser extends XmlParser {
        
        /** Constructs a BPELDocumentXmlParser.
         */
        public BPELDocumentXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            
            // If the process element is not from the namespace that is currently supported
            // throw an exception
           if (!Namespaces.isBPELElement(BPELProcess.TAG, uri, localName, qName)) {
                if(BPELProcess.TAG == localName) {
                    throw new RuntimeException(mMsg.getString("UPSUPPORTED_BPEL_VERSION", BPELDocument.BPEL_NAMESPACE));
                } else {
                    throw new RuntimeException(mMsg.getString("NOT_A_BPEL_DOC", BPELDocument.BPEL_NAMESPACE));
                }
            }
            
            SAXParseVisitorService vService =
                    (SAXParseVisitorService) getVisitorService();
            BPELParseContext context = vService.getBPELParseContext();
           
            BPELProcess tProcess = getXmlDocument().createProcess();
            getXmlDocument().setDocumentProcess(tProcess);
            tProcess.setQualifiedName(qName);
            tProcess.setLocator(getParserSupport().getLocator());
            SAXParserSupport.setAttributes(tProcess, attributes);
            tProcess.accept(getVisitor());
            tProcess.createAndSetNamespacePrefix(BPELProcess.XSD_NAME_SPACE_URI, BPELProcess.XSD_NAME_SPACE_PREFIX);
            context.getDeferredActionRegistry()
                .getTypeLoaderHolderCollector().add(tProcess);
        }
        
        /** Called when a processing instruction is encountered.
         * @param   target  Target of processing instruction.
         * @param   data    Data of processing instruction.
         * @throws  SAXException    When SAX problems occur.
         */
        public void processingInstruction(String target, String data)
        throws SAXException {
            XMLProcessingInstruction tProcessingInstruction =
                    getXmlDocument().createProcessingInstruction();
            getXmlDocument().addProcessingInstruction(tProcessingInstruction);
            tProcessingInstruction.setTarget(target);
            tProcessingInstruction.setData(data);
            tProcessingInstruction.setLocator(getParserSupport().getLocator());
            tProcessingInstruction.accept(getVisitor());
        }
    }
    
    
    /** Implements XML parser for process element.
     */
    protected class ProcessXmlParser extends XmlParser {
        
        /** Constructs a ProcessXmlParser.
         */
        public ProcessXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            SAXParseVisitorService vService =
                (SAXParseVisitorService) getVisitorService();
            BPELParseContext context = vService.getBPELParseContext();
            if (Namespaces.isBPELElement(Import.TAG, uri, localName, qName)) {
                Import newImport = getXmlDocument().createImport();
                ((BPELProcess) getCurrentXMLNode()).addImport(newImport);
                newImport.setQualifiedName(qName);
                newImport.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(newImport, attributes);
                newImport.accept(getVisitor());
            } else if (Namespaces.isBPELElement(PartnerLinks.TAG, uri, localName, qName)) {
                
                PartnerLinks tPartners = getXmlDocument().createPartnerLinks();
                ((BPELProcess) getCurrentXMLNode()).setPartnerLinks(tPartners);
                tPartners.setQualifiedName(qName);
                tPartners.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tPartners, attributes);
                tPartners.accept(getVisitor());
            } else if(!((SAXParseVisitorService) getVisitorService()).isLoadOnlyPartnersAndImports()) {
                
                if (Namespaces.isBPELElement(Variables.TAG, uri, localName, qName)) {
                    Variables tContainers = getXmlDocument().createVariables();
                    ((BPELProcess) getCurrentXMLNode()).setVariables(tContainers);
                    tContainers.setQualifiedName(qName);
                    tContainers.setLocator(getParserSupport().getLocator());
                    SAXParserSupport.setAttributes(tContainers, attributes);
                    tContainers.accept(getVisitor());
                } else if (Namespaces.isBPELElement(CorrelationSets.TAG, uri, localName, qName)) {
                    CorrelationSets tCorrelationSets =
                            getXmlDocument().createCorrelationSets();
                    ((BPELProcess) getCurrentXMLNode()).setCorrelationSets(tCorrelationSets);
                    tCorrelationSets.setQualifiedName(qName);
                    tCorrelationSets.setLocator(getParserSupport().getLocator());
                    SAXParserSupport.setAttributes(tCorrelationSets, attributes);
                    tCorrelationSets.accept(getVisitor());
                } else if (Namespaces.isBPELElement(FaultHandlers.TAG, uri, localName, qName)) {
                    FaultHandlers tFaultHandlers = getXmlDocument()
                    .createFaultHandlers();
                    ((BPELProcess) getCurrentXMLNode()).setFaultHandlers(tFaultHandlers);
                    tFaultHandlers.setQualifiedName(qName);
                    tFaultHandlers.setLocator(getParserSupport().getLocator());
                    SAXParserSupport.setAttributes(tFaultHandlers, attributes);
                    tFaultHandlers.accept(getVisitor());
                } else if (Namespaces.isBPELElement(CompensationHandler.TAG, uri, localName, qName)) {
                    CompensationHandler tCompensationHandler =
                            getXmlDocument().createCompensationHandler();
                    ((BPELProcess) getCurrentXMLNode()).setCompensationHandler(tCompensationHandler);
                    tCompensationHandler.setQualifiedName(qName);
                    tCompensationHandler.setLocator(getParserSupport().getLocator());
                    SAXParserSupport.setAttributes(tCompensationHandler, attributes);
                    tCompensationHandler.accept(getVisitor());
                }else if (Namespaces.isBPELElement(EventHandlers.TAG, uri, localName, qName)) {
                    EventHandlers tEventHandlers =
                        getXmlDocument().createEventHandlers();
                    ((BPELProcess) getCurrentXMLNode()).setEventHandlers(tEventHandlers);
                    tEventHandlers.setQualifiedName(qName);
                    tEventHandlers.setLocator(getParserSupport().getLocator());
                    SAXParserSupport.setAttributes(tEventHandlers, attributes);
                    tEventHandlers.accept(getVisitor());
                } else if(addDocumentationElement(getXmlDocument(), qName, attributes, getParserSupport().getLocator())) {
                    // captured
                } else {
                    readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
                }
            }
            
            
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(BPELProcess.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                mLogger.warning("ProcessXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for partners element.
     */
    protected class PartnerLinksXmlParser extends XmlParser {
        
        /** Constructs a PartnersXmlParser.
         */
        public PartnerLinksXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(PartnerLink.TAG, uri, localName, qName)) {
                PartnerLink tPartner = getXmlDocument().createPartnerLink();
                ((PartnerLinks) getCurrentXMLNode()).addPartnerLink(tPartner);
                tPartner.setQualifiedName(qName);
                tPartner.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tPartner, attributes);
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(PartnerLink.TAG, uri, localName, qName)) {
            } else if (Namespaces.isBPELElement(PartnerLinks.TAG, uri, localName, qName)) {
                Collection pLinks = ((PartnerLinks) getCurrentXMLNode()).getPartnerLinks();
                for (Object pLink : pLinks) {
                    ((RPartnerLinkImpl) pLink).setAssociatedScope(
                            (BPELProcessOrScope) getCurrentXMLNode().getParent());
                }
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;

                mLogger.warning("PartnersXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for Variables element.
     */
    protected class VariablesXmlParser extends XmlParser {
        
        /** Constructs a ContainersXmlParser.
         */
        public VariablesXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(Variable.TAG, uri, localName, qName)) {
                Variable tContainer = getXmlDocument().createVariable();
                ((Variables) getCurrentXMLNode()).addVariable(tContainer);
                tContainer.setQualifiedName(qName);
                tContainer.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tContainer, attributes);
                SAXParseVisitorService vService =
                    (SAXParseVisitorService) getVisitorService();
                BPELParseContext context = vService.getBPELParseContext();
                //Register deferred initialization
                context.getDeferredActionRegistry()
                    .getActionAccepterCollector().add(tContainer);
                tContainer.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(Variables.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("ContainersXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for container element.
     */
    protected class VariableXmlParser extends XmlParser {
        
        /** Constructs a ContainerXmlParser
         */
        public VariableXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(Variable.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("ContainerXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for sequence element.
     */
    protected class SequenceXmlParser extends XmlParser {
        
        /** Constructs a SequenceXmlParser.
         */
        public SequenceXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Sequence.TAG, uri, localName, qName)) {
            	/* Do the set string before the popXmlParser() call. Once the parser is popped the current 
            	 * element is changed. So if we had a sequence (seq2) directly inside  a sequence (seq1) 
            	 * and we pop the parser when the endElement of seq2 is called, then the current element 
            	 * is changed to that of seq1. So if the line ((BaseElementImpl) getCurrentXMLNode()).setString()
            	 * is after popParser, then the toString() method for seq2 will show the element as seq1. Note 
            	 * that this happens only when we have situations when an element is directly embedded inside
            	 * an element of the same type. For example, flow within flow, if within if, etc.
            	 */
                ((BaseElementImpl) getCurrentXMLNode()).setString();
                getParserSupport().popXmlParser();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("SequenceXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for flow element.
     */
    protected class FlowXmlParser extends XmlParser {
        
        /** Constructs a FlowXmlParser.
         */
        public FlowXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (Namespaces.isBPELElement(Links.TAG, uri, localName, qName)) {
                Links tLinks = getXmlDocument().createLinks();
                ((Flow) getCurrentXMLNode()).setLinks(tLinks);
                tLinks.setQualifiedName(qName);
                tLinks.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tLinks, attributes);
                tLinks.accept(getVisitor());
            } else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Flow.TAG, uri, localName, qName)) {
            	/* Do the set string before the popXmlParser() call. Once the parser is popped the current 
            	 * element is changed. So if we had a sequence (seq2) directly inside  a sequence (seq1) 
            	 * and we pop the parser when the endElement of seq2 is called, then the current element 
            	 * is changed to that of seq1. So if the line ((BaseElementImpl) getCurrentXMLNode()).setString()
            	 * is after popParser, then the toString() method for seq2 will show the element as seq1. Note 
            	 * that this happens only when we have situations when an element is directly embedded inside
            	 * an element of the same type. For example, flow within flow, if within if, etc.
            	 */            	
                ((BaseElementImpl) getCurrentXMLNode()).setString();
                getParserSupport().popXmlParser();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("FlowXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for correlationSets element.
     */
    protected class CorrelationSetsXmlParser extends XmlParser {
        
        /** Constructs a CorrelationSetsXmlParser.
         */
        public CorrelationSetsXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(CorrelationSet.TAG, uri, localName, qName)) {
                CorrelationSet tCorrelationSet = getXmlDocument().createCorrelationSet();
                ((CorrelationSets) getCurrentXMLNode()).addCorrelationSet(tCorrelationSet);
                
                tCorrelationSet.setQualifiedName(qName);
                tCorrelationSet.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCorrelationSet, attributes);
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(CorrelationSet.TAG, uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(CorrelationSets.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("CorrelationSetsXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for faultHandlers element.
     */
    protected class FaultHandlersXmlParser extends XmlParser {
        
        /** Constructs a FaultHandlersXmlParser
         */
        public FaultHandlersXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(Catch.TAG, uri, localName, qName)) {
                Catch tCatch = getXmlDocument().createCatch();
                ((FaultHandlers) getCurrentXMLNode()).addCatch(tCatch);
                tCatch.setQualifiedName(qName);
                tCatch.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCatch, attributes);
                SAXParseVisitorService vService =
                    (SAXParseVisitorService) getVisitorService();
                BPELParseContext context = vService.getBPELParseContext();
                tCatch.initMembersLocally();
                //Register deferred initialization
                context.getDeferredActionRegistry()
                    .getActionAccepterCollector().add(tCatch);
                tCatch.accept(getVisitor());
            } else if (Namespaces.isBPELElement(CatchAll.TAG, uri, localName, qName)) {
                CatchAll tCatchAll = getXmlDocument().createCatchAll();
                ((FaultHandlers) getCurrentXMLNode()).setCatchAll(tCatchAll);
                tCatchAll.setQualifiedName(qName);
                tCatchAll.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCatchAll, attributes);
                tCatchAll.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(FaultHandlers.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("FaultHandlersXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for catch element.
     */
    protected class CatchXmlParser extends XmlParser {
        
        /** Constructs a CatchXmlParser.
         */
        public CatchXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
 
            if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else {
            	readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            
            if (Namespaces.isBPELElement(Catch.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("CatchXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for catchAll element.
     */
    protected class CatchAllXmlParser extends XmlParser {
        
        /** Constructs a CatchAllXmlParser.
         */
        public CatchAllXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {

            if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else {
            	readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(CatchAll.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("CatchAllXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for compensationHandler element.
     */
    protected class CompensationHandlerXmlParser extends XmlParser {
        
        /** Constructs a CompensationHandlerXmlParser.
         */
        public CompensationHandlerXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(CompensationHandler.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning(
                        "CompensationHandlerXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for terminationHandler element.
     */
    protected class TerminationHandlerXmlParser extends XmlParser {
        
        /** Constructs a TerminationHandlerXmlParser.
         */
        public TerminationHandlerXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(TerminationHandler.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning(
                        "TerminationHandlerXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for receive element.
     */
    protected class ReceiveXmlParser extends XmlParser {
        
        /** Constructs a ReceiveXmlParser.
         */
        public ReceiveXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (Namespaces.isBPELElement(Correlations.TAG, uri, localName, qName)) {
                Correlations tCorrelations = getXmlDocument().createCorrelations();
                ((Receive) getCurrentXMLNode()).setCorrelations(tCorrelations);
                tCorrelations.setQualifiedName(qName);
                tCorrelations.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCorrelations, attributes);
                tCorrelations.accept(getVisitor());
            }else if(Namespaces.isBPELElement(FromPart.TAG, uri, localName, qName)) {
                FromPart tFromPart = getXmlDocument().createFromPart();
                ((Receive) getCurrentXMLNode()).setFromPart(tFromPart);
                tFromPart.setQualifiedName(qName);
                tFromPart.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tFromPart, attributes);
                tFromPart.accept(getVisitor());                
      
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Receive.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("ReceiveXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for correlations element.
     */
    protected class CorrelationsXmlParser extends XmlParser {
        
        /** Constructs a CorrelationsXmlParser.
         */
        public CorrelationsXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(Correlation.TAG, uri, localName, qName)) {
                Correlation tCorrelation = getXmlDocument().createCorrelation();
                ((Correlations) getCurrentXMLNode()).addCorrelation(tCorrelation);
                tCorrelation.setQualifiedName(qName);
                tCorrelation.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCorrelation, attributes);
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(Correlation.TAG, uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Correlations.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("CorrelationsXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for links element.
     */
    protected class LinksXmlParser extends XmlParser {
        
        /** Constructs a LinksXmlParser.
         */
        public LinksXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(Link.TAG, uri, localName, qName)) {
                Link tLink = getXmlDocument().createLink();
                ((Links) getCurrentXMLNode()).addLink(tLink);
                tLink.setQualifiedName(qName);
                tLink.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tLink, attributes);
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(Link.TAG, uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Links.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("LinksXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
        /** Implements XML parser for If element.
     */
    protected class IfXmlParser extends XmlParser {
        
        /** Constructs a IfXmlParser.
         */
        public IfXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if(Namespaces.isBPELElement(Condition.TAG, uri, localName, qName)) {
                Condition tCondition = getXmlDocument().createCondition();
                ((If) getCurrentXMLNode()).setBPELCondition(tCondition);
                tCondition.setQualifiedName(qName);
                tCondition.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCondition, attributes);
                tCondition.accept(getVisitor());
            } else if(Namespaces.isBPELElement(ElseIf.TAG, uri, localName, qName)) {
                ElseIf tElseIf = getXmlDocument().createElseIf();
                ((If) getCurrentXMLNode()).addElseIf(tElseIf);
                tElseIf.setQualifiedName(qName);
                tElseIf.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tElseIf, attributes);
                tElseIf.accept(getVisitor());
            } else if(Namespaces.isBPELElement(Else.TAG, uri, localName, qName)) {
                Else tElse = getXmlDocument().createElse();
                ((If) getCurrentXMLNode()).setElse(tElse);
                tElse.setQualifiedName(qName);
                tElse.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tElse, attributes);
                tElse.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(If.TAG, uri, localName, qName)) {
            	/* Do the set string before the popXmlParser() call. Once the parser is popped the current 
            	 * element is changed. So if we had a sequence (seq2) directly inside  a sequence (seq1) 
            	 * and we pop the parser when the endElement of seq2 is called, then the current element 
            	 * is changed to that of seq1. So if the line ((BaseElementImpl) getCurrentXMLNode()).setString()
            	 * is after popParser, then the toString() method for seq2 will show the element as seq1. Note 
            	 * that this happens only when we have situations when an element is directly embedded inside
            	 * an element of the same type. For example, flow within flow, if within if, etc.
            	 */            	            	
                ((BaseElementImpl) getCurrentXMLNode()).setString();
                getParserSupport().popXmlParser();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("IfXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for If element.
     */
    protected class ElseIfXmlParser extends XmlParser {
        
        /** Constructs a IfXmlParser.
         */
        public ElseIfXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
           if(Namespaces.isBPELElement(Condition.TAG, uri, localName, qName)) {
                Condition tCondition = getXmlDocument().createCondition();
                ((ElseIf) getCurrentXMLNode()).setBPELCondition(tCondition);
                tCondition.setQualifiedName(qName);
                tCondition.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCondition, attributes);
                tCondition.accept(getVisitor());
            } else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(ElseIf.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("IfXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
            
            /*ElseIf elseIf = (ElseIf)getCurrentXMLNode();
            String expr = elseIf.getCondition();
            
            if (Utility.isEmpty(expr) || expr.equals(".")) {
                return;
            }
            try {
                elseIf.setXPathVariablesList(Utility.getXPathExpressionList(expr));
            } catch (Exception e) {
                throw new SAXException("Exception while parsing the expression :" + expr);
            }*/
        }
    }
    
    
    /** Implements XML parser for If element.
     */
    protected class ElseXmlParser extends XmlParser {
        
        /** Constructs a IfXmlParser.
         */
        public ElseXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Else.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("IfXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for switch element.
     */
    protected class SwitchXmlParser extends XmlParser {
        
        /** Constructs a SwitchXmlParser.
         */
        public SwitchXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Case.TAG, uri, localName, qName)) {
                Case tCase = getXmlDocument().createCase();
                ((Switch) getCurrentXMLNode()).addCase(tCase);
                tCase.setQualifiedName(qName);
                tCase.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCase, attributes);
                tCase.accept(getVisitor());
            } else if (Namespaces.isBPELElement(Otherwise.TAG, uri, localName, qName)) {
                Otherwise tOtherwise = getXmlDocument().createOtherwise();
                ((Switch) getCurrentXMLNode()).setOtherwise(tOtherwise);
                tOtherwise.setQualifiedName(qName);
                tOtherwise.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tOtherwise, attributes);
                tOtherwise.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Switch.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("SwitchXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for case element.
     */
    protected class CaseXmlParser extends XmlParser {
        
        /** Constructs a CaseXmlParser.
         */
        public CaseXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(Condition.TAG, uri, localName, qName)) {
                Condition tCondition = getXmlDocument().createCondition();
                ((Case) getCurrentXMLNode()).setBPELCondition(tCondition);
                tCondition.setQualifiedName(qName);
                tCondition.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCondition, attributes);
                tCondition.accept(getVisitor());
            } else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(Case.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("CaseXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
            /*String expr = ((Case) getCurrentXMLNode()).getCondition();
            if (Utility.isEmpty(expr) || expr.equals(".")) {
                return;
            }
            try {
                ((Case) getCurrentXMLNode()).setXPathVariablesList(Utility.getXPathExpressionList(expr));
            } catch (Exception e) {
                throw new SAXException("Exception while parsing the expression :" + expr);
            }*/
        }
    }
    
    
    
    /** Implements XML parser for otherwise element.
     */
    protected class OtherwiseXmlParser extends XmlParser {
        
        /** Constructs a OtherwiseXmlParser.
         */
        public OtherwiseXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(Otherwise.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("OtherwiseXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }

    /** Implements XML parser for assign element.
     */
    protected class AssignXmlParser extends XmlParser {
        
        /** Constructs a AssignXmlParser.
         */
        public AssignXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Copy.TAG, uri, localName, qName)) {
                Copy tCopy = getXmlDocument().createCopy();
                ((Assign) getCurrentXMLNode()).addCopy(tCopy);
                tCopy.setQualifiedName(qName);
                tCopy.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCopy, attributes);
                tCopy.accept(getVisitor());
            } else if (Namespaces.isBPELElement(ExtensionAssignOperation.TAG, uri, localName, qName)) {
                ExtensionAssignOperation tExtensionAssignOperation = getXmlDocument().createExtensionAssignOperation();
                ((Assign) getCurrentXMLNode()).setExtensionAssignOperation(tExtensionAssignOperation);
                tExtensionAssignOperation.setQualifiedName(qName);
                tExtensionAssignOperation.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tExtensionAssignOperation, attributes);
                tExtensionAssignOperation.accept(getVisitor());
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (Namespaces.isSbynBPELExtnElement(ForEach.TAG, uri, localName, qName)) {
                ForEach tForEach = getXmlDocument().createForEachSBYN();
                ((Assign) getCurrentXMLNode()).addForEach(tForEach);
                tForEach.setQualifiedName(qName);
                tForEach.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tForEach, attributes);
                tForEach.accept(getVisitor());
            } else if (Namespaces.isSbynBPELRuntimeExtnElement(Choose.TAG, uri, localName, qName)) {
                Choose tChoose = getXmlDocument().createChoose();
                ((Assign) getCurrentXMLNode()).addChoose(tChoose);
                tChoose.setQualifiedName(qName);
                tChoose.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tChoose, attributes);
                tChoose.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)
            && !Namespaces.isSbynBPELExtnElement(localName, uri, localName, qName)
            && !Namespaces.isSbynBPELRuntimeExtnElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Assign.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("AssignXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for copy element.
     */
    protected class CopyXmlParser extends XmlParser {
        
        /** Constructs a CopyXmlParser.
         */
        public CopyXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(From.TAG, uri, localName, qName)) {
                From tFrom = getXmlDocument().createFrom();
                ((Copy) getCurrentXMLNode()).setFrom(tFrom);
                tFrom.setQualifiedName(qName);
                tFrom.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tFrom.getXmlAttributes(), tFrom,
                        attributes);
                tFrom.accept(getVisitor());
            } else if (Namespaces.isBPELElement(To.TAG, uri, localName, qName)) {
                To tTo = getXmlDocument().createTo();
                ((Copy) getCurrentXMLNode()).setTo(tTo);
                tTo.setQualifiedName(qName);
                tTo.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tTo.getXmlAttributes(), tTo,
                        attributes);
                
                tTo.accept(getVisitor());
                
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(To.TAG, uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Copy.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("CopyXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for from element.
     */
    protected class FromXmlParser extends XmlParser {
        
        /** Constructs a FromXmlParser.
         */
        public FromXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(Literal.TAG, uri, localName, qName)) {
                Literal tLiteral = getXmlDocument().createLiteral();
                ((From) getCurrentXMLNode()).setLiteral(tLiteral);
                tLiteral.setQualifiedName(qName);
                tLiteral.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tLiteral, attributes);
                tLiteral.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(), uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
        throws SAXException {
            String content = new String(ch, start, length);
            if (content.length() == 0) {
                return;
            }
            XMLText expression = ((From) getCurrentXMLNode()).getExpressionText();
            if (null == expression) {
                expression = getXmlDocument().createXmlText();
                ((From) getCurrentXMLNode()).setExpressionText(expression);
            }
            String val = expression.getValue();
            if (null == val) {
                expression.setValue(content);
            } else {
                expression.setValue(val.concat(content));
            }
            expression.setLocator(getParserSupport().getLocator());
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(From.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                BaseElementImpl elem = (BaseElementImpl) getCurrentXMLNode(); 
                elem.setString();
                List children = elem.getChildren();
                if (children != null && children.size() == 1) { // one Text child
                    if (children.get(0) instanceof XMLText) {
                        String xpath = ((XMLText) children.get(0)).getValue();
                        Matcher m = Pattern.compile(
                                "doXslTransform[\\s]*\\([^,]*,").matcher(xpath);
                        while (m.find()) {
                            // parse stylesheet param from fxn call
                            String fxn = m.group(), xsl = "";
                            int open = fxn.indexOf("("), 
                                sgl = fxn.indexOf("'", open),
                                dbl = fxn.indexOf("\"", open);
                            if ((dbl >= 0 && sgl > dbl) || sgl <= 0) {
                                xsl = fxn.substring(dbl + 1, fxn.indexOf("\"", dbl + 1));
                            }
                            else {
                                xsl = fxn.substring(sgl + 1, fxn.indexOf("'", sgl + 1));
                            }
                            
                            // store by file's absolute path
                            try {
                                ParsingCaches caches =                                 
                                        DefaultXmlParserFactory.this.getXmlDocument()
                                            .getBPELParseContext().getCaches();
                                xsl = Utility.parseDoXslTransform(
                                        xsl, caches.getCurrentBaseURI());
                                caches.getXslCache().add(xsl);
                            }
                            catch (Exception e) {
                                throw new SAXException(I18n.loc(
                                        "BPMOD-6005: XSL stylesheet {0} could not be loaded: {1}", 
                                        xsl, e.getMessage()));
                            }
                        }
                    }
                }
            } 
        }
    }
    
    /** Visits a text section.
     * @param   t   XML text section
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(XMLText t) {
        return false;
    }
    
    
    
    /** Implements XML parser for To element.
     */
    protected class ToXmlParser extends XmlParser {
        
        /** Constructs a ToXmlParser.
         */
        public ToXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(), uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
                throws SAXException {
            String content = new String(ch, start, length);
            if (content.length() == 0) {
                return;
            }

            XMLText expression = ((To) getCurrentXMLNode()).getQueryText();
            if (null == expression) {
                expression = getXmlDocument().createXmlText();
                ((To) getCurrentXMLNode()).setQueryText(expression);
            }
            String val = expression.getValue();
            if (null == val) {
                expression.setValue(content);
            } else {
                expression.setValue(val.concat(content));
            }
            expression.setLocator(getParserSupport().getLocator());
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(To.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            }
        }
    }
    
    /** Implements XML parser for extensionAssignOperation element.
     */
    protected class ExtensionAssignOperationElementParser extends XmlParser {
        
        /** Constructs a ExtensionAssignOperationElementParser.
         */
        public ExtensionAssignOperationElementParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isSunBPELExtnDataHandler(SunExtExpression.TAG, uri, localName, qName)) {
                SunExtExpression tSunExtExpression = getXmlDocument().createSunExtExpression();
                ((ExtensionAssignOperation) getCurrentXMLNode()).setSunExtExpression(tSunExtExpression);
                tSunExtExpression.setQualifiedName(qName);
                tSunExtExpression.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tSunExtExpression, attributes);
                tSunExtExpression.accept(getVisitor());
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
                throws SAXException {
            if (Namespaces.isBPELElement(ExtensionAssignOperation.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if (("import".equalsIgnoreCase(localName))
                        || ("documentation".equalsIgnoreCase(localName)))
                    return;

                mLogger.warning("FromXmlParser.endElement(): "
                        + mMsg.getString(UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    

    
    /** Implements XML parser for reply element.
     */
    protected class ReplyXmlParser extends XmlParser {
        
        /** Constructs a ReplyXmlParser.
         */
        public ReplyXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (Namespaces.isBPELElement(Correlations.TAG, uri, localName, qName)) {
                Correlations tCorrelations = getXmlDocument().createCorrelations();
                ((Reply) getCurrentXMLNode()).setCorrelations(tCorrelations);
                tCorrelations.setQualifiedName(qName);
                tCorrelations.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCorrelations, attributes);
                tCorrelations.accept(getVisitor());
            } else if(Namespaces.isBPELElement(ToPart.TAG, uri, localName, qName)) {
                ToPart tToPart = getXmlDocument().createToPart();
                ((Reply) getCurrentXMLNode()).setToPart(tToPart);
                tToPart.setQualifiedName(qName);
                tToPart.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tToPart, attributes);
                tToPart.accept(getVisitor());                
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Reply.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("ReplyXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    
    /** Implements XML parser for invoke element.
     */
    protected class InvokeXmlParser extends XmlParser {
        
        /** Constructs a InvokeXmlParser.
         */
        public InvokeXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (Namespaces.isBPELElement(Correlations.TAG, uri, localName, qName)) {
                Correlations tCorrelations = getXmlDocument().createCorrelations();
                ((Invoke) getCurrentXMLNode()).setCorrelations(tCorrelations);
                tCorrelations.setQualifiedName(qName);
                tCorrelations.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCorrelations, attributes);
                tCorrelations.accept(getVisitor());
            } else if (Namespaces.isBPELElement(Catch.TAG, uri, localName, qName)) {
                Catch tCatch = getXmlDocument().createCatch();
                ((Invoke) getCurrentXMLNode()).addCatch(tCatch);
                tCatch.setQualifiedName(qName);
                tCatch.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCatch, attributes);
                tCatch.accept(getVisitor());
            } else if (Namespaces.isBPELElement(CatchAll.TAG, uri, localName, qName)) {
                CatchAll tCatchAll = getXmlDocument().createCatchAll();
                ((Invoke) getCurrentXMLNode()).setCatchAll(tCatchAll);
                tCatchAll.setQualifiedName(qName);
                tCatchAll.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCatchAll, attributes);
                tCatchAll.accept(getVisitor());
            } else if (Namespaces.isBPELElement(CompensationHandler.TAG, uri, localName, qName)) {
                CompensationHandler tCompensationHandler = getXmlDocument().createCompensationHandler();
                ((Invoke) getCurrentXMLNode()).setCompensationHandler(tCompensationHandler);
                tCompensationHandler.setQualifiedName(qName);
                tCompensationHandler.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCompensationHandler, attributes);
                tCompensationHandler.accept(getVisitor());
            } else if(Namespaces.isBPELElement(FromPart.TAG, uri, localName, qName)) {
                FromPart tFromPart = getXmlDocument().createFromPart();
                ((Invoke) getCurrentXMLNode()).setFromPart(tFromPart);
                tFromPart.setQualifiedName(qName);
                tFromPart.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tFromPart, attributes);
                tFromPart.accept(getVisitor());
            } else if(Namespaces.isBPELElement(ToPart.TAG, uri, localName, qName)) {
                ToPart tToPart = getXmlDocument().createToPart();
                ((Invoke) getCurrentXMLNode()).setToPart(tToPart);
                tToPart.setQualifiedName(qName);
                tToPart.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tToPart, attributes);
                tToPart.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Invoke.TAG, uri, localName, qName)) {
                
                // Expand invoke if it has inline compensation and/or fault handler
                XMLNode parent = getCurrentXMLNode().getParent();
                Scope convScope = getParserSupport().expandInvoke((Invoke) getCurrentXMLNode());
                if (convScope != null) {
                    if (parent instanceof SingleActivityHolder) {
                        ((SingleActivityHolder) parent).setActivity(convScope);
                    } else if (parent instanceof MultipleActivityHolder) {
                        MultipleActivityHolder mah = (MultipleActivityHolder) parent;
                        mah.setActivity(mah.indexOfActivity(getCurrentXMLNode()), convScope);
                    } else {
                        throw new EInsightModelException(
                                "Error while expanding Invoke; parent is not an Activity holder!");
                    }
                }
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("InvokeXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for throw element.
     */
    protected class ThrowXmlParser extends XmlParser {
        
        /** Constructs a ThrowXmlParser.
         */
        public ThrowXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Throw.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("ThrowXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for rethrow element.
     */
    protected class RethrowXmlParser extends XmlParser {
        
        /** Constructs a RethrowXmlParser.
         */
        public RethrowXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Rethrow.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("RethrowXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for RepeatUntil element.
     */
    protected class RepeatUntilXmlParser extends XmlParser {
        
        /** Constructs a RepeatUntilXmlParser.
         */
        public RepeatUntilXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if(Namespaces.isBPELElement(Condition.TAG, uri, localName, qName)) {
                Condition tCondition = getXmlDocument().createCondition();
                ((RepeatUntil) getCurrentXMLNode()).setBPELCondition(tCondition);
                tCondition.setQualifiedName(qName);
                tCondition.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCondition, attributes);
                tCondition.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(RepeatUntil.TAG, uri, localName, qName)) {
            	/* Do the set string before the popXmlParser() call. Once the parser is popped the current 
            	 * element is changed. So if we had a sequence (seq2) directly inside  a sequence (seq1) 
            	 * and we pop the parser when the endElement of seq2 is called, then the current element 
            	 * is changed to that of seq1. So if the line ((BaseElementImpl) getCurrentXMLNode()).setString()
            	 * is after popParser, then the toString() method for seq2 will show the element as seq1. Note 
            	 * that this happens only when we have situations when an element is directly embedded inside
            	 * an element of the same type. For example, flow within flow, if within if, etc.
            	 */            	            	
                ((BaseElementImpl) getCurrentXMLNode()).setString();
                getParserSupport().popXmlParser();            	
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("RepeatUntilXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for validate element.
     */
    protected class ValidateXmlParser extends XmlParser {
        
        /** Constructs a ValidateXmlParser.
         */
        public ValidateXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Validate.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("RethrowXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for exit element.
     */
    protected class ExitXmlParser extends XmlParser {
        
        /** Constructs a TerminateXmlParser.
         */
        public ExitXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Terminate.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("TerminateXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for trace element (sun extension).
     */
    protected class TraceSunExtXmlParser extends XmlParser {
        
        /** Constructs a TraceSunExtXmlParser.
         */
        public TraceSunExtXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
        	
        	if (Namespaces.isSunBPELExtnTraceElement(Log.TAG, uri, localName, qName)) {
        		Log tLog = getXmlDocument().createLogSunExt();
            	tLog.setQualifiedName(qName);
            	tLog.setLocator(getParserSupport().getLocator());
            	SAXParserSupport.setAttributes(tLog, attributes);
            	((Trace) getCurrentXMLNode()).addLog(tLog);
            	tLog.accept(getVisitor());
        	} else if (Namespaces.isSunBPELExtnTraceElement(Alert.TAG, uri, localName, qName)) {
        		Alert tAlert = getXmlDocument().createAlertSunExt();
            	tAlert.setQualifiedName(qName);
            	tAlert.setLocator(getParserSupport().getLocator());
            	SAXParserSupport.setAttributes(tAlert, attributes);
            	((Trace) getCurrentXMLNode()).addAlert(tAlert);
            	tAlert.accept(getVisitor());
        	} else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("TraceSunExtXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for log element (sun extension).
     */
    protected class LogSunExtXmlParser extends XmlParser {
        
        /** Constructs a LogSunExtXmlParser.
         */
        public LogSunExtXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
        	
        	if (Namespaces.isBPELElement(From.TAG, uri, localName, qName)) {
        		From tFrom = getXmlDocument().createFrom();
            	tFrom.setQualifiedName(qName);
            	tFrom.setLocator(getParserSupport().getLocator());
            	((Log) getCurrentXMLNode()).setFrom(tFrom);
            	SAXParserSupport.setAttributes(tFrom, attributes);
            	tFrom.accept(getVisitor());
        	} else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isSunBPELExtnTraceElement(Log.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("LogSunExtXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for alert element (sun extension).
     */
    protected class AlertSunExtXmlParser extends XmlParser {
        
        /** Constructs a AlertSunExtXmlParser.
         */
        public AlertSunExtXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
        	
        	if (Namespaces.isBPELElement(From.TAG, uri, localName, qName)) {
        		From tFrom = getXmlDocument().createFrom();
            	tFrom.setQualifiedName(qName);
            	tFrom.setLocator(getParserSupport().getLocator());
            	((Alert) getCurrentXMLNode()).setFrom(tFrom);
            	SAXParserSupport.setAttributes(tFrom, attributes);
            	tFrom.accept(getVisitor());
        	} else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isSunBPELExtnTraceElement(Alert.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("AlertSunExtXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    
    /** Implements XML parser for wait element.
     */
    protected class WaitXmlParser extends XmlParser {
        
        /** Constructs a WaitXmlParser.
         */
        public WaitXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(For.TAG, uri, localName, qName)) {
                For tFor = getXmlDocument().createFor();
                ((Wait) getCurrentXMLNode()).setBPELFor(tFor);
                tFor.setQualifiedName(qName);
                tFor.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tFor, attributes);
                tFor.accept(getVisitor());
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (Namespaces.isBPELElement(Until.TAG, uri, localName, qName)) {
                Until tUntil = getXmlDocument().createUntil();
                ((Wait) getCurrentXMLNode()).setBPELUntil(tUntil);
                tUntil.setQualifiedName(qName);
                tUntil.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tUntil, attributes);
                tUntil.accept(getVisitor());
            } else if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Wait.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("WaitXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for empty element.
     */
    protected class EmptyXmlParser extends XmlParser {
        
        /** Constructs a EmptyXmlParser.
         */
        public EmptyXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Empty.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("EmptyXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for while element.
     */
    protected class WhileXmlParser extends XmlParser {
        
        /** Constructs a WhileXmlParser.
         */
        public WhileXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (Namespaces.isBPELElement(Condition.TAG, uri, localName, qName)) {
                Condition tCondition = getXmlDocument().createCondition();
                ((While) getCurrentXMLNode()).setBPELCondition(tCondition);
                tCondition.setQualifiedName(qName);
                tCondition.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCondition, attributes);
                tCondition.accept(getVisitor());
            } else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(While.TAG, uri, localName, qName)) {
            	/* Do the set string before the popXmlParser() call. Once the parser is popped the current 
            	 * element is changed. So if we had a sequence (seq2) directly inside  a sequence (seq1) 
            	 * and we pop the parser when the endElement of seq2 is called, then the current element 
            	 * is changed to that of seq1. So if the line ((BaseElementImpl) getCurrentXMLNode()).setString()
            	 * is after popParser, then the toString() method for seq2 will show the element as seq1. Note 
            	 * that this happens only when we have situations when an element is directly embedded inside
            	 * an element of the same type. For example, flow within flow, if within if, etc.
            	 */            	            	
                ((BaseElementImpl) getCurrentXMLNode()).setString();
                getParserSupport().popXmlParser();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("WhileXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for while element.
     */
    protected class ForEachXmlParser extends XmlParser {
        
        /** Constructs a WhileXmlParser.
         */
        public ForEachXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (Namespaces.isBPELElement(StartCounterValue.TAG, uri, localName, qName)) {
                Iterator tIterator = ((com.sun.bpel.model.ForEach) getCurrentXMLNode()).getIterator();
                if (tIterator == null) {
                    tIterator=getXmlDocument().createIterator();
                    ((com.sun.bpel.model.ForEach) getCurrentXMLNode()).setIterator(tIterator);
                }
                StartCounterValue tStartCounterValue = getXmlDocument().createStartCounterValue();
                tIterator.setStartCounterValue(tStartCounterValue);
                tStartCounterValue.setQualifiedName(qName);
                tStartCounterValue.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tStartCounterValue, attributes);
                SAXParseVisitorService vService =
                    (SAXParseVisitorService) getVisitorService();
                BPELParseContext context = vService.getBPELParseContext();
                ((com.sun.bpel.model.ForEach) getCurrentXMLNode()).initMembersLocally();
                //Register deferred initialization
                context.getDeferredActionRegistry()
                    .getActionAccepterCollector().add((com.sun.bpel.model.ForEach) getCurrentXMLNode());
                tStartCounterValue.accept(getVisitor());
            } else if (Namespaces.isBPELElement(FinalCounterValue.TAG, uri, localName, qName)) {
                Iterator tIterator = ((com.sun.bpel.model.ForEach) getCurrentXMLNode()).getIterator();
                if (tIterator == null) {
                    tIterator=getXmlDocument().createIterator();
                    ((com.sun.bpel.model.ForEach) getCurrentXMLNode()).setIterator(tIterator);
                }                
                FinalCounterValue tFinalCounterValue = getXmlDocument().createFinalCounterValue();
                tIterator.setFinalCounterValue(tFinalCounterValue);
                tFinalCounterValue.setQualifiedName(qName);
                tFinalCounterValue.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tFinalCounterValue, attributes);
                tFinalCounterValue.accept(getVisitor());
            } else if (Namespaces.isBPELElement(CompletionCondition.TAG, uri, localName, qName)) {
                CompletionCondition tCompleteCondition = getXmlDocument().createCompletionCondition();
                ((com.sun.bpel.model.ForEach)getCurrentXMLNode()).setCompletionCondition(tCompleteCondition);
                tCompleteCondition.setQualifiedName(qName);
                tCompleteCondition.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCompleteCondition, attributes);
                tCompleteCondition.accept(getVisitor());
            } else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(com.sun.bpel.model.ForEach.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else{
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("ForEachXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
       
        }

    }    
    
    /** Implements XML parser for while element.
     */
    protected class CompleteConditionParser extends XmlParser {
        
        /** Constructs a WhileXmlParser.
         */
        public CompleteConditionParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(Branches.TAG, uri, localName, qName)) {
                CompletionCondition tCompleteCondition = (CompletionCondition)getCurrentXMLNode();
                Branches tBranches =  getXmlDocument().createBranches();
                tCompleteCondition.setBranches(tBranches);
                tBranches.setQualifiedName(qName);
                tBranches.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tBranches, attributes);
                tBranches.accept(getVisitor());                
            } else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(CompletionCondition.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else{
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("ForEachXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
       
        }
    }    
        
    
    /** Implements XML parser for pick element.
     */
    protected class PickXmlParser extends XmlParser {
        
        /** Constructs a PickXmlParser.
         */
        public PickXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (Namespaces.isBPELElement(OnMessage.TAG, uri, localName, qName)) {
                OnMessage tOnMessage = getXmlDocument().createOnMessage();
                ((Pick) getCurrentXMLNode()).addOnMessage(tOnMessage);
                tOnMessage.setQualifiedName(qName);
                tOnMessage.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tOnMessage, attributes);
                tOnMessage.accept(getVisitor());
            } else if (Namespaces.isBPELElement(OnAlarm.TAG, uri, localName, qName)) {
                OnAlarm tOnAlarm = getXmlDocument().createOnAlarm();
                ((Pick) getCurrentXMLNode()).addOnAlarm(tOnAlarm);
                tOnAlarm.setQualifiedName(qName);
                tOnAlarm.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tOnAlarm, attributes);
                tOnAlarm.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Pick.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("PickXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for onMessage element.
     */
    protected class OnMessageXmlParser extends XmlParser {
        
        /** Constructs a OnMessageXmlParser.
         */
        public OnMessageXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(Correlations.TAG, uri, localName, qName)) {
                Correlations tCorrelations = getXmlDocument().createCorrelations();
                ((OnMessage) getCurrentXMLNode()).setCorrelations(tCorrelations);
                tCorrelations.setQualifiedName(qName);
                tCorrelations.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCorrelations, attributes);
                tCorrelations.accept(getVisitor());
            } else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(OnMessage.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("OnMessageXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for onAlarm element.
     */
    protected class OnAlarmXmlParser extends XmlParser {
        
        /** Constructs a OnAlarmXmlParser.
         */
        public OnAlarmXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(For.TAG, uri, localName, qName)) {
                For tFor = getXmlDocument().createFor();
                ((OnAlarm) getCurrentXMLNode()).setBPELFor(tFor);
                tFor.setQualifiedName(qName);
                tFor.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tFor, attributes);
                tFor.accept(getVisitor());
            } else if (Namespaces.isBPELElement(Until.TAG, uri, localName, qName)) {
                Until tUntil = getXmlDocument().createUntil();
                ((OnAlarm) getCurrentXMLNode()).setBPELUntil(tUntil);
                tUntil.setQualifiedName(qName);
                tUntil.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tUntil, attributes);
                tUntil.accept(getVisitor());
            } else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(OnAlarm.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("OnAlarmXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for scope element.
     */
    protected class ScopeXmlParser extends XmlParser {
        
        /** Constructs a ScopeXmlParser.
         */
        public ScopeXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (Namespaces.isSunBPELExtnTraceElement(Trace.TAG, uri, localName, qName)) {
            	Trace tTrace = getXmlDocument().createTraceSunExt();
            	tTrace.setQualifiedName(qName); 
            	tTrace.setLocator(getParserSupport().getLocator());
            	((BPELElement) getCurrentXMLNode()).setTrace(tTrace);
            	tTrace.accept(getVisitor());
            } else if (Namespaces.isBPELElement(PartnerLinks.TAG, uri, localName, qName)) {
                PartnerLinks tPartners = getXmlDocument().createPartnerLinks();
                ((Scope) getCurrentXMLNode()).setPartnerLinks(tPartners);
                tPartners.setQualifiedName(qName);
                tPartners.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tPartners, attributes);
                tPartners.accept(getVisitor());
            } else if (Namespaces.isBPELElement(Variables.TAG, uri, localName, qName)) {
                Variables tContainers =((Scope) getCurrentXMLNode()).getVariables();
                if (tContainers == null) {
                     tContainers = getXmlDocument().createVariables();
                    ((Scope) getCurrentXMLNode()).setVariables(tContainers);
                }
                tContainers.setQualifiedName(qName);
                tContainers.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tContainers, attributes);
                tContainers.accept(getVisitor());
            } else if (Namespaces.isBPELElement(CorrelationSets.TAG, uri, localName, qName)) {
                CorrelationSets tCorrelationSets =
                        getXmlDocument().createCorrelationSets();
                ((Scope) getCurrentXMLNode()).setCorrelationSets(tCorrelationSets);
                tCorrelationSets.setQualifiedName(qName);
                tCorrelationSets.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCorrelationSets, attributes);
                tCorrelationSets.accept(getVisitor());
            } else if (Namespaces.isBPELElement(FaultHandlers.TAG, uri, localName, qName)) {
                FaultHandlers tFaultHandlers = getXmlDocument().createFaultHandlers();
                ((Scope) getCurrentXMLNode()).setFaultHandlers(tFaultHandlers);
                tFaultHandlers.setQualifiedName(qName);
                tFaultHandlers.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tFaultHandlers, attributes);
                tFaultHandlers.accept(getVisitor());
            } else if (Namespaces.isBPELElement(CompensationHandler.TAG, uri, localName, qName)) {
                CompensationHandler tCompensationHandler = getXmlDocument().createCompensationHandler();
                ((Scope) getCurrentXMLNode()).setCompensationHandler(tCompensationHandler);
                tCompensationHandler.setQualifiedName(qName);
                tCompensationHandler.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCompensationHandler, attributes);
                tCompensationHandler.accept(getVisitor());
            } else if (Namespaces.isBPELElement(TerminationHandler.TAG, uri, localName, qName)) {
                TerminationHandler tTerminationHandler = getXmlDocument().createTerminationHandler();
                ((Scope) getCurrentXMLNode()).setTerminationHandler(tTerminationHandler);
                tTerminationHandler.setQualifiedName(qName);
                tTerminationHandler.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tTerminationHandler, attributes);
                tTerminationHandler.accept(getVisitor());
            } else if (Namespaces.isBPELElement(EventHandlers.TAG, uri, localName, qName)) {
                EventHandlers tEventHandlers =
                    getXmlDocument().createEventHandlers();
                ((Scope) getCurrentXMLNode()).setEventHandlers(tEventHandlers);
                tEventHandlers.setQualifiedName(qName);
                tEventHandlers.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tEventHandlers, attributes);
                tEventHandlers.accept(getVisitor());
            }else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Scope.TAG, uri, localName, qName)) {
            	/* Do the set string before the popXmlParser() call. Once the parser is popped the current 
            	 * element is changed. So if we had a sequence (seq2) directly inside  a sequence (seq1) 
            	 * and we pop the parser when the endElement of seq2 is called, then the current element 
            	 * is changed to that of seq1. So if the line ((BaseElementImpl) getCurrentXMLNode()).setString()
            	 * is after popParser, then the toString() method for seq2 will show the element as seq1. Note 
            	 * that this happens only when we have situations when an element is directly embedded inside
            	 * an element of the same type. For example, flow within flow, if within if, etc.
            	 */            	            	
                ((BaseElementImpl) getCurrentXMLNode()).setString();
                getParserSupport().popXmlParser();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("ScopeXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for compensate element.
     */
    protected class CompensateXmlParser extends XmlParser {
        
        /** Constructs a CompensateXmlParser.
         */
        public CompensateXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(Compensate.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("CompensateXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }

    /** Implements XML parser for compensateScope element.
     */
    protected class CompensateScopeXmlParser extends XmlParser {
        
        /** Constructs a CompensateXmlParser.
         */
        public CompensateScopeXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (addStandardElement((Activity) getCurrentXMLNode(), uri, localName, qName,
                    attributes, getParserSupport().getLocator())) {
                // action taken already in above test
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (endStandardElement(uri, localName, qName)) {
                // action taken already in above test
            } else if (Namespaces.isBPELElement(CompensateScope.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("CompensateScopeXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }

    /** Implements XML parser for forEach element.
     */
    protected class ForEachSBYNXmlParser extends XmlParser {
        
        /** Constructs a ForEachXmlParser.
         */
        public ForEachSBYNXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(Copy.TAG, uri, localName, qName)) {
                Copy tCopy = getXmlDocument().createCopy();
                ((ForEach) getCurrentXMLNode()).addCopy(tCopy);
                tCopy.setQualifiedName(qName);
                tCopy.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCopy, attributes);
                tCopy.accept(getVisitor());
            } else if (Namespaces.isSbynBPELExtnElement(ForEach.TAG, uri, localName, qName)) {
                ForEach tForEach = getXmlDocument().createForEachSBYN();
                ((ForEach) getCurrentXMLNode()).addForEach(tForEach);
                tForEach.setQualifiedName(qName);
                tForEach.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tForEach, attributes);
                tForEach.accept(getVisitor());
            } else if (Namespaces.isSbynBPELRuntimeExtnElement(Choose.TAG, uri, localName, qName)) {
                Choose tChoose = getXmlDocument().createChoose();
                ((ForEach) getCurrentXMLNode()).addChoose(tChoose);
                tChoose.setQualifiedName(qName);
                tChoose.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tChoose, attributes);
                tChoose.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)
            && !Namespaces.isSbynBPELExtnElement(localName, uri, localName, qName)
            && !Namespaces.isSbynBPELRuntimeExtnElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isSbynBPELExtnElement(ForEach.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("ForEachXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for Choose element.
     */
    protected class ChooseXmlParser extends XmlParser {
        
        /** Constructs a ChooseXmlParser.
         */
        public ChooseXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isSbynBPELRuntimeExtnElement(When.TAG, uri, localName, qName)) {
                When tWhen = getXmlDocument().createWhen();
                ((Choose) getCurrentXMLNode()).addWhen(tWhen);
                tWhen.setQualifiedName(qName);
                tWhen.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tWhen, attributes);
                tWhen.accept(getVisitor());
            } else if (Namespaces.isSbynBPELRuntimeExtnElement(Default.TAG, uri, localName, qName)) {
                Default tDefault = getXmlDocument().createDefault();
                ((Choose) getCurrentXMLNode()).setDefault(tDefault);
                tDefault.setQualifiedName(qName);
                tDefault.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tDefault, attributes);
                tDefault.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)
            && !Namespaces.isSbynBPELExtnElement(localName, uri, localName, qName)
            && !Namespaces.isSbynBPELRuntimeExtnElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isSbynBPELRuntimeExtnElement(Choose.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("ChooseXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for When element.
     */
    protected class WhenXmlParser extends XmlParser {
        
        /** Constructs a WhenXmlParser.
         */
        public WhenXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(Copy.TAG, uri, localName, qName)) {
                Copy tCopy = getXmlDocument().createCopy();
                ((When) getCurrentXMLNode()).addCopy(tCopy);
                tCopy.setQualifiedName(qName);
                tCopy.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCopy, attributes);
                tCopy.accept(getVisitor());
            } else if (Namespaces.isSbynBPELExtnElement(ForEach.TAG, uri, localName, qName)) {
                ForEach tForEach = getXmlDocument().createForEachSBYN();
                ((When) getCurrentXMLNode()).addForEach(tForEach);
                tForEach.setQualifiedName(qName);
                tForEach.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tForEach, attributes);
                tForEach.accept(getVisitor());
            } else if (Namespaces.isSbynBPELRuntimeExtnElement(Choose.TAG, uri, localName, qName)) {
                Choose tChoose = getXmlDocument().createChoose();
                ((When) getCurrentXMLNode()).addChoose(tChoose);
                tChoose.setQualifiedName(qName);
                tChoose.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tChoose, attributes);
                tChoose.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)
            && !Namespaces.isSbynBPELExtnElement(localName, uri, localName, qName)
            && !Namespaces.isSbynBPELRuntimeExtnElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isSbynBPELRuntimeExtnElement(When.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("WhenXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
            
            /*String expr = ((When) getCurrentXMLNode()).getCondition();
            if (Utility.isEmpty(expr) || expr.equals(".")) {
                return;
            }
            try {
                ((When) getCurrentXMLNode()).setXPathVariablesList(Utility.getXPathExpressionList(expr));
            } catch (Exception e) {
                throw new SAXException("Exception while parsing the expression :" + expr);
            }*/
        }
    }
    
    /** Implements XML parser for Default element.
     */
    protected class DefaultXmlParser extends XmlParser {
        
        /** Constructs a DefaultXmlParser.
         */
        public DefaultXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(Copy.TAG, uri, localName, qName)) {
                Copy tCopy = getXmlDocument().createCopy();
                ((Default) getCurrentXMLNode()).addCopy(tCopy);
                tCopy.setQualifiedName(qName);
                tCopy.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCopy, attributes);
                tCopy.accept(getVisitor());
            } else if (Namespaces.isSbynBPELExtnElement(ForEach.TAG, uri, localName, qName)) {
                ForEach tForEach = getXmlDocument().createForEachSBYN();
                ((Default) getCurrentXMLNode()).addForEach(tForEach);
                tForEach.setQualifiedName(qName);
                tForEach.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tForEach, attributes);
                tForEach.accept(getVisitor());
            } else if (Namespaces.isSbynBPELRuntimeExtnElement(Choose.TAG, uri, localName, qName)) {
                Choose tChoose = getXmlDocument().createChoose();
                ((Default) getCurrentXMLNode()).addChoose(tChoose);
                tChoose.setQualifiedName(qName);
                tChoose.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tChoose, attributes);
                tChoose.accept(getVisitor());
            } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)
            && !Namespaces.isSbynBPELExtnElement(localName, uri, localName, qName)
            && !Namespaces.isSbynBPELRuntimeExtnElement(localName, uri, localName, qName)) {
                addExtensibilityElement(getCurrentXMLNode(),  uri, localName, qName, attributes);
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName,
                        qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isSbynBPELRuntimeExtnElement(Default.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("DefaultXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /**
     * Implements XML parser for the &lt;extensibilityElement&gt; element.
     */
    protected class ExtensibilityElementXmlParser extends XmlParser {
        
        /** Constructs a ExtensibilityElementXmlParser.
         */
        public ExtensibilityElementXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            ExtensibilityElement exten = getXmlDocument().createExtensibilityElement();
            exten.setElementType(com.sun.bpel.xml.NamespaceUtility.
            			getQNameFromURIAndString(uri, qName));
            exten.setLocator(getParserSupport().getLocator());
            SAXParserSupport.setAttributes(exten, attributes);
            ((ExtensibilityElement) getCurrentXMLNode()).addExtensibilityElement(exten);
            exten.accept(getVisitor());
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            getParserSupport().popXmlParser();
            ((BaseElementImpl) getCurrentXMLNode()).setString();
        }
    }
    
    
    /** Implements XML parser for documentation element.
     */
    protected class DocumentationXmlParser extends XmlParser {
        
        /** Constructs a DocumentationXmlParser.
         */
        public DocumentationXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
        throws SAXException {
            String val = ((Documentation) getCurrentXMLNode()).getValue();
            if (null == val) {
                ((Documentation) getCurrentXMLNode()).setValue(new String(ch, start, length));
            } else {
                ((Documentation) getCurrentXMLNode()).setValue(val.concat(new String(ch, start, length)));
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (qName.endsWith(Documentation.TAG)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.fine("DocumentationXmlParser.endElement(): "
                        + mMsg.getString(UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    
    /** Implements XML parser for Condition element.
     */
    protected class ConditionXmlParser extends XmlParser {
        
        /** Constructs a ConditionXmlParser.
         */
        public ConditionXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
        throws SAXException {
            String val = ((Condition) getCurrentXMLNode()).getValue();
            if (null == val) {
                ((Condition) getCurrentXMLNode()).setValue(new String(ch, start, length));
            } else {
                ((Condition) getCurrentXMLNode()).setValue(val.concat(new String(ch, start, length)));
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (qName.endsWith(Condition.TAG)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.fine("ConditionXmlParser.endElement(): "
                        + mMsg.getString(UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for StartCounterValue element.
     */
    protected class StartCounterValueParser extends XmlParser {
        
        /** Constructs a StartCounterValueParser.
         */
        public StartCounterValueParser() {
            super(getParserSupport());
        }
        
        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
        throws SAXException {
            String val = ((StartCounterValue) getCurrentXMLNode()).getValue();
            if (null == val) {
                ((StartCounterValue) getCurrentXMLNode()).setValue(new String(ch, start, length));
            } else {
                ((StartCounterValue) getCurrentXMLNode()).setValue(val.concat(new String(ch, start, length)));
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (qName.endsWith(StartCounterValue.TAG)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.fine("StartCounterValueParser.endElement(): "
                        + mMsg.getString(UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }    
    
    /** Implements XML parser for StartCounterValue element.
     */
    protected class FinalCounterValueParser extends XmlParser {
        
        /** Constructs a FinalCounterValueParser.
         */
        public FinalCounterValueParser() {
            super(getParserSupport());
        }
        
        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
        throws SAXException {
            String val = ((FinalCounterValue) getCurrentXMLNode()).getValue();
            if (null == val) {
                ((FinalCounterValue) getCurrentXMLNode()).setValue(new String(ch, start, length));
            } else {
                ((FinalCounterValue) getCurrentXMLNode()).setValue(val.concat(new String(ch, start, length)));
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (qName.endsWith(FinalCounterValue.TAG)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.fine("FinalCounterValueParser.endElement(): "
                        + mMsg.getString(UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }      
    /** Implements XML parser for Branches element.
     */
    protected class BranchesParser extends XmlParser {
        
        /** Constructs a BranchesParser.
         */
        public BranchesParser() {
            super(getParserSupport());
        }
        
        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
        throws SAXException {
            String val = ((Branches) getCurrentXMLNode()).getValue();
            if (null == val) {
                ((Branches) getCurrentXMLNode()).setValue(new String(ch, start, length));
            } else {
                ((Branches) getCurrentXMLNode()).setValue(val.concat(new String(ch, start, length)));
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (qName.endsWith(Branches.TAG)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.fine("BranchesParser.endElement(): "
                        + mMsg.getString(UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }          
    /** Implements XML parser for FromPart element.
    */
    protected class FromPartXMLParser extends XmlParser {
            
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
        }
        
        /** Constructs a FromPartParser.
         */
        public FromPartXMLParser() {
            super(getParserSupport());
        }
        
        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
        throws SAXException {
            String val = ((FromPart) getCurrentXMLNode()).getValue();
            if (null == val) {
                ((FromPart) getCurrentXMLNode()).setValue(new String(ch, start, length));
            } else {
                ((FromPart) getCurrentXMLNode()).setValue(val.concat(new String(ch, start, length)));
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (qName.endsWith(FromPart.TAG)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.fine("FromPartXMLParser.endElement(): "
                        + mMsg.getString(UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
            /** Implements XML parser for FromPart element.
     */
    protected class ToPartXMLParser extends XmlParser {
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
        }
        
        /** Constructs a ToPartParser.
         */
        public ToPartXMLParser() {
            super(getParserSupport());
        }
        
        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
        throws SAXException {
            String val = ((ToPart) getCurrentXMLNode()).getValue();
            if (null == val) {
                ((ToPart) getCurrentXMLNode()).setValue(new String(ch, start, length));
            } else {
                ((ToPart) getCurrentXMLNode()).setValue(val.concat(new String(ch, start, length)));
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (qName.endsWith(ToPart.TAG)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.fine("ToPartXMLParser.endElement(): "
                        + mMsg.getString(UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    /** Implements XML parser for Expression element, it is Sun's extension immediate child of extensionAssignOperation.
     */
    protected class SunExtExpressionElementParser extends XmlParser {
        /**
         * Constructs a ExpressionElementParser.
         */
        public SunExtExpressionElementParser() {
            super(getParserSupport());
        }

        /**
         * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String,
         *      java.lang.String, java.lang.String, org.xml.sax.Attributes)
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
        }

        /**
         * Called when character data (CDATA) is detected.
         * 
         * @param ch
         *            Characters encountered.
         * @param start
         *            Starting offset in array.
         * @param length
         *            Number of characters.
         * @throws SAXException
         *             If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
                throws SAXException {
            String val = ((SunExtExpression) getCurrentXMLNode()).getValue();
            if (null == val) {
                ((SunExtExpression) getCurrentXMLNode()).setValue(new String(ch, start,
                        length));
            } else {
                ((SunExtExpression) getCurrentXMLNode()).setValue(val.concat(new String(
                        ch, start, length)));
            }
        }

        /**
         * Called when an element is ended.
         * 
         * @param uri
         *            URI of namespace.
         * @param localName
         *            Local name of element.
         * @param qName
         *            Qualified name of element.
         * @throws SAXException
         *             If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
                throws SAXException {
            if (qName.endsWith(SunExtExpression.TAG)) {
                getParserSupport().popXmlParser();
                String value = ((SunExtExpression) getCurrentXMLNode()).getValue();
                ((SunExtExpression) getCurrentXMLNode()).setExpression(value);
            }
        }
    }
    
    
    /** Implements XML parser for Literal element.
     */
    protected class LiteralXmlParser extends XmlParser {
        /** Constructs a LiteralXmlParser.
         */
        public LiteralXmlParser() {
            super(getParserSupport());
        }
        
        /** @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes)
         */
        public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            try {
                String prefix= "";

                Map nsMap = ((XMLElement) getCurrentXMLNode()).getTotalNamespaces();
                Set nsKeys = nsMap.keySet();
                String nsKey = null;
                String ns = null;
                for (java.util.Iterator itr = nsKeys.iterator(); itr.hasNext(); ) {
                    nsKey = (String) itr.next();
                    ns = (String) nsMap.get(nsKey);
                    if (ns.equals(uri)) {
                        prefix = nsKey;
                        break;
                    }
                }

                Document doc = factory.newDocumentBuilder().newDocument();
                Element childElem = doc.createElementNS(uri, prefix + ":" + localName);
                doc.appendChild(childElem);
                ((Literal) getCurrentXMLNode()).setEII(childElem);
                String attrQName;
                String val;
                String attrURI;
                Attr attr;
                for (int i = 0, n = attributes.getLength(); i < n; i++) {
                    attrQName = attributes.getQName(i);
                    val = attributes.getValue(i);
                    attrURI = attributes.getURI(i);
                    if (Utility.isEmpty(attrURI)) {
                        attr = doc.createAttribute(attrQName);
                    } else {
                        attr = doc.createAttributeNS(attrURI, attrQName);
                    }
                    attr.setNodeValue(val);
                    childElem.setAttributeNodeNS(attr);
                }

                attr = null;
                for (java.util.Iterator itr = nsKeys.iterator(); itr.hasNext(); ) {
                    nsKey = (String) itr.next();
                    ns = (String) nsMap.get(nsKey);
                    childElem.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:" + nsKey, ns);                        	
                    
//                    if(nsKey == "xmlns") {
//                    	Attr childElemAttr = childElem.getAttributeNodeNS("http://www.w3.org/2000/xmlns/", nsKey);
//                        
//                        if (childElemAttr == null) {
//                        	childElem.setAttributeNS("http://www.w3.org/2000/xmlns/", nsKey, ns);                        	
//                        }
//                    } else {
//                        Attr childElemAttr = childElem.getAttributeNodeNS("http://www.w3.org/2000/xmlns/", "xmlns:" + nsKey);
//                        if (childElemAttr == null) {
//                            childElem.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:" + nsKey, ns);                        	
//                        }
//                    }
                }
                    
                LiteralElementParser litElemParser = new LiteralElementParser(childElem, this);
                getParserSupport().getXmlReader().setContentHandler(litElemParser);
            } catch (ParserConfigurationException pCE) {
                throw new RuntimeException(pCE);
            }
        }

        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
        throws SAXException {
            if (((Literal) getCurrentXMLNode()).getEII() == null) {
                String val = ((Literal) getCurrentXMLNode()).getValue();
                if (null == val) {
                    ((Literal) getCurrentXMLNode()).setValue(new String(ch, start, length));
                } else {
                    ((Literal) getCurrentXMLNode()).setValue(val.concat(new String(ch, start, length)));
                }
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (qName.endsWith(Literal.TAG)) {
                getParserSupport().popXmlParser();
                if (((Literal) getCurrentXMLNode()).getEII() != null) {
                    ((Literal) getCurrentXMLNode()).setValue(null);
                    Element origElem = ((Literal) getCurrentXMLNode()).getEII();
                    String str = createXmlString(origElem);
                    Element finalElem = createDOMElement(str);
                    ((Literal) getCurrentXMLNode()).setEII(finalElem);
                }
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            }
        }
        
        /**
         * Convert the Element to a string representing XML  
         * @param node The DOM Node
         * @return The string representing XML
         */
        public String createXmlString(Node node) {
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
        
        /**
         * Creates DOM element from stringified xml string
         *
         * @param stringiXml stringified xml string
         * @return DOM element
         * @throws RuntimeException if cannot create DOM
         */
        private Element createDOMElement(String xmlString) {
            InputSource is;
            try {
                is = new InputSource(new StringReader(xmlString));
                DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                factory.setNamespaceAware(true);
               
                DocumentBuilder db = factory.newDocumentBuilder();
                Document doc = null;
                try { 
                    doc = db.parse(is); 
                } finally { 
                     
                }
                return doc.getDocumentElement();
            } catch (Exception e) {
                throw new RuntimeException(e);
            } 
        }        
    }
    
    
    /** Implements XML parser for For element.
     */
    protected class ForXmlParser extends XmlParser {
        
        /** Constructs a ForXmlParser.
         */
        public ForXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
        throws SAXException {
            String val = ((For) getCurrentXMLNode()).getValue();
            if (null == val) {
                ((For) getCurrentXMLNode()).setValue(new String(ch, start, length));
            } else {
                ((For) getCurrentXMLNode()).setValue(val.concat(new String(ch, start, length)));
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (qName.endsWith(For.TAG)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.fine("ForXmlParser.endElement(): "
                        + mMsg.getString(UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for Until element.
     */
    protected class UntilXmlParser extends XmlParser {
        
        /** Constructs a UntilXmlParser.
         */
        public UntilXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
        throws SAXException {
            String val = ((Until) getCurrentXMLNode()).getValue();
            if (null == val) {
                ((Until) getCurrentXMLNode()).setValue(new String(ch, start, length));
            } else {
                ((Until) getCurrentXMLNode()).setValue(val.concat(new String(ch, start, length)));
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (qName.endsWith(Until.TAG)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.fine("UntilXmlParser.endElement(): "
                        + mMsg.getString(UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }
    
    
    /** Implements XML parser for RepeatEvery element.
     */
    protected class RepeatEveryXmlParser extends XmlParser {
        
        /** Constructs a RepeatEveryXmlParser.
         */
        public RepeatEveryXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
        throws SAXException {
            String val = ((RepeatEvery) getCurrentXMLNode()).getValue();
            if (null == val) {
                ((RepeatEvery) getCurrentXMLNode()).setValue(new String(ch, start, length));
            } else {
                ((RepeatEvery) getCurrentXMLNode()).setValue(val.concat(new String(ch, start, length)));
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (qName.endsWith(RepeatEvery.TAG)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.fine("RepeatEveryXmlParser.endElement(): "
                        + mMsg.getString(UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }

    /** Implements XML parser for onMessage element.
     */
    protected class EventHandlersOnEventXmlParser extends XmlParser {
        
        /** Constructs a EventHandlersOnEventXmlParser.
         */
        public EventHandlersOnEventXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(Correlations.TAG, uri, localName, qName)) {
                Correlations tCorrelations = getXmlDocument().createCorrelations();
                ((EventHandlersOnEvent) getCurrentXMLNode()).setCorrelations(tCorrelations);
                tCorrelations.setQualifiedName(qName);
                tCorrelations.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tCorrelations, attributes);
                tCorrelations.accept(getVisitor());
            }else if(Namespaces.isBPELElement(FromPart.TAG, uri, localName, qName)) {
                FromPart tFromPart = getXmlDocument().createFromPart();
                ((EventHandlersOnEvent) getCurrentXMLNode()).setFromPart(tFromPart);
                tFromPart.setQualifiedName(qName);
                tFromPart.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tFromPart, attributes);
                tFromPart.accept(getVisitor());   
            }  else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(EventHandlersOnEvent.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("EventHandlersOnEventXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }

    /** Implements XML parser for onAlarm element.
     */
    protected class EventHandlersOnAlarmXmlParser extends XmlParser {
        
        /** Constructs a EventHandlersOnAlarmXmlParser.
         */
        public EventHandlersOnAlarmXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(For.TAG, uri, localName, qName)) {
                For tFor = getXmlDocument().createFor();
                ((EventHandlersOnAlarm) getCurrentXMLNode()).setBPELFor(tFor);
                tFor.setQualifiedName(qName);
                tFor.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tFor, attributes);
                tFor.accept(getVisitor());
            } else if (Namespaces.isBPELElement(Until.TAG, uri, localName, qName)) {
                Until tUntil = getXmlDocument().createUntil();
                ((EventHandlersOnAlarm) getCurrentXMLNode()).setBPELUntil(tUntil);
                tUntil.setQualifiedName(qName);
                tUntil.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tUntil, attributes);
                tUntil.accept(getVisitor());
            }else if (Namespaces.isBPELElement(RepeatEvery.TAG, uri, localName, qName)) {
                RepeatEvery tRepeatEvery = getXmlDocument().createRepeatEvery();
                ((EventHandlersOnAlarm) getCurrentXMLNode()).setBPELRepeatEvery(tRepeatEvery);
                tRepeatEvery.setQualifiedName(qName);
                tRepeatEvery.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tRepeatEvery, attributes);
                tRepeatEvery.accept(getVisitor());
            } 
            else {
                readActivity(getCurrentXMLNode(), uri, localName, qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(EventHandlersOnAlarm.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("EventHandlersOnAlarmXmlParser.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }

    /** Implements XML parser for onAlarm element.
     */
    protected class EventHandlersXmlParser extends XmlParser {
        
        /** Constructs a EventHandlersXmlParser.
         */
        public EventHandlersXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName, Attributes attributes)
                throws SAXException {
            if (Namespaces.isBPELElement(EventHandlersOnEvent.TAG, uri, localName, qName)) {
                EventHandlersOnEvent tEventHandlersOnEvent = getXmlDocument()
                        .createEventHandlersOnEvent();
                ((EventHandlers) getCurrentXMLNode()).addOnEvent(tEventHandlersOnEvent);
                tEventHandlersOnEvent.setQualifiedName(qName);
                tEventHandlersOnEvent.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tEventHandlersOnEvent, attributes);
                SAXParseVisitorService vService =
                    (SAXParseVisitorService) getVisitorService();
                BPELParseContext context = vService.getBPELParseContext();
                tEventHandlersOnEvent.initMembersLocally();
                //Register deferred initialization
                context.getDeferredActionRegistry()
                    .getActionAccepterCollector().add(tEventHandlersOnEvent);
                tEventHandlersOnEvent.accept(getVisitor());
            } else if (Namespaces.isBPELElement(EventHandlersOnAlarm.TAG, uri, localName, qName)) {
                EventHandlersOnAlarm tEventHandlersOnAlarm = getXmlDocument()
                        .createEventHandlersOnAlarm();
                ((EventHandlers) getCurrentXMLNode()).addOnAlarm(tEventHandlersOnAlarm);
                tEventHandlersOnAlarm.setQualifiedName(qName);
                tEventHandlersOnAlarm.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tEventHandlersOnAlarm, attributes);
                tEventHandlersOnAlarm.accept(getVisitor());
            } else {
                handleIllegalElement(getCurrentXMLNode().getLocalName(), uri, localName, qName,
                        attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            if (Namespaces.isBPELElement(EventHandlers.TAG, uri, localName, qName)) {
                getParserSupport().popXmlParser();
                ((BaseElementImpl) getCurrentXMLNode()).setString();
            } else {
                if(("import".equalsIgnoreCase(localName)) || ("documentation".equalsIgnoreCase(localName)))
                    return;
                
                mLogger.warning("EventHandlers.endElement(): "
                        + mMsg.getString(
                        UNRECOGNIZED_END_ELEMENT, qName, uri));
            }
        }
    }

    /** Add a potential documentation element.
     * @param   qName   Qualified name of potential element.
     * @param   attrs   Attributes of potential element.
     * @param   locator Locator for potential element.
     * @return  <tt>true</tt> if a documentation element was added.
     */
    protected boolean addDocumentationElement(XMLNode currentXMLNode, String qName, Attributes attrs, Locator locator) {
        boolean added = false;
        if (qName.endsWith(Documentation.TAG)) {
            Documentation doc = getXmlDocument().createBPELDocumentation();
            doc.setQualifiedName(qName);
            doc.setLocator(locator);
            SAXParserSupport.setAttributes(doc, attrs);
            currentXMLNode.addChild(doc);
            doc.accept(getVisitor());
            added = true;
        }
        return added;
    }
    
    
    /** Add standard elements.
     * @param   act         Activity for adding standard elements.
     * @param   uri         URI namespace.
     * @param   localName   Local name.
     * @param   qName       Qualified name.
     * @param   attrs       Attributes.
     * @param   locator     Locator for standard element.
     * @return  <tt>true</tt> if a standard element was added.
     */
    protected boolean addStandardElement(Activity act, String uri, String localName, String qName,
            Attributes attrs, Locator locator) {
        boolean found = true;
        if (Namespaces.isBPELElement(Target.TAG, uri, localName, qName)) {
            Target t = getXmlDocument().createTarget();
            act.addTarget(t);
            t.setQualifiedName(qName);
            t.setLocator(locator);
            SAXParserSupport.setAttributes(t, attrs);
        } else if (Namespaces.isBPELElement(Source.TAG, uri, localName, qName)) {
            Source s = getXmlDocument().createSource();
            act.addSource(s);
            s.setQualifiedName(qName);
            s.setLocator(locator);
            SAXParserSupport.setAttributes(s, attrs);
        } else {
            found = false;
        }
        return found;
    }
    
    /** Detect ends of standard elements.
     * @param   uri         URI namespace.
     * @param   localName   Local name.
     * @param   qName       Qualified name.
     * @return  <tt>true</tt> if a standard element ended.
     */
    protected boolean endStandardElement(String uri, String localName, String qName) {
        boolean found = true;
        if (Namespaces.isBPELElement(Target.TAG, uri, localName, qName)) {
            // action taken already in above test
        } else if (Namespaces.isBPELElement(Source.TAG, uri, localName, qName)) {
            // action taken already in above test
        } else {
            found = false;
        }
        return found;
    }
    
    /** Handle illegal elements found.
     */
    protected void handleIllegalElement(String foundInElem, String uri, String localName,
            String qName, Attributes attributes) {
        if (("import".equalsIgnoreCase(localName))
                || ("documentation".equalsIgnoreCase(localName)))
            return;
        mLogger.severe(mMsg.getString(ILLEGAL_ELEMENT_ENCOUNTERED, uri, localName, foundInElem));
    }
    
    /**
     * Adds a BPEL extensibility element under the current BPEL element.
     *
     * @param   uri         The governing URI namespace.
     * @param   localName   Local name of the element.
     * @param   qName       Qualified name of the element.
     * @param   attributes  Attributes of the element.
     */
    protected void addExtensibilityElement(XMLNode currentXMLNode,
            String uri,
            String localName,
            String qName,
            Attributes attributes) {
        mLogger.finer(mMsg.getString(UNRECOGNIZED_START_ELEMENT, qName, uri));
        ExtensibilityElement exten = getXmlDocument().createExtensibilityElement();
        exten.setElementType(com.sun.bpel.xml.NamespaceUtility.
    			getQNameFromURIAndString(uri, qName));
        SAXParserSupport.setAttributes(exten, attributes);
        exten.setLocator(getParserSupport().getLocator());
        currentXMLNode.addChild(exten);
        exten.accept(getVisitor());
    }
    
    /** Add a potential documentation element.
     * @param   qName   Qualified name of potential element.
     * @param   attrs   Attributes of potential element.
     * @param   locator Locator for potential element.
     * @return  <tt>true</tt> if a documentation element was added.
     */
    protected boolean addDocumentationElement(XMLNode currentXMLNode,
            BPELDocument document,
            String qName,
            Attributes attrs,
            Locator locator) {
        boolean added = false;
        if (qName.endsWith(Documentation.TAG)) {
            Documentation doc = document.createBPELDocumentation();
            doc.setQualifiedName(qName);
            doc.setLocator(locator);
            SAXParserSupport.setAttributes(doc, attrs);
            currentXMLNode.addChild(doc);
            doc.accept(getVisitor());
            added = true;
        }
        return added;
    }
    
    /** Reads in a activity element or extensibility element.
     *
     * @param   uri         URI governing that qualified name.
     * @param   localName   Local name of element.
     * @param   qName       The qualified name of the element.
     * @param   attributes  Attributes of the element.
     * @throws  SAXException    when XML syntax errors occur.
     */
    protected void readActivity(XMLNode currentXMLNode,
            String uri,
            String localName,
            String qName,
            Attributes attributes) throws SAXException {
        Activity tActivity = null;
        if (Namespaces.isBPELElement(Receive.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createReceive();
        } else if (Namespaces.isBPELElement(Reply.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createReply();
        } else if (Namespaces.isBPELElement(Invoke.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createInvoke();
        } else if (Namespaces.isBPELElement(Assign.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createAssign();
        } else if (Namespaces.isBPELElement(Throw.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createThrow();
        } else if (Namespaces.isBPELElement(Terminate.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createTerminate();
        } else if (Namespaces.isBPELElement(Wait.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createWait();
        } else if (Namespaces.isBPELElement(Empty.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createEmpty();
        } else if (Namespaces.isBPELElement(Sequence.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createSequence();
        } else if (Namespaces.isBPELElement(Switch.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createSwitch();
        } else if (Namespaces.isBPELElement(While.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createWhile();
        } else if (Namespaces.isBPELElement(Pick.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createPick();
        } else if (Namespaces.isBPELElement(Flow.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createFlow();
        } else if (Namespaces.isBPELElement(Scope.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createScope();
        } else if (Namespaces.isBPELElement(Compensate.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createCompensate();
        } else if (Namespaces.isBPELElement(CompensateScope.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createCompensateScope();
        } else if (Namespaces.isBPELElement(Rethrow.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createRethrow();
        } else if (Namespaces.isBPELElement(RepeatUntil.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createRepeatUntil();
        } else if (Namespaces.isBPELElement(If.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createIf();
        } else if (Namespaces.isBPELElement(Validate.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createValidate();
        } else if (Namespaces.isBPELElement(com.sun.bpel.model.ForEach.TAG, uri, localName, qName)) {
            tActivity = getXmlDocument().createForEach();
        } 
        if (tActivity != null) {
            if (currentXMLNode instanceof SingleActivityHolder) {
                ((SingleActivityHolder) currentXMLNode).setActivity(tActivity);
            } else {
                MultipleActivityHolder curElementXML = (MultipleActivityHolder) currentXMLNode;
                curElementXML.addActivity(tActivity);
            }
            tActivity.setQualifiedName(qName);
            tActivity.setLocator(getParserSupport().getLocator());
            SAXParserSupport.setAttributes(tActivity, attributes);
            tActivity.accept(getVisitor());
        } else if (!Namespaces.isBPELElement(localName, uri, localName, qName)) {
            addExtensibilityElement(currentXMLNode,  uri, localName, qName, attributes);
        } else {
            handleIllegalElement(currentXMLNode.getLocalName(), uri, localName,
                    qName, attributes);
        }
    }
    
    private class LiteralElementParser extends XmlParser {
        Element mElem;
        QName mEndTag;
        XmlParser mParentParser;
        Element mCurrElement;
        Document mDoc;
        Stack<Element> currElements = new Stack<Element>();
        Text mCurrText;

        LiteralElementParser(Element elem, XmlParser parentParser) {
            mElem = elem;
            mEndTag = new QName(elem.getNamespaceURI(), elem.getLocalName());
            mParentParser = parentParser;
            mDoc = mElem.getOwnerDocument();
            mCurrElement = elem;
            mCurrText = mDoc.createTextNode("");
            mCurrElement.appendChild(mCurrText);
        }

        /** @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes)
         */
        public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

            Element childElem = mDoc.createElementNS(uri, localName);
            mCurrElement.appendChild(childElem);
            String attrQName;
            String val;
            String attrURI;
            Attr attr;
            for (int i = 0, n = attributes.getLength(); i < n; i++) {
                attrQName = attributes.getQName(i);
                val = attributes.getValue(i);
                attrURI = attributes.getURI(i);
                if (Utility.isEmpty(attrURI)) {
                    attr = mDoc.createAttribute(attrQName);
                } else {
                    attr = mDoc.createAttributeNS(attrURI, attrQName);
                }                
                attr.setNodeValue(val);
                childElem.setAttributeNodeNS(attr);
            }
            currElements.push(mCurrElement);
            mCurrElement = childElem;
            mCurrText = mDoc.createTextNode("");
            mCurrElement.appendChild(mCurrText);            
        }


        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
        throws SAXException {
            String origVal = mCurrText.getNodeValue();
            CharArrayWriter charWtr = new CharArrayWriter();
            charWtr.write(ch, start, length);
            String appendVal = charWtr.toString();
            if (origVal == null) {
                origVal = appendVal;
            } else {
                origVal = origVal.concat(appendVal);
            }
            mCurrText.setNodeValue(origVal);
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
        throws SAXException {
            QName tagName = new QName(uri, localName);
            if (tagName.equals(mEndTag)) {
                getParserSupport().getXmlReader().setContentHandler(mParentParser);
            } else {
                mCurrElement = currElements.pop();
                mCurrText = mDoc.createTextNode("");
                mCurrElement.appendChild(mCurrText);
            }
        }        
    }
}
