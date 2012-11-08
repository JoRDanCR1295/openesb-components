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
 * @(#)BPELDocument.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import java.io.Writer;
import java.util.Collection;

import org.apache.xmlbeans.SchemaTypeLoader;
import org.apache.xmlbeans.SchemaTypeSystem;

import com.sun.bpel.model.extensions.Alert;
import com.sun.bpel.model.extensions.Choose;
import com.sun.bpel.model.extensions.Default;
import com.sun.bpel.model.extensions.ExtensibilityElement;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.model.extensions.Log;
import com.sun.bpel.model.extensions.Trace;
import com.sun.bpel.model.extensions.When;
import com.sun.bpel.xml.common.model.Documentation;
import com.sun.bpel.xml.common.model.PrivateExtensionMapModel;
import com.sun.bpel.xml.common.model.XMLComment;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.model.XMLText;
import com.sun.bpel.xml.common.visitor.VisitorService;

/**
 * Describes a BPEL4WS XML Document.  Also provides a factory for all the
 * BPEL4WS elements since they're only valid within the context of a BPEL4WS
 * document.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface BPELDocument extends XMLDocument, XMLElement {
    long VARIABLE_START_ID = 1000000;
    long ACTIVITY_START_ID = 2000000;
    long CORRELATION_START_ID = 3000000;
    long CATCH_START_ID = 4000000;
    long PARTNERLINK_START_ID = 5000000;
    
    /** Prefix for BPEL document */
    public static final String BPEL_PREFIX = "bpws";

    /** Namespace for BPEL document */
    public static final String BPEL_NAMESPACE =
            "http://docs.oasis-open.org/wsbpel/2.0/process/executable";

    /**
     * property change event
     * the value in property change event is File Object of the import
     * document whose parsing is started.
     */
    public static final String IMPORTED_DOCUMENT_PARSING_STARTED = "IMPORTED_DOCUMENT_PARSING_STARTED";

    /**
     * the value in property change event is File Object of the import
     * document whose parsing is ended.
     */
    public static final String IMPORTED_DOCUMENT_PARSING_ENDED = "IMPORTED_DOCUMENT_PARSING_ENDED";


    /**
     * Serializes the BPEL document to the writer.
     * @param writer the writer
     */
    void serialize(Writer writer);

    /**
     * Duplicates (deep cloning) an element.
     * @param   original    Original element to be cloned.
     * @param   destination Destination document.
     * @return  Duplicated element.
     */
    XMLNode duplicate(XMLNode original, BPELDocument destination);

    /**
     * Validates the BPEL document.
     * @param   todoListeners   Collection of <code>ToDoListener</code> for <code>ToDoEvent</code>
     */
    void validate(Collection todoListeners);

    /** Validates the BPEL document.
     * @param   v   Values to use.
     *              <ol start="0">
     *              <li><code>java.util.Collection</code> of <code>com.sun.bpel.model.common.todotask.ToDoListener</code>
     *              objects.</li>
     *              <li>Optional. <code>com.sun.bpel.model.common.visitor.ValidateConfiguration</code> to use
     *              for validation.</li>
     *              <li>Optional. <code>com.sun.bpel.model.common.model.XMLNode</code> to start validation at.</li>
     *              <li>Optional. <code>com.stc.repository.Repository</code> handle for eInsight license
     *              verification.</li>
     *              </ol>
     */
    void validate(Object[] v);

    /** Initializes the BPEL document with an empty process and the specified targetNamespace.
     * @param   tns         Target namespace.
     * @param   procName    Name of the Business Process.
     */

    void initializeEmptyProcess(String tns, String procName);

    /** Getter for the root element, i.e., the &lt;process&gt;, in the document.
     * @return  Root element in the document.
     */
    BPELProcess getDocumentProcess();

    /** Setter for the root element, i.e., the &lt;process&gt;, in the document.
     * @param   p   Root element in the document.
     */
    void setDocumentProcess(BPELProcess p);

    /** Traverses the document to perform some work by the visitor
     * that is provided by the service.
     *
     * @param   s   Visitor service provider.
     * @param   v   Values to prepare the persistor.
     */
    void traverse(VisitorService s, Object[] v);

    /** Creates a XML node for this document.
     * @param   t   Type of node to create.
     *  <table border="1">
     *  <th>Type</th><th>XML Node created</th>
     *  <tr><td>"activity"</td><td>ActivityImpl</td></tr>
     *  <tr><td>"assign"</td><td>AssignImpl</td></tr>
     *  <tr><td>"case"</td><td>CaseImpl</td></tr>
     *  <tr><td>"catch"</td><td>CatchImpl</td></tr>
     *  <tr><td>"catchAll"</td><td>CatchAllImpl</td></tr>
     *  <tr><td>"compensate"</td><td>CompensateImpl</td></tr>
     *  <tr><td>"compensationHandler"</td><td>CompensationHandlerImpl</td></tr>
     *  <tr><td>"container"</td><td>VariableImpl</td></tr>
     *  <tr><td>"containers"</td><td>VariablesImpl</td></tr>
     *  <tr><td>"copy"</td><td>CopyImpl</td></tr>
     *  <tr><td>"correlation"</td><td>CorrelationImpl</td></tr>
     *  <tr><td>"correlations"</td><td>CorrelationsImpl</td></tr>
     *  <tr><td>"correlationSet"</td><td>CorrelationSetImpl</td></tr>
     *  <tr><td>"correlationSets"</td><td>CorrelationSetsImpl</td></tr>
     *  <tr><td>"empty"</td><td>EmptyImpl</td></tr>
     *  <tr><td>"faultHandlers"</td><td>FaultHandlersImpl</td></tr>
     *  <tr><td>"flow"</td><td>FlowImpl</td></tr>
     *  <tr><td>"from"</td><td>FromImpl</td></tr>
     *  <tr><td>"invoke"</td><td>InvokeImpl</td></tr>
     *  <tr><td>"link"</td><td>LinkImpl</td></tr>
     *  <tr><td>"links"</td><td>LinksImpl</td></tr>
     *  <tr><td>"onAlarm"</td><td>OnAlarmImpl</td></tr>
     *  <tr><td>"onMessage"</td><td>OnMessageImpl</td></tr>
     *  <tr><td>"otherwise"</td><td>OtherwiseImpl</td></tr>
     *  <tr><td>"partner"</td><td>PartnerLinkImpl</td></tr>
     *  <tr><td>"partners"</td><td>PartnerLinksImpl</td></tr>
     *  <tr><td>"pick"</td><td>PickImpl</td></tr>
     *  <tr><td>"process"</td><td>BPELProcessImpl</td></tr>
     *  <tr><td>"#processingInstruction"</td><td>XMLProcessingInstructionImpl
     *  </td></tr>
     *  <tr><td>"receive"</td><td>ReceiveImpl</td></tr>
     *  <tr><td>"reply"</td><td>ReplyImpl</td></tr>
     *  <tr><td>"scope"</td><td>ScopeImpl</td></tr>
     *  <tr><td>"sequence"</td><td>SequenceImpl</td></tr>
     *  <tr><td>"source"</td><td>SourceImpl</td></tr>
     *  <tr><td>"switch"</td><td>SwitchImpl</td></tr>
     *  <tr><td>"target"</td><td>TargetImpl</td></tr>
     *  <tr><td>"terminate"</td><td>TerminateImpl</td></tr>
     *  <tr><td>"terminationHandler"</td><td>TerminationHandlerImpl</td></tr>
     *  <tr><td>"throw"</td><td>ThrowImpl</td></tr>
     *  <tr><td>"to"</td><td>ToImpl</td></tr>
     *  <tr><td>"wait"</td><td>WaitImpl</td></tr>
     *  <tr><td>"while"</td><td>WhileImpl</td></tr>
     *  <tr><td>"message"</td><td>WSDLMessageImpl</td></tr>
     *  <tr><td>"#comment"</td><td>XMLCommentImpl</td></tr>
     *  <tr><td>"#text"</td><td>XMLTextImpl</td></tr>
     *  <tr><td>"#extension"</td><td>ExtensibilityElementImpl</td></tr>     *  </table>
     * @return  New XML node.
     */
    XMLNode createXmlNode(String t);

    /** Creates a text section for XML element.
     * @return  new text section.
     */
    XMLText createXmlText();

    /** Creates a XML comment.
     * @return  new comment.
     */
    XMLComment createXmlComment();

    /** Creates a process element.
     * @return  new process element.
     */
    BPELProcess createProcess();

    /** Creates a partnerLinks element.
     * @return  new partnerLinks element.
     */
    PartnerLinks createPartnerLinks();

    /** Creates a partnerLink element.
     * @return  new partnerLink element.
     */
    PartnerLink createPartnerLink();

    /** Creates a Variables element.
     * @return  new Variables element.
     */
    Variables createVariables();

    /** Creates a Variable element.
     * @return  new Variable element.
     */
    Variable createVariable();

    /** Creates a correlationSets element.
     * @return  new correlationSets element.
     */
    CorrelationSets createCorrelationSets();

    /** Creates a correlationSet element.
     * @return  new correlationSet element.
     */
    CorrelationSet createCorrelationSet();

    /** Creates a faultHandlers element.
     * @return  new faultHandlers element.
     */
    FaultHandlers createFaultHandlers();

    /** Creates a eventHandlers element.
     * @return  new eventHandlers element.
     */
    EventHandlers createEventHandlers();

    /** Creates a catch element.
     * @return  new catch element.
     */
    Catch createCatch();

    /** Creates a catchAll element.
     * @return  new catchAll element.
     */
    CatchAll createCatchAll();

    /** Creates a compensationHandler element.
     * @return  new compensationHandler element.
     */
    CompensationHandler createCompensationHandler();

    /** Creates a terminationHandler element.
     * @return  new terminationHandler element.
     */
    TerminationHandler createTerminationHandler();    
    
    /** Creates a receive element.
     * @return  new receive element.
     */
    Receive createReceive();

    /** Creates a target element.
     * @return  new target element.
     */
    Target createTarget();

    /** Creates a source element.
     * @return  new source element.
     */
    Source createSource();

    /** Creates a correlations element.
     * @return  new correlations element.
     */
    Correlations createCorrelations();

    /** Creates a correlation element.
     * @return  new correlation element.
     */
    Correlation createCorrelation();

    /** Creates a reply element.
     * @return  new reply element.
     */
    Reply createReply();

    /** Creates a invoke element.
     * @return  new invoke element.
     */
    Invoke createInvoke();

    /** Creates a assign element.
     * @return  new assign element.
     */
    Assign createAssign();

    /** Create a copy element.
     * @return  new copy element.
     */
    Copy createCopy();

    /** Creates a throw element.
     * @return  new throw element.
     */
    Throw createThrow();

    /** Creates a terminate element.
     * @return  new terminate element.
     */
    Terminate createTerminate();

    /** Creates a wait element.
     * @return  new wait element.
     */
    Wait createWait();

    /** Creates a empty element.
     * @return  new empty element.
     */
    Empty createEmpty();

    /** Creates a sequence element.
     * @return  new sequence element.
     */
    Sequence createSequence();

    /** Creates a switch element.
     * @return  new switch element.
     */
    Switch createSwitch();

    /** Creates a while element.
     * @return  new while element.
     */
    While createWhile();

    /** Creates a pick element.
     * @return  new pick element.
     */
    Pick createPick();

    /** Creates a flow element.
     * @return  new flow element.
     */
    Flow createFlow();

    /** Creates a scope element.
     * @return  new scope element.
     */
    Scope createScope();

    /** Creates a compensate element.
     * @return  new compensate element.
     */
    Compensate createCompensate();

    /** Creates a compensateScope element.
     * @return  new compensate element.
     */
    CompensateScope createCompensateScope();

    /** Creates a case element.
     * @return  new case element.
     */
    Case createCase();

    /** Creates a from element.
     * @return  new from element.
     */
    From createFrom();

    /** Creates a link element.
     * @return  new link element.
     */
    Link createLink();

    /** Creates a links element.
     * @return  new links element.
     */
    Links createLinks();

    /** Creates a otherwise element.
     * @return  new otherwise element.
     */
    Otherwise createOtherwise();

    /** Creates a to element.
     * @return  new to element.
     */
    To createTo();

    /** Creates a onMessage element.
     * @return  new onMessage element.
     */
    OnMessage createOnMessage();

    /** Creates a onAlarm element.
     * @return  new onAlarm element.
     */
    OnAlarm createOnAlarm();

    /** Creates a ForEach (SBYN BPEL Extension) element.
     * @return new ForEach element.
     */
    com.sun.bpel.model.extensions.ForEach createForEachSBYN();

    /** Create a bpel20 ForEach element.
     * @return new ForEach element
     */
    com.sun.bpel.model.ForEach createForEach ();

    /** Creates a Choose (SBYN BPEL Extension) element.
     * @return new Choose element.
     */
    Choose createChoose();

    /** Creates a When (SBYN BPEL Extension) element.
     * @return new When element.
     */
    When createWhen();

    /** Creates a Default (SBYN BPEL Extension) element.
     * @return new Default element.
     */
    Default createDefault();

    /**
     * Creates an Import (bpel 2.0) element.
     * @return new Import element.
     */
    Import createImport();

    /**
     * Creates a generic extensibility element.
     * @return new extensibility element.
     */
    ExtensibilityElement createExtensibilityElement();

    /**
     *  create Documentation
     * @return Documentation
     */
    Documentation createBPELDocumentation();

    /**
     * create Condition.
     * @return Condition
     */
    Condition createCondition();

    /**
     * create Literal.
     * @return Literal
     */
    Literal createLiteral();

    /**
     * create For.
     * @return For
     */
    For createFor();

    /**
     * Create Until.
     * @return Until.
     */
    Until createUntil();

    Rethrow createRethrow();

    RepeatUntil createRepeatUntil();

    If createIf();

    ElseIf createElseIf();

    Else createElse();

    Validate createValidate();

    FromPart createFromPart();

    ToPart createToPart();

    Iterator createIterator();

    StartCounterValue createStartCounterValue();

    FinalCounterValue createFinalCounterValue();

    CompletionCondition createCompletionCondition();

    Branches createBranches();

    EventHandlersOnEvent createEventHandlersOnEvent ();

    EventHandlersOnAlarm createEventHandlersOnAlarm ();

    RepeatEvery createRepeatEvery ();
    
    Trace createTraceSunExt();
    
    ExtensionAssignOperation createExtensionAssignOperation();
    
    SunExtExpression createSunExtExpression();
    
    Log createLogSunExt();
    
    Alert createAlertSunExt();

    /** Gets the private extension map model associated with this document's associated
     *  repository object (if any).
     *
     *  @return Private extension map model for this document.
     *  @since  5.1.0
     */
    PrivateExtensionMapModel getPrivateExtensionMapModel();

    BPELParseContext getBPELParseContext();

    void setBPELParseContext(BPELParseContext context);
}
