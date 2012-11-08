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
 * @(#)ValidateVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.visitor;

import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.wsdl.Fault;
import javax.wsdl.Operation;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.Assign;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Case;
import com.sun.bpel.model.Catch;
import com.sun.bpel.model.CatchAll;
import com.sun.bpel.model.Compensate;
import com.sun.bpel.model.CompensationHandler;
import com.sun.bpel.model.Condition;
import com.sun.bpel.model.Else;
import com.sun.bpel.model.ElseIf;
import com.sun.bpel.model.EventHandlersOnEvent;
import com.sun.bpel.model.ExtensionAssignOperation;
import com.sun.bpel.model.If;
import com.sun.bpel.model.SunExtExpression;
import com.sun.bpel.model.TerminationHandler;
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
import com.sun.bpel.model.From;
import com.sun.bpel.model.Import;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.Link;
import com.sun.bpel.model.Links;
import com.sun.bpel.model.MultipleActivityHolder;
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
import com.sun.bpel.model.SingleActivityHolder;
import com.sun.bpel.model.Source;
import com.sun.bpel.model.Switch;
import com.sun.bpel.model.Target;
import com.sun.bpel.model.Terminate;
import com.sun.bpel.model.Throw;
import com.sun.bpel.model.To;
import com.sun.bpel.model.Validate;
import com.sun.bpel.model.Wait;
import com.sun.bpel.model.While;
import com.sun.bpel.model.common.MessageManager;
import com.sun.bpel.model.common.visitor.ValidateConfiguration;
import com.sun.bpel.model.common.visitor.ValidateSupport;
import com.sun.bpel.model.extensions.ExtensibilityElement;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.model.util.Utility;
import com.sun.bpel.model.xpath.AbstractXPathModelHelper;
import com.sun.bpel.model.xpath.XPathException;
import com.sun.bpel.model.xpath.XPathExpression;
import com.sun.bpel.model.xpath.XPathModel;
import com.sun.bpel.xml.common.model.XMLComment;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.model.XMLProcessingInstruction;
import com.sun.bpel.xml.common.model.XMLText;
import com.sun.bpel.xml.common.todotask.ToDoEvent;
import com.sun.bpel.xml.common.todotask.ToDoListener;
import com.sun.bpel.xml.common.todotask.ToDoEvent.Severity;
import com.sun.bpel.xml.common.visitor.ParentChildrenVisitor;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Visits all the model nodes and validates them.
 *
 * @author Sun Microsystems
 * @version 
 */
public class ValidateVisitor extends AbstractVisitor implements ParentChildrenVisitor {
    private BPELDocument mBpelDocument = null;
    
    /** First activity must be either receive or pick */
    public static final String VAL_FIRST_ACTIVITY_NOT_CREATE_INSTANCE =
            "VAL_FIRST_ACTIVITY_NOT_CREATE_INSTANCE";  // Not I18N
    
    /** Please define a receive or pick as first activity */
    public static final String FIX_FIRST_ACTIVITY_NOT_CREATE_INSTANCE =
            "FIX_FIRST_ACTIVITY_NOT_CREATE_INSTANCE";  // Not I18N
    
    /** Reply without matching receive or onMessage */
    public static final String VAL_REPLY_WITHOUT_MATCH = "VAL_REPLY_WITHOUT_MATCH";  // Not I18N
    
    /** Please match reply's partner, portType, operation with one of existing receive or onMessage */
    public static final String FIX_REPLY_WITHOUT_MATCH = "FIX_REPLY_WITHOUT_MATCH";  // Not I18N
    
    /** Fault not thrown by operation in scope */
    public static final String VAL_FAULT_NOT_THROWN_IN_SCOPE = "VAL_FAULT_NOT_THROWN_IN_SCOPE";  // Not I18N
    
    /** Please catch one actually thrown */
    public static final String FIX_FAULT_NOT_THROWN_IN_SCOPE = "FIX_FAULT_NOT_THROWN_IN_SCOPE";  // Not I18N
    
    /** Catch not effective */
    public static final String VAL_INEFFECTIVE_CATCH = "VAL_INEFFECTIVE_CATCH";  // Not I18N
    
    /** Please define faultName and/or faultContainer */
    public static final String FIX_INEFFECTIVE_CATCH = "FIX_INEFFECTIVE_CATCH";  // Not I18N
    
    /** Incorrect from partner format */
    public static final String VAL_INCORRECT_FROM_PARTNER_FMT = "VAL_INCORRECT_FROM_PARTNER_FMT";  // Not I18N
    
    /** PartnerLink does not exist in document */
    public static final String PARTNER_DOES_NOT_EXIST = "PARTNER_DOES_NOT_EXIST";  // Not I18N
    
    /** Please add partner to document */
    public static final String FIX_PARTNER_DOES_NOT_EXIST = "FIX_PARTNER_DOES_NOT_EXIST";  // Not I18N
    
    /** Please define serviceReference attribute */
    public static final String FIX_INCORRECT_FROM_PARTNER_FMT = "FIX_INCORRECT_FROM_PARTNER_FMT";  // Not I18N
    
    /** Incorrect from format */
    public static final String VAL_INCORRECT_FROM_FMT = "VAL_INCORRECT_FROM_FMT";  // Not I18N
    
    /** Please define a valid from format */
    public static final String FIX_INCORRECT_FROM_FMT = "FIX_INCORRECT_FROM_FMT";  // Not I18N
    
    /** Incorrect from format, missing container */
    public static final String VAL_MISSING_FROM_CONTAINER = "VAL_MISSING_FROM_CONTAINER";  // Not I18N
    
    /** Please define a valid from format container */
    public static final String FIX_MISSING_FROM_CONTAINER = "FIX_MISSING_FROM_CONTAINER";  // Not I18N
    
    /** missing from part, container and query is present **/
    public static final String VAL_MISSING_FROM_PART = "VAL_MISSING_FROM_PART";
    
    public static final String FIX_MISSING_FROM_PART = "FIX_MISSING_FROM_PART";
    
	
    /** Incorrect to format, missing container */
    public static final String VAL_MISSING_TO_CONTAINER = "VAL_MISSING_TO_CONTAINER";  // Not I18N
    
    /** Please define a valid from format container */
    public static final String FIX_MISSING_TO_CONTAINER = "FIX_MISSING_TO_CONTAINER";  // Not I18N
    
    /** missing to part, container and query is present **/
    public static final String VAL_MISSING_TO_PART = "VAL_MISSING_TO_PART";
    
    public static final String FIX_MISSING_TO_PART = "FIX_MISSING_TO_PART";
    
//    /** Incorrect from format, invalid container */
//    public static final String VAL_INVALID_FROM_CONTAINER = "VAL_INVALID_FROM_CONTAINER";  // Not I18N
//    
//    /** Please define a valid from format container */
//    public static final String FIX_INVALID_FROM_CONTAINER = "FIX_INVALID_FROM_CONTAINER";  // Not I18N
//    
    
    /** Incomplete wait */
    public static final String VAL_INCOMPLETE_ALARM = "VAL_INCOMPLETE_ALARM";  // Not I18N
    
    /** Please complete wait */
    public static final String FIX_INCOMPLETE_ALARM = "FIX_INCOMPLETE_ALARM";  // Not I18N
    
    /** wait's for attribute is not a valid xpath expression */
    public static final String VAL_INCORRECT_WAIT_FOR_EXPRESSION = "VAL_INCORRECT_WAIT_FOR_EXPRESSION";  // Not I18N
    public static final String FIX_INCORRECT_WAIT_FOR_EXPRESSION = "FIX_INCORRECT_WAIT_FOR_EXPRESSION";  // Not I18N
    
	
    /** wait's until attribute is not a valid xpath expression */
    public static final String VAL_INCORRECT_WAIT_UNTIL_EXPRESSION = "VAL_INCORRECT_WAIT_UNTIL_EXPRESSION";  // Not I18N
    public static final String FIX_INCORRECT_WAIT_UNTIL_EXPRESSION = "FIX_INCORRECT_WAIT_UNTIL_EXPRESSION";  // Not I18N
    
    /** import location is not valid*/
    public static final String VAL_INVALID_IMPORT_TYPE = "VAL_INVALID_IMPORT_TYPE";
    public static final String FIX_INVALID_IMPORT_TYPE = "FIX_INVALID_IMPORT_TYPE";
    
    /** wsdl import location is not valid*/
    public static final String VAL_INVALID_IMPORTED_WSDL_DOCUMENT_LOCATION = "VAL_INVALID_IMPORTED_WSDL_DOCUMENT_LOCATION";
    public static final String FIX_INVALID_IMPORTED_WSDL_DOCUMENT_LOCATION = "FIX_INVALID_IMPORTED_WSDL_DOCUMENT_LOCATION";
    
    /** xsd import location is not valid*/
    public static final String VAL_INVALID_IMPORTED_XSD_DOCUMENT_LOCATION = "VAL_INVALID_IMPORTED_XSD_DOCUMENT_LOCATION";
    public static final String FIX_INVALID_IMPORTED_XSD_DOCUMENT_LOCATION = "FIX_INVALID_IMPORTED_XSD_DOCUMENT_LOCATION";
    
    /**Catch has faultVariable specified and it is not referencable */
    public static final String VAL_MISSING_FAULTVARIABLE_IN_CATCH = "VAL_MISSING_FAULTVARIABLE_IN_CATCH"; 
    public static final String FIX_MISSING_FAULTVARIABLE_IN_CATCH = "FIX_MISSING_FAULTVARIABLE_IN_CATCH"; 
    
    /** wait for and until not specified**/
    public static final String VAL_INCOMPLETE_WAIT = "VAL_INCOMPLETE_WAIT";
    public static final String FIX_INCOMPLETE_WAIT = "FIX_INCOMPLETE_WAIT";
    
    /**catch: both faultMessageType and faultElement are specified**/
    public static final String VAL_BOTH_FAULT_MESSAGE_TYPE_AND_FAULT_ELEMENT_ARE_PRESENT = "VAL_BOTH_FAULT_MESSAGE_TYPE_AND_FAULT_ELEMENT_ARE_PRESENT";
    public static final String FIX_BOTH_FAULT_MESSAGE_TYPE_AND_FAULT_ELEMENT_ARE_PRESENT = "FIX_BOTH_FAULT_MESSAGE_TYPE_AND_FAULT_ELEMENT_ARE_PRESENT";
    
    /** catch: faultMessageType should accompany faultVariable**/
    public static final String VAL_FAULT_MESSAGE_TYPE_SHOULD_ACCOMPANY_WITH_FAULT_VARIABLE = "VAL_FAULT_MESSAGE_TYPE_SHOULD_ACCOMPANY_WITH_FAULT_VARIABLE";
    public static final String FIX_FAULT_MESSAGE_TYPE_SHOULD_ACCOMPANY_WITH_FAULT_VARIABLE = "FIX_FAULT_MESSAGE_TYPE_SHOULD_ACCOMPANY_WITH_FAULT_VARIABLE";
    
    /** catch: faultElement should accompany faultVariable**/
    public static final String VAL_FAULT_ELEMENT_SHOULD_ACCOMPANY_WITH_FAULT_VARIABLE = "VAL_FAULT_ELEMENT_SHOULD_ACCOMPANY_WITH_FAULT_VARIABLE";
    public static final String FIX_FAULT_ELEMENT_SHOULD_ACCOMPANY_WITH_FAULT_VARIABLE = "FIX_FAULT_ELEMENT_SHOULD_ACCOMPANY_WITH_FAULT_VARIABLE";
    
    /** The logger. */
    private static final Logger mLogger = Logger.getLogger(ValidateVisitor.class.getName());
    
    /** MessageManager for localized strings. */
    private static MessageManager mMsg = MessageManager.getManager(ValidateVisitor.class);
    
    /** Validate configuration singleton. */
    private static ValidateConfiguration mValConfig;
    
    /** Indicates whether searching for receive/pick with createInstance = yes */
    private boolean searchForCreateInstance = true;
    
    /** List of receive and pick-onMessage encountered thus far */
    private List receiveOnMessageList = new ArrayList();
    
    /** List of reply's deferred for finding matching receive/pick */
    private List replyDeferredMatchList = new ArrayList();
    
    /** Handle to repository for eInsight license verification */
    
    
    /** List of start activity enabled activities */
    private List startEnabledList = new ArrayList();
    
    /** Creates a new instance of ValidateVisitor */
    public ValidateVisitor() {
        super();
        
        // Find validate configuration
        String valConfigPropFile = System.getProperty("com.stc.bpms.model.visitor.ValidateConfiguration");
        if (valConfigPropFile !=  null) {
            try {
                FileInputStream fis = new FileInputStream(valConfigPropFile);
                ValidateConfiguration valConfig = new ValidateConfiguration();
                valConfig.load(fis);
                
                synchronized (this.getClass()) {
                    mValConfig = valConfig;
                }
            } catch (Exception e) {
                valConfigPropFile = null;
            }
        }
        
        if (null == valConfigPropFile) {
            Properties defaults = new Properties();
            defaults.setProperty(ValidateConfiguration.BPEL_SYNTAX_ATTRIB_REQUIRED, "true");
            defaults.setProperty(ValidateConfiguration.BPEL_SYNTAX_ATTRIB_QNAME, "true");
            defaults.setProperty(ValidateConfiguration.BPEL_SYNTAX_ATTRIB_NCNAME, "false");
            defaults.setProperty(ValidateConfiguration.BPEL_SYNTAX_ATTRIB_BOOLEAN, "true");
            defaults.setProperty(ValidateConfiguration.BPEL_SYNTAX_ELEM_MIN, "true");
            defaults.setProperty(ValidateConfiguration.BPEL_SYNTAX_ELEM_REQUIRED, "true");
            defaults.setProperty(ValidateConfiguration.BPEL_SEMANTICS_CREATE_INSTANCE, "true");
            defaults.setProperty(ValidateConfiguration.BPEL_CONSISTENCY_MATCH_CATCH, "false");
            
            synchronized (this.getClass()) {
                mValConfig = new ValidateConfiguration(defaults);
            }
        }
    }
    
    /** Gets the validate visitor support.
     * @return  Visitor support.
     */
    public ValidateSupport getValidateSupport() {
        if (null == getVisitorSupport()) {
            setVisitorSupport(new ValidateSupport(mValConfig));
        }
        return (ValidateSupport) getVisitorSupport();
    }
    
    /** @see Visitor#reset
     */
    public boolean reset() {
        searchForCreateInstance = true;
        receiveOnMessageList.clear();
        replyDeferredMatchList.clear();
        getValidateSupport().clearToDoListeners();
        return true;
    }
    
    /** Prepares the visitor for use.
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
    public void prepare(Object[] v) {
        // 1st argument is collection of ToDoListeners
        Collection toDoListeners = (Collection) v[0];
        Iterator iter = toDoListeners.iterator();
        while (iter.hasNext()) {
            getValidateSupport().addToDoListener((ToDoListener) iter.next());
        }
        if ((v.length > 1) && (v[1] != null) && (v[1] instanceof ValidateConfiguration)) {
            mValConfig = (ValidateConfiguration) v[1];
        }
    }
    
    /** Concludes the visitor after use.
     */
    public void conclude() {
        // Re-assert any deferred reply's for matching with receive or onMessage
        if (replyDeferredMatchList.size() > 0) {
            for (int i = 0, n = replyDeferredMatchList.size(); i < n; i++) {
                if (!assertReplyMatchReceiveOnMessage(((Reply) replyDeferredMatchList.get(i)), true)) {
                    break;
                }
            }
        }
        getValidateSupport().handleCreateInstance(this.mBpelDocument.getDocumentProcess(),this.startEnabledList,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        // Deregister listeners
        getValidateSupport().clearToDoListeners();
    }
    
    /** Visits a BPEL document.
     * @param   d   a BPEL document.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(BPELDocument d) {
        this.mBpelDocument = d;
        return true;
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
     *
     */
    public boolean visit(XMLText t) {
        return true;
    }
    
    /** Visits a process element.
     * @param   p a process element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(BPELProcess p) {
    	
    	getValidateSupport().assertBPELProcessNamespace(p);
    	
        getValidateSupport().assertRequiredAttrib(p.getName(), BPELProcess.ATTR.NAME,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertNCNameAttrib(p.getName(), BPELProcess.ATTR.NAME,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertRequiredAttrib(p.getTargetNamespace(), BPELProcess.ATTR.TARGET_NAMESPACE,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertBooleanAttrib(p.getSuppressJoinFailure(),
                BPELProcess.ATTR.SUPPRESS_JOIN_FAILURE,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertBooleanAttrib(p.getEnableInstanceCompensation(),
                BPELProcess.ATTR.ENABLE_INSTANCE_COMPENSATION,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertBooleanAttrib(p.getAbstractProcess(), BPELProcess.ATTR.ABSTRACT_PROCESS,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /** Tests if an activity is a structured activity for the purpose of starting
     * a business process instance.
     * @param   a   Activity to test
     * @return  <code>true</code> if such.
     */
    private boolean isStructuredActivity(Activity a) {
        return ((a instanceof Sequence) || (a instanceof Flow) || (a instanceof Scope));
    }
    
    /** Asserts the first substantive activity is a receive or pick with createInstance
     * set to "yes".
     * @param   a   Activity to begin search.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    private boolean assertCreateInstanceActivity(Activity a) {
        if (!mValConfig.getBooleanProperty(ValidateConfiguration.BPEL_SEMANTICS_CREATE_INSTANCE)) {
            return true;
        }
        
        //TODO: terminationHandler is present only at the scope level (and not at the 
        //process level). So not sure whether we need to search within the terminationHandler.
        if (searchForCreateInstance && !isWithinFaultCompensationTerminationHandler(a)) {
            Activity first = null;
            if (isStructuredActivity(a)) {
                if (a instanceof SingleActivityHolder) {
                    first = ((SingleActivityHolder) a).getActivity();
                } else if ((a instanceof MultipleActivityHolder)
                && (((MultipleActivityHolder) a).getActivitySize() > 0)) {
                    first = ((MultipleActivityHolder) a).getActivity(0);
                }
                
                if ((null == first) || isStructuredActivity(first)) {
                    return true;
                }
            } else {
                first = a;
            }
            if (first instanceof While) {
                return true;
            }
            boolean found = false;
            if (first instanceof Receive) {
                found = "yes".equals(((Receive) first).getCreateInstance());
            } else if (first instanceof Pick) {
                found = "yes".equals(((Pick) first).getCreateInstance());
            }
            
            searchForCreateInstance = false;
            
            //TODO:RA check if this is a valid case
            
            if (!found) {
                return getValidateSupport().fireToDo(new ToDoEvent(
                        a, ToDoEvent.Category.BPEL_SEMANTICS, ToDoEvent.Severity.ERROR,
                        mMsg.getString(VAL_FIRST_ACTIVITY_NOT_CREATE_INSTANCE),
                        mMsg.getString(FIX_FIRST_ACTIVITY_NOT_CREATE_INSTANCE)
                        ));
            }
            
        }
        return true;
    }
    
    /** Tests if node is within a fault or compensation or termination handler.
     * @param   n   Node to test.
     * @return  <code>true</code> if it is.
     */
    private boolean isWithinFaultCompensationTerminationHandler(XMLNode n) {
        XMLNode parent = n.getParent();
        if ((parent != null) && !(parent instanceof XMLDocument)) {
            if ((parent instanceof FaultHandlers) || (parent instanceof CompensationHandler)
            		|| (parent instanceof TerminationHandler)) {
                return true;
            } else {
                return isWithinFaultCompensationTerminationHandler(parent);
            }
        }
        return false;
    }
    
    /** Visits a partners element.
     * @param   p   a partners element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(PartnerLinks p) {
        getValidateSupport().assertMinimumElem(p.getPartnerLinksSize(), 1, PartnerLink.TAG,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /** Visits a partner element.
     * @param   p   a partner element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(PartnerLink p) {
        getValidateSupport().assertRequiredAttrib(p.getName(), PartnerLink.ATTR.NAME,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(p.getName(), PartnerLink.ATTR.NAME,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertRequiredAttrib(p.getName(), PartnerLink.ATTR.PARTNER_LINK_TYPE,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertRequiredQNameAttrib(p.getPartnerLinkType(), PartnerLink.ATTR.PARTNER_LINK_TYPE,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertQNameAttrib(p.getPartnerLinkType().toString(), PartnerLink.ATTR.PARTNER_LINK_TYPE,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(p.getMyRole(), PartnerLink.ATTR.MY_ROLE,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(p.getPartnerRole(), PartnerLink.ATTR.PARTNER_ROLE,
                p, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertServiceLinkType(p.getPartnerLinkType(), p, this.mBpelDocument,ToDoEvent.Category.BPEL_SEMANTICS);
        
        if(p.getMyRole() != null) {
        	getValidateSupport().assertServiceLinkTypeRole(p.getMyRole(), p, this.mBpelDocument,ToDoEvent.Category.BPEL_SEMANTICS);
        }
        
        if(p.getPartnerRole() != null) {
        	getValidateSupport().assertServiceLinkTypeRole(p.getPartnerRole(), p, this.mBpelDocument,ToDoEvent.Category.BPEL_SEMANTICS);
        }
        
        return true;
    }
    
    /** Visits a containers element.
     * @param   c   a containers element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Variables c) {
        getValidateSupport().assertMinimumElem(c.getVariableSize(), 1, Variable.TAG,
                c, ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /** Visits a container element.
     * @param   c   a container element.
     * @return  <code>true</code> if traversal is to continue.
     */
    public boolean visit(Variable c) {
        getValidateSupport().assertRequiredAttrib(c.getName(), Variable.ATTR.NAME,
                c, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(c.getName(), Variable.ATTR.NAME,
                c, ToDoEvent.Category.BPEL_SYNTAX);
        
        if(c.getMessageType() != null) {
	        getValidateSupport().assertQNameAttrib(c.getMessageType().toString(), Variable.ATTR.MESSAGE_TYPE,
	                c, ToDoEvent.Category.BPEL_SYNTAX);
	        
	        
	        getValidateSupport().isValidMessageType(c.getMessageType().toString(), Variable.ATTR.NAME,
	                c, ToDoEvent.Category.BPEL_SEMANTICS);
        }
        
        return true;
    }
    
    /** Visits a correlationSets element.
     * @param   c   a correlationSets element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CorrelationSets c) {
        getValidateSupport().assertMinimumElem(c.getCorrelationSetSize(), 1, CorrelationSet.TAG,
                c, ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /** Visits a correlationSet element.
     * @param   c   a correlationSet element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CorrelationSet c) {
        getValidateSupport().assertRequiredAttrib(c.getName(), CorrelationSet.ATTR.NAME,
                c, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(c.getName(), CorrelationSet.ATTR.NAME,
                c, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertRequiredQNameAttribArray(c.getProperties(), CorrelationSet.ATTR.PROPERTIES,
                c, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertQNameArrayAttrib(c.getProperties(), CorrelationSet.ATTR.PROPERTIES,
                c, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertSemanticallyCorrectCorrelationSet(c.getName(), CorrelationSet.TAG, c,
                ToDoEvent.Category.BPEL_SEMANTICS);
        
        
        return true;
    }
    
    /** Visits a faultHandlers element.
     * @param   f   a faultHandlers element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(FaultHandlers f) {
        return true;
    }
    
    /** Visits a catch element.
     * @param   c   a catch element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Catch c) {
    	
    	//check if catch is empty
    	getValidateSupport().isValidCatch(c, ToDoEvent.Category.BPEL_SEMANTICS);
    	
    	//faultName is optional
    	if(c.getFaultName() != null) {
    		//if fault name is present then check if it is a QName
	        getValidateSupport().assertQNameAttrib(c.getFaultName(), Catch.ATTR.FAULT_NAME,
	                c, ToDoEvent.Category.BPEL_SYNTAX);
	        
	        //if fault name is present make sure it is valid.
	        //bpel20 spec:
	        //Each catch activity is defined to intercept a specific kind of fault, 
	    	//defined by a globally unique fault QName and a variable for the data associated with the fault. 
	    	//If the fault name is missing, then the catch will intercept all faults with the right 
	    	//type of fault data
	        getValidateSupport().assertIsValidFaultName(c.getFaultName(), Catch.ATTR.FAULT_NAME, c,
	                ToDoEvent.Category.BPEL_SEMANTICS);
	        
    	}
    	
    	//check if faultVariable is valid NCName
        getValidateSupport().assertNCNameAttrib(c.getFaultVariable(), Catch.ATTR.FAULT_CONTAINER,
                c, ToDoEvent.Category.BPEL_SYNTAX);
        
        //validate to ensure that only one of faultElement and faultMessageType
        //is present not both
        if(c.getFaultMessageType() != null && c.getFaultElement() != null) {
        	getValidateSupport().fireToDo(new ToDoEvent(c, 
					   ToDoEvent.Category.BPEL_SYNTAX, 
					   Severity.ERROR, 
					   this.mMsg.getString(VAL_BOTH_FAULT_MESSAGE_TYPE_AND_FAULT_ELEMENT_ARE_PRESENT, 
					   c.getFaultMessageType(), c.getFaultElement(), c) , 
					   this.mMsg.getString(FIX_BOTH_FAULT_MESSAGE_TYPE_AND_FAULT_ELEMENT_ARE_PRESENT)));
        } else if(c.getFaultMessageType() != null && c.getBPELFaultVariable() == null) {
        	//if fault Message Type is present then fault variable should be present
        	getValidateSupport().fireToDo(new ToDoEvent(c, 
					   ToDoEvent.Category.BPEL_SYNTAX, 
					   Severity.ERROR, 
					   this.mMsg.getString(VAL_FAULT_MESSAGE_TYPE_SHOULD_ACCOMPANY_WITH_FAULT_VARIABLE,
					   c.getFaultMessageType(), c), 
					   this.mMsg.getString(FIX_FAULT_MESSAGE_TYPE_SHOULD_ACCOMPANY_WITH_FAULT_VARIABLE)));
        } else if(c.getFaultElement() != null && c.getBPELFaultVariable() == null) {
        	//if fault Message Type is present then fault variable should be present
        	getValidateSupport().fireToDo(new ToDoEvent(c, 
					   ToDoEvent.Category.BPEL_SYNTAX, 
					   Severity.ERROR, 
					   this.mMsg.getString(VAL_FAULT_ELEMENT_SHOULD_ACCOMPANY_WITH_FAULT_VARIABLE, 
					   c.getFaultElement(), c) , 
					   this.mMsg.getString(FIX_FAULT_ELEMENT_SHOULD_ACCOMPANY_WITH_FAULT_VARIABLE)));
        }
        
        
        //faultVariable is optional
        if(c.getFaultVariable() != null) {
        	//if fault variable is specified, make sure it is referenceable
        	if(c.getBPELFaultVariable() == null) {
             getValidateSupport().fireToDo(new ToDoEvent(c, 
            		 					   ToDoEvent.Category.BPEL_SYNTAX, 
            		 					   Severity.ERROR, 
            		 					   this.mMsg.getString(VAL_MISSING_FAULTVARIABLE_IN_CATCH, 
            		 					   c.getFaultVariable(), 
            		 					   c) , 
            		 					   this.mMsg.getString(FIX_MISSING_FAULTVARIABLE_IN_CATCH)));
            
	        } else {
	        	
	        	
	        }
        }
        
        
        
        return true;
    }
    
    /** Asserts that a catch matches a fault thrown within the scope.
     * @param   c   catch to assert.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    private boolean assertCatchMatchesFault(Catch c) {
        if (!mValConfig.getBooleanProperty(ValidateConfiguration.BPEL_CONSISTENCY_MATCH_CATCH)) {
            return true;
        }
        
        if(c.getFaultName() == null) {
        	return true;
        }
        
        String faultQNameString = c.getFaultName().toString();
        
        if ((c.getParent() != null) 
        	&& !ValidateSupport.isAttributeAbsent(faultQNameString)) {
            if (c.getParent() instanceof Invoke) {
                Invoke invokeScope = (Invoke) c.getParent();
                Operation operation = invokeScope.getWSDLOperation();
                if ((operation != null) && (findOperationFault(operation, faultQNameString) == null)) {
                    return getValidateSupport().fireToDo(
                            new ToDoEvent(c, ToDoEvent.Category.BPEL_SEMANTICS, ToDoEvent.Severity.WARNING,
                            mMsg.getString(VAL_FAULT_NOT_THROWN_IN_SCOPE, faultQNameString),
                            mMsg.getString(FIX_FAULT_NOT_THROWN_IN_SCOPE, faultQNameString)));
                }
            } else if (c.getParent() instanceof FaultHandlers) {
                XMLElement scopabableContainer = (XMLElement) c.getParent().getParent();
                if (scopabableContainer != null) {
                    Activity opActivity = null;
                    if (scopabableContainer instanceof BPELProcess) {
                        opActivity = ((BPELProcess) scopabableContainer).getActivity();
                    } else if (scopabableContainer instanceof Scope) {
                        opActivity = ((Scope) scopabableContainer).getActivity();
                    }
                    if (opActivity != null) {
                        Collection operations = new ArrayList();
                        operations = getAllOperations(opActivity);
                        Iterator iter = operations.iterator();
                        boolean found = false;
                        while (iter.hasNext()) {
                            if (findOperationFault((Operation) iter.next(), faultQNameString) != null) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            return getValidateSupport().fireToDo(
                                    new ToDoEvent(c, ToDoEvent.Category.BPEL_SEMANTICS, ToDoEvent.Severity.WARNING,
                                    mMsg.getString(VAL_FAULT_NOT_THROWN_IN_SCOPE, faultQNameString),
                                    mMsg.getString(FIX_FAULT_NOT_THROWN_IN_SCOPE, faultQNameString)));
                        }
                    }
                }
            }
        }
        return true;
    }
    
    /** Find fault in operation.
     * @param   op  Operation to search.
     * @param   fault   Fault name to find.
     * @return  Fault if found or <code>null</code> if not.
     */
    public static Fault findOperationFault(Operation op, String fault) {
        return op.getFault(fault);
    }
    
    /** Find all operations in the element.
     * @param   elem        Element to search (ex. sequence, flow).
     * @param   results     A modifiable collection to place results.
     * @return  Collection of <code>Operation</code> objects.
     */
    public static Collection getAllOperations(XMLElement elem) {
    	ArrayList results = new ArrayList();
    	
        Operation operObj = null;
        if (elem.hasChildren()) {
            List kids = elem.getChildren();
            for (int k = 0, n = kids.size(); k < n; k++) {
            	results.addAll(getAllOperations((XMLElement) kids.get(k)));
            }
        } else if (elem instanceof Receive) {
            operObj = ((Receive) elem).getWSDLOperation();
        } else if (elem instanceof Reply) {
            operObj = ((Reply) elem).getWSDLOperation();
        } else if (elem instanceof Invoke) {
            operObj = ((Invoke) elem).getWSDLOperation();
        } else if (elem instanceof OnMessage) {
            operObj = ((OnMessage) elem).getWSDLOperation();
        }
        if (operObj != null) {
            results.add(operObj);
        }
        return results;
    }
    
    /** Visits a catchAll element.
     * @param   c   a catchAll element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CatchAll c) {
        getValidateSupport().isValidCatchAll(c, ToDoEvent.Category.BPEL_SEMANTICS);
        
        return true;
    }
    
    /** Visits a compensationHandler element.
     * @param   c   a compensationHandler element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(CompensationHandler c) {
        getValidateSupport().isValidCompensationHandler(c, ToDoEvent.Category.BPEL_SEMANTICS);
        
        return true;
    }
    
    /** Visits a terminationHandler element.
     * @param   t   a terminationHandler element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(TerminationHandler t) {
        getValidateSupport().isValidTerminationHandler(t, ToDoEvent.Category.BPEL_SEMANTICS);
        
        return true;
    }
    
    private void assertPartnerExists(Activity activity, String part) {
        PartnerLinks partners = mBpelDocument.getDocumentProcess().getPartnerLinks();
        PartnerLink partner = partners.getPartnerLink(part);
        if (partner == null) {
            String activityName = activity.getName();
            if (activityName == null) {
                activityName = "Unknown";
            }
            getValidateSupport().fireToDo(
                    new ToDoEvent(activity, ToDoEvent.Category.BPEL_SEMANTICS, ToDoEvent.Severity.ERROR,
                    mMsg.getString(PARTNER_DOES_NOT_EXIST, activityName, part),
                    mMsg.getString(FIX_PARTNER_DOES_NOT_EXIST, part)));
        }
    }
    
    /** Visits a receive element.
     * @param   r   a receive element.
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Receive r) {
        getValidateSupport().assertRequiredAttrib(r.getPartnerLink(), Receive.ATTR.PARTNERLINK, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertNCNameAttrib(r.getPartnerLink(), Receive.ATTR.PARTNERLINK, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertRequiredQNameAttrib(r.getPortType(), Receive.ATTR.PORT_TYPE, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertQNameAttrib(r.getPortType().toString(), Receive.ATTR.PORT_TYPE, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertRequiredAttrib(r.getOperation(), Receive.ATTR.OPERATION, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(r.getOperation(), Receive.ATTR.OPERATION, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertRequiredAttrib(r.getVariable(), Receive.ATTR.VARIABLE, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(r.getVariable(), Receive.ATTR.VARIABLE, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertBooleanAttrib(r.getCreateInstance(), Receive.ATTR.CREATE_INSTANCE, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
    
        getValidateSupport().assertSemanticallyCorrectAttrib(r.getPortType().toString(),
        													 r.getOperation(),
        												     Receive.ATTR.PORT_TYPE, 
        												     r,
        												     r.getWSDLPortType(),
        												     r.getWSDLOperation(),
        												     ToDoEvent.Category.BPEL_SEMANTICS);
        
        
        assertPartnerExists(r, r.getPartnerLink());
        
        
        if (getValidateSupport().isStartEnabled(r, ToDoEvent.Category.BPEL_SYNTAX)) {
            startEnabledList.add(r);
        }
        
        getValidateSupport().assertContainer(r, this.mBpelDocument, r.getVariable(), ToDoEvent.Category.BPEL_SEMANTICS);
        
        receiveOnMessageList.add(r);
        return true;
    }
    
    /** Visits a target element.
     * @param   t   a target element.
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Target t) {
        getValidateSupport().assertRequiredAttrib(t.getLinkName(), Target.ATTR.LINK_NAME, t,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /** Visits a source element.
     * @param   s   a source element.
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Source s) {
        getValidateSupport().assertRequiredAttrib(s.getLinkName(), Source.ATTR.LINK_NAME, s,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /** Visits a correlations element.
     * @param   c   a correlations element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Correlations c) {
        getValidateSupport().assertMinimumElem(c.getCorrelationSize(), 1, Correlation.TAG, c,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        /*
          if (!getValidateSupport().assertSematicallyCorrectCorrelations(c.getCorrelationSize(), 1, Correlation.TAG, c,
          ToDoEvent.Category.BPEL_SEMANTICS)) {
          return false;
          }
         */
        return true;
    }
    
    /** Visits a correlation element.
     * @param   c   a correlation element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Correlation c) {
        getValidateSupport().assertRequiredAttrib(c.getSet(), Correlation.ATTR.SET, c,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(c.getSet(), Correlation.ATTR.SET, c,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertEnumeratedAttrib(c.getInitiate(), 
        											c.getXmlAttributes()[Correlation.INITIATE].getEnumValues(), 
        											Correlation.ATTR.INITIATE, 
        											c,
        											ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertEnumeratedAttrib(c.getPattern(),
                c.getXmlAttributes()[Correlation.PATTERN].getEnumValues(),
                Correlation.ATTR.PATTERN, c, ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /** Visits a sequence element.
     * @param   s   a sequence element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Sequence s) {
        assertCreateInstanceActivity(s);
        
        this.getValidateSupport().isAValidSequence(s, Sequence.TAG, ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /** Visits a flow element.
     * @param   f   a flow element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Flow f) {
        assertCreateInstanceActivity(f);
        
        return true;
    }
    
    /** Visits a links element.
     * @param   l   a links element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Links l) {
        getValidateSupport().assertMinimumElem(l.getLinkSize(), 1, Link.TAG,
                l, ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /** Visits a link element.
     * @param   l   a link element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Link l) {
        getValidateSupport().assertRequiredAttrib(l.getName(), Link.ATTR.NAME, l,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /** Visits a switch element.
     * @param   s   a switch element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Switch s) {
        getValidateSupport().assertMinimumElem(s.getCaseSize(), 1, Case.TAG,
                s, ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /** Visits a case element.
     * @param   c   a case element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Case c) {
        getValidateSupport().assertRequiredElement(c.getBPELCondition(), Condition.TAG,
                c, ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
        //if (! getValidateSupport().assertValidContainer(c.getC
    }
    
    /** Visits a otherwise element.
     * @param   o   a otherwise element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(Otherwise o) {
        return true;
    }
    
    /** Visits a assign element.
     * @param   a   a assign element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Assign a) {
        /*return getValidateSupport().assertMinimumElem(a.getCopySize(), 1, Copy.TAG,
          a, ToDoEvent.Category.BPEL_SYNTAX);*/
        this.getValidateSupport().hasMinimumElements(a, ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /** Visits a copy element.
     * @param   c   a copy element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Copy c) {
        getValidateSupport().assertRequiredElement(c.getFrom(), From.TAG,
                c, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertRequiredElement(c.getTo(), To.TAG,
                c, ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /** Visits a from element.
     * @param   f   a from element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(From f) {
        // Only allow the valid forms for From
        boolean validForm = false;
        
        //from container is invalid
        String container = f.getVariable();
        if(container != null && container.trim().length() != 0) {
        	//if container attribute is specified make sure it is valid
        	getValidateSupport().assertContainer(f, this.mBpelDocument, container, ToDoEvent.Category.BPEL_SEMANTICS);
        	
        	//from container and part are present make sure container has that part.
            String part = f.getPart();
            if (part != null && part.trim().length() != 0 ) {
            	getValidateSupport().assertContainerPart(f, this.mBpelDocument, container, part, ToDoEvent.Category.BPEL_SEMANTICS);
            }
        }
        
        
        //from container missing but part  is present
        if (ValidateSupport.isAttributeAbsent(f.getVariable())
        	&& (!ValidateSupport.isAttributeAbsent(f.getPart()))
			) {
        	getValidateSupport().fireToDo(
                    new ToDoEvent(f, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR,
                    mMsg.getString(VAL_MISSING_FROM_CONTAINER),
                    mMsg.getString(FIX_MISSING_FROM_CONTAINER)));
            
        }
        
        //TODO: add validation for expression with $var format
        
        // from container format
        if (!ValidateSupport.isAttributeAbsent(f.getVariable())
        && ValidateSupport.isAttributeAbsent(f.getPartnerLink())
        && ValidateSupport.isAttributeAbsent(f.getExpression())
        /*&& ValidateSupport.isAttributeAbsent(f.getOpaque())*/) {
            return true;
            // from partner format
        } else if (!ValidateSupport.isAttributeAbsent(f.getPartnerLink())
        && ValidateSupport.isAttributeAbsent(f.getVariable())
        && ValidateSupport.isAttributeAbsent(f.getExpression())
        /*&& ValidateSupport.isAttributeAbsent(f.getOpaque())*/) {
            //partner is present (so should have serviceReference present)
        	//container, expression and opaque is missing
        	if (ValidateSupport.isAttributeAbsent(f.getEndPointReference())) {
                return getValidateSupport().fireToDo(
                        new ToDoEvent(f, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR,
                        mMsg.getString(VAL_INCORRECT_FROM_PARTNER_FMT),
                        mMsg.getString(FIX_INCORRECT_FROM_PARTNER_FMT)));
            }
            return getValidateSupport()
            .assertEnumeratedAttrib(f.getEndPointReference(),
                    f.getXmlAttributes()[From.ENDPOINT_REFERENCE].getEnumValues(),
                    From.ATTR.ENDPOINT_REFERENCE, f, ToDoEvent.Category.BPEL_SYNTAX);
        } else if (f.hasChildren()) {
            return true;
            // from expression format
        } else if (!ValidateSupport.isAttributeAbsent(f.getExpression())
        && ValidateSupport.isAttributeAbsent(f.getVariable())
        && ValidateSupport.isAttributeAbsent(f.getPartnerLink())
        /*&& ValidateSupport.isAttributeAbsent(f.getOpaque())*/) {
            
        	return true;
            // from opaque format
        } else if (/*!ValidateSupport.isAttributeAbsent(f.getOpaque())
        &&*/ ValidateSupport.isAttributeAbsent(f.getVariable())
        && ValidateSupport.isAttributeAbsent(f.getPartnerLink())
        && ValidateSupport.isAttributeAbsent(f.getExpression())) {
            return true;
        }
        // Not a valid format
        getValidateSupport().fireToDo(
                new ToDoEvent(f, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR,
                mMsg.getString(VAL_INCORRECT_FROM_FMT),
                mMsg.getString(FIX_INCORRECT_FROM_FMT)));
    
        return true;
    }
    
    /** Visits a to element.
     * @param   t   a to element.
     * @return <tt>true</tt> if auto-traversal is to continue.
     *
     */
    public boolean visit(To t) {
    	//to container is invalid
        String container = t.getVariable();
        if(container != null && container.trim().length() != 0) {
        	//if container attribute is specified make sure it is valid
        	getValidateSupport().assertContainer(t, this.mBpelDocument, container, ToDoEvent.Category.BPEL_SEMANTICS);
        	
        	//to container and part are present make sure container has that part.
            String part = t.getPart();
            if (part != null && part.trim().length() != 0 ) {
            	getValidateSupport().assertContainerPart(t, this.mBpelDocument, container, part, ToDoEvent.Category.BPEL_SEMANTICS);
            }
        }
        
        
        //to container missing but part is present
        if (ValidateSupport.isAttributeAbsent(t.getVariable())
        	&& (!ValidateSupport.isAttributeAbsent(t.getPart()))
			) {
        	getValidateSupport().fireToDo(
                    new ToDoEvent(t, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR,
                    mMsg.getString(VAL_MISSING_TO_CONTAINER),
                    mMsg.getString(FIX_MISSING_TO_CONTAINER)));
            
        }
        
        
    	return true;
    }
    
    /** Visits a reply element.
     * @param   r   a reply element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Reply r) {
        getValidateSupport().assertRequiredAttrib(r.getPartnerLink(), Reply.ATTR.PARTNERLINK, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(r.getPartnerLink(), Reply.ATTR.PARTNERLINK, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertRequiredQNameAttrib(r.getPortType(), Reply.ATTR.PORT_TYPE, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertQNameAttrib(r.getPortType().toString(), Reply.ATTR.PORT_TYPE, r,
                ToDoEvent.Category.BPEL_SYNTAX);
    
        
        getValidateSupport().assertRequiredAttrib(r.getOperation(), Reply.ATTR.OPERATION, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(r.getOperation(), Reply.ATTR.OPERATION, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertRequiredAttrib(r.getVariable(), Reply.ATTR.VARIABLE, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(r.getVariable(), Reply.ATTR.VARIABLE, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertQNameAttrib(r.getFaultName(), Reply.ATTR.FAULT_NAME, r,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertSemanticallyCorrectAttrib(r.getPortType().toString(),
        													 r.getOperation(),
        													 Reply.ATTR.PORT_TYPE, 
        													 r,
        													 r.getWSDLPortType(),
        													 r.getWSDLOperation(),
        													 ToDoEvent.Category.BPEL_SEMANTICS);
        
        assertPartnerExists(r, r.getPartnerLink());
        
        if (!assertReplyMatchReceiveOnMessage(r, false)) {
            // defer asserting reply till all receives and onMessages have been read in
            replyDeferredMatchList.add(r);
        }
        
        getValidateSupport().assertContainer(r, this.mBpelDocument, r.getVariable(), ToDoEvent.Category.BPEL_SEMANTICS);
        
        return true;
    }
    
    /**
     * Assert whether reply match an existing receive or pick onMessage.
     * @param   r           reply to assert.
     * @param   finalCheck  final check: <code>true</code> if final (last) check to be made.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    private boolean assertReplyMatchReceiveOnMessage(Reply r, boolean finalCheck) {
        
        // Find a match within list of receive/onMessage encountered
        boolean found = false;
        for (int i = 0, n = receiveOnMessageList.size(); i < n; i++) {
            if (receiveOnMessageList.get(i) instanceof Receive) {
                Receive receive = (Receive) receiveOnMessageList.get(i);
                if ((found = (Utility.areEqual(receive.getPartnerLink(), r.getPartnerLink())
                	&& receive.getPortType() != null 
                	&& r.getPortType() != null 
                	&& Utility.areEqual(receive.getPortType(), r.getPortType())
                && Utility.areEqual(receive.getOperation(), r.getOperation())))) {
                    break;
                }
            } else if (receiveOnMessageList.get(i) instanceof OnMessage) {
                OnMessage onMessage = (OnMessage) receiveOnMessageList.get(i);
                if ((found = (Utility.areEqual(onMessage.getPartnerLink(), r.getPartnerLink())
                && onMessage.getPortType() != null 
                && r.getPortType()!= null 
                && Utility.areEqual(onMessage.getPortType(), r.getPortType())
                && Utility.areEqual(onMessage.getOperation(), r.getOperation())))) {
                    break;
                }
            }
        }
        
        if (!found && finalCheck) {
            return getValidateSupport().fireToDo(
                    new ToDoEvent(r, ToDoEvent.Category.BPEL_SEMANTICS, ToDoEvent.Severity.ERROR,
                    mMsg.getString(VAL_REPLY_WITHOUT_MATCH),
                    mMsg.getString(FIX_REPLY_WITHOUT_MATCH)));
        }
        
        return found;
    }
    
    /** Visits a invoke element.
     * @param   i   a invoke element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Invoke i) {
        getValidateSupport().assertRequiredAttrib(i.getPartnerLink(), Invoke.ATTR.PARTNERLINK, i,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertNCNameAttrib(i.getPartnerLink(), Invoke.ATTR.PARTNERLINK, i,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertRequiredQNameAttrib(i.getPortType(), Invoke.ATTR.PORT_TYPE, i,
                ToDoEvent.Category.BPEL_SYNTAX);
    
	if(i.getPortType() != null) {    
            getValidateSupport().assertQNameAttrib(i.getPortType().toString(), Invoke.ATTR.PORT_TYPE, i,
                    ToDoEvent.Category.BPEL_SYNTAX);
        }
        
        getValidateSupport().assertRequiredAttrib(i.getOperation(), Invoke.ATTR.OPERATION, i,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(i.getOperation(), Invoke.ATTR.OPERATION, i,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertRequiredAttrib(i.getInputVariable(), Invoke.ATTR.INPUT_VARIABLE, i,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(i.getInputVariable(), Invoke.ATTR.INPUT_VARIABLE, i,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(i.getOutputVariable(), Invoke.ATTR.OUTPUT_VARIABLE, i,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertSemanticallyCorrectAttrib(i.getPortType().toString(),
        											         i.getOperation(),
        												     Invoke.ATTR.PORT_TYPE, 
        												     i,
        												     i.getWSDLPortType(),
        												     i.getWSDLOperation(),
        												     ToDoEvent.Category.BPEL_SEMANTICS);
        
        
        return true;
    }
    
    /** Visits a throw element.
     * @param   t   a throw element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Throw t) {
        getValidateSupport().assertRequiredQNameAttrib(t.getFaultName(), Throw.ATTR.FAULT_NAME, t,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertQNameAttrib(t.getFaultName(), Throw.ATTR.FAULT_NAME, t,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertNCNameAttrib(t.getFaultVariable(), Throw.ATTR.FAULT_VARIABLE, t,
                ToDoEvent.Category.BPEL_SYNTAX);
        
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
    	if (getValidateSupport().isAttributeAbsent(w.getFor())
            && getValidateSupport().isAttributeAbsent(w.getUntil())) {
//    		 Neither for nor until is defined
            return getValidateSupport().fireToDo(
                    new ToDoEvent(w, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR,
                    mMsg.getString(VAL_INCOMPLETE_WAIT, Wait.TAG),
                    mMsg.getString(FIX_INCOMPLETE_WAIT, Wait.TAG)));
            
        }
        
        
        //validate that for or untill value is a valid xpath expression
        if(w.getFor() != null && !w.getFor().trim().equals("")) { 
        	try {
        		validateXPATHExression(w.getFor());
            } catch (XPathException e) {
            	return getValidateSupport().fireToDo(
                        new ToDoEvent(w, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR,
                        mMsg.getString(VAL_INCORRECT_WAIT_FOR_EXPRESSION, new Object[] {Wait.TAG, e.getMessage()}),
                        mMsg.getString(FIX_INCORRECT_WAIT_FOR_EXPRESSION)));
            }
        } 
        
        if (w.getUntil() != null && !w.getUntil().trim().equals("")) {
        	try {
        		validateXPATHExression(w.getUntil());
            } catch (XPathException e) {
            	return getValidateSupport().fireToDo(
                        new ToDoEvent(w, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR,
                        mMsg.getString(VAL_INCORRECT_WAIT_UNTIL_EXPRESSION, new Object[] {Wait.TAG, e.getMessage()}),
                        mMsg.getString(FIX_INCORRECT_WAIT_UNTIL_EXPRESSION)));
            }
        }
        
        return true;
    }
    
    private void validateXPATHExression(String expr) throws XPathException {
    	XPathModel model = AbstractXPathModelHelper.getInstance().newXPathModel();
        XPathExpression xPathExpr = model.parseExpression(expr);
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
        getValidateSupport().assertRequiredElement(w.getBPELCondition(), Condition.TAG, w,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().isValidWhile(w, ToDoEvent.Category.BPEL_SEMANTICS);
        
        return true;
    }
    
    /** Visits a pick element.
     * @param   p   a pick element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Pick p) {
        getValidateSupport().assertBooleanAttrib(p.getCreateInstance(), Pick.ATTR.CREATE_INSTANCE, p,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertMinimumElem(p.getOnMessageSize(), 1, OnMessage.TAG, p,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        if (getValidateSupport().isStartEnabled(p, ToDoEvent.Category.BPEL_SYNTAX)) {
            startEnabledList.add(p);
        }
        
        return true;
    }
    
    /** Visits a onMessage element.
     * @param   o   a onMessage element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(OnMessage o) {
        getValidateSupport().assertRequiredAttrib(o.getPartnerLink(), OnMessage.ATTR.PARTNERLINK, o,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(o.getPartnerLink(), OnMessage.ATTR.PARTNERLINK, o,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertRequiredQNameAttrib(o.getPortType(), OnMessage.ATTR.PORT_TYPE, o,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertNCNameAttrib(o.getPortType().toString(), OnMessage.ATTR.PORT_TYPE, o,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertRequiredAttrib(o.getOperation(), OnMessage.ATTR.OPERATION, o,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertNCNameAttrib(o.getOperation(), OnMessage.ATTR.OPERATION, o,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertRequiredAttrib(o.getVariable(), OnMessage.ATTR.VARIABLE, o,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(o.getVariable(), OnMessage.ATTR.VARIABLE, o,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        if(o.getPortType() != null) {
	        getValidateSupport().assertSemanticallyCorrectAttrib(o.getPortType().toString(),
	        													 o.getOperation(),
	        													 OnMessage.ATTR.PORT_TYPE, 
	        													 o,
	        													 o.getWSDLPortType(),
	        													 o.getWSDLOperation(),
	        													 ToDoEvent.Category.BPEL_SEMANTICS);
        }
        
        receiveOnMessageList.add(o);
        return true;
    }
    
    /** Visits a onAlarm element.
     * @param   o   a onAlarm element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(OnAlarm o) {
        if (getValidateSupport().isAttributeAbsent(o.getFor())
        	&& getValidateSupport().isAttributeAbsent(o.getUntil())) {
        	//Neither for nor until is defined
            return getValidateSupport().fireToDo(
                    new ToDoEvent(o, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR,
                    mMsg.getString(VAL_INCOMPLETE_ALARM, OnAlarm.TAG),
                    mMsg.getString(FIX_INCOMPLETE_ALARM, OnAlarm.TAG)));
            
        }
        
        
//      validate that for or untill value is a valid xpath expression
        if(o.getFor() != null && !o.getFor().trim().equals("")) { 
        	try {
        		validateXPATHExression(o.getFor());
            } catch (XPathException e) {
            	return getValidateSupport().fireToDo(
                        new ToDoEvent(o, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR,
                        mMsg.getString(VAL_INCORRECT_WAIT_FOR_EXPRESSION, new Object[] {OnAlarm.TAG, e.getMessage()}),
                        mMsg.getString(FIX_INCORRECT_WAIT_FOR_EXPRESSION)));
            }
        } 
        
        if (o.getUntil() != null && !o.getUntil().trim().equals("")) {
        	try {
        		validateXPATHExression(o.getUntil());
            } catch (XPathException e) {
            	return getValidateSupport().fireToDo(
                        new ToDoEvent(o, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR,
                        mMsg.getString(VAL_INCORRECT_WAIT_UNTIL_EXPRESSION, new Object[] {OnAlarm.TAG, e.getMessage()}),
                        mMsg.getString(FIX_INCORRECT_WAIT_UNTIL_EXPRESSION)));
            }
        }
        
        return true;
        
    }
    
    /** Visits a scope element.
     * @param   s   a scope element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Scope s) {
        getValidateSupport()
        .assertRequiredAttrib(s.getIsolated(),
                Scope.ATTR.ISOLATED, s, ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport()
        .assertBooleanAttrib(s.getIsolated(),
                Scope.ATTR.ISOLATED, s, ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().isValidScope(s, ToDoEvent.Category.BPEL_SEMANTICS);
        
        
        assertCreateInstanceActivity(s);
        
        return true;
    }
    
    /** Visits a compensate element.
     * @param   c   a compensate element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Compensate c) {
        getValidateSupport().assertNCNameAttrib(c.getScope(), Compensate.ATTR.SCOPE, c,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertScopeNameIsValid(c.getScope(), Compensate.ATTR.SCOPE, c,
                ToDoEvent.Category.BPEL_SEMANTICS);
        
        
        return true;
    }
    
    /**
     * Visits a forEach element
     *
     * @param f a <code>ForEach</code> value
     * @return a <code>boolean</code> value
     */
    public boolean visit(ForEach f) {
        this.getValidateSupport().verifyForEach(f, ToDoEvent.Category.BPEL_SYNTAX);
        
        return true;
    }
    
    /**
     * @see BPELVisitor#visit(ExtensibilityElement)
     */
    public boolean visit(ExtensibilityElement ext) {
        return true;
    }
    
    /** Visits a Import element.
     * @param   p   a Import element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Import imp) {
        getValidateSupport().assertRequiredAttrib(imp.getNamespace(), Import.ATTR.NAMESPACE, imp,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        getValidateSupport().assertRequiredAttrib(imp.getLocation(), Import.ATTR.LOCATION, imp,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        
        getValidateSupport().assertRequiredAttrib(imp.getImportType(), Import.ATTR.IMPORT_TYPE, imp,
                ToDoEvent.Category.BPEL_SYNTAX);
        
        //check if import type is correct.
        if(!Import.WSDL_IMPORT_TYPE.equals(imp.getImportType()) 
           && !Import.XSD_IMPORT_TYPE.equals(imp.getImportType())) {
        	return getValidateSupport().fireToDo(
                    new ToDoEvent(imp, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR,
                    mMsg.getString(VAL_INVALID_IMPORT_TYPE, imp.getImportType()),
                    mMsg.getString(FIX_INVALID_IMPORT_TYPE, Import.WSDL_IMPORT_TYPE, Import.XSD_IMPORT_TYPE)));
        }
        
        //check if import is really valid
        Object importedDocumentObject = imp.getImportedObject();
        if(importedDocumentObject == null) {
        	if(Import.WSDL_IMPORT_TYPE.equals(imp.getImportType())) {
	        	return getValidateSupport().fireToDo(
	                    new ToDoEvent(imp, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR,
	                    mMsg.getString(VAL_INVALID_IMPORTED_WSDL_DOCUMENT_LOCATION, imp.getLocation()),
	                    mMsg.getString(FIX_INVALID_IMPORTED_WSDL_DOCUMENT_LOCATION)));
        	} else {
        		return getValidateSupport().fireToDo(
                        new ToDoEvent(imp, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR,
                        mMsg.getString(VAL_INVALID_IMPORTED_XSD_DOCUMENT_LOCATION, imp.getLocation()),
                        mMsg.getString(FIX_INVALID_IMPORTED_XSD_DOCUMENT_LOCATION)));
            	
        	}
        }
        return true;
    }

    /** Visits a validate element.
     * @param   v validate element.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Validate v) {
        // todo check attributes if required
        return true;
    }

    public boolean visit(If d) {
        // TODO Auto-generated method stub
        return true;
    }
    

    public boolean visit(ElseIf d) {
        // TODO Auto-generated method stub
        return true;
    }

    public boolean visit(Else d) {
        // TODO Auto-generated method stub
        return true;
    }

    public boolean visit(EventHandlersOnEvent onEvent) {
        // TODO Auto-generated method stub
        return false;
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
