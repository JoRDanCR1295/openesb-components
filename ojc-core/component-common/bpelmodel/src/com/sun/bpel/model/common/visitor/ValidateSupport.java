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
 * @(#)ValidateSupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.common.visitor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.StringTokenizer;
import java.util.TreeSet;
import java.util.logging.Logger;

import javax.wsdl.Definition;
import javax.wsdl.Fault;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.XmlNCName;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.Assign;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Case;
import com.sun.bpel.model.Catch;
import com.sun.bpel.model.CatchAll;
import com.sun.bpel.model.Compensate;
import com.sun.bpel.model.CompensationHandler;
import com.sun.bpel.model.TerminationHandler;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.CorrelationSet;
import com.sun.bpel.model.FaultHandlers;
import com.sun.bpel.model.Flow;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.Else;
import com.sun.bpel.model.ElseIf;
import com.sun.bpel.model.If;
import com.sun.bpel.model.OnAlarm;
import com.sun.bpel.model.OnMessage;
import com.sun.bpel.model.Otherwise;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.Switch;
import com.sun.bpel.model.Throw;
import com.sun.bpel.model.While;
import com.sun.bpel.model.common.MessageManager;
import com.sun.bpel.model.extensions.Choose;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.model.util.Utility;
import com.sun.bpel.model.visitor.StandardFaults;
import com.sun.bpel.xml.NamespaceUtility;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.todotask.ToDoEvent;
import com.sun.bpel.xml.common.todotask.ToDoListener;
import com.sun.bpel.xml.common.todotask.ToDoEvent.Category;
import com.sun.bpel.xml.common.todotask.ToDoEvent.Severity;
import com.sun.bpel.xml.common.visitor.VisitorSupport;
import com.sun.bpel.xml.xsd.ValidationUtils;
import com.sun.wsdl4j.ext.bpel.MessageProperty;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;
import com.sun.wsdl4j.ext.bpel.PartnerLinkRole;
import com.sun.wsdl4j.ext.bpel.PartnerLinkType;


/**
 * Supports validation of BPEL/WSDL documents.
 *
 * @author Sun Microsystems
 * @version 
 */
public class ValidateSupport implements VisitorSupport {
    private static final String YES = "yes";
    /** The logger. */
    private static final Logger mLogger = Logger.getLogger(ValidateSupport.class.getName());
    
    /** MessageManager for localized strings. */    
    private static MessageManager mMsg = MessageManager.getManager(ValidateSupport.class);
    
    /** Validate Configuration */
    private ValidateConfiguration mValConfig;
    
    /** Hold stack of original parents */
    private Stack originals = new Stack();
    
    /** Holds to-do listeners */
    private Set toDoListeners = new HashSet();
    
    /** eVision-only legal Page Flow create instance object type */
    private static final String EVISION_CREATE_INSTANCE_TYPE = "link";  // Not I18N
    
    /** receive */
    private static final String ACTTYPE_RECEIVE = "ACTTYPE_RECEIVE";  // Not I18N
    
    /** select a message event */
    private static final String ACTTYPE_PICK = "ACTTYPE_PICK";  // Not I18N
    
    /** Missing required attribute */
    public static final String VAL_MISSING_ATTRIB = "VAL_MISSING_ATTRIB";  // Not I18N
    
    /** Please define required attribute */
    public static final String FIX_MISSING_ATTRIB = "FIX_MISSING_ATTRIB";  // Not I18N
    
    /** Not NCName attribute */
    public static final String VAL_NOT_NCNAME_ATTRIB = "VAL_NOT_NCNAME_ATTRIB";  // Not I18N
    
    /** Please make attribute comply to NCName */
    public static final String FIX_NOT_NCNAME_ATTRIB = "FIX_NOT_NCNAME_ATTRIB";  // Not I18N
    
    /** Not QName attribute */
    public static final String VAL_NOT_QNAME_ATTRIB = "VAL_NOT_QNAME_ATTRIB";  // Not I18N
    
    /** Please make attribute comply to QName */
    public static final String FIX_NOT_QNAME_ATTRIB = "FIX_NOT_QNAME_ATTRIB";  // Not I18N
    
    /** Prefix in QName is null */
    public static final String VAL_PREFIX_NULL_QNAME_ATTRIB = 
        "VAL_PREFIX_NULL_QNAME_ATTRIB";  // Not I18N
    
    /** Please define a prefix in the QName */
    public static final String FIX_PREFIX_NULL_QNAME_ATTRIB = 
        "FIX_PREFIX_NULL_QNAME_ATTRIB";  // Not I18N

    /** Prefix not defined */
    public static final String VAL_PREFIX_NOT_DEFINED = "VAL_PREFIX_NOT_DEFINED";  // Not I18N
    
    /** Please associate prefix */
    public static final String FIX_PREFIX_NOT_DEFINED = "FIX_PREFIX_NOT_DEFINED";  // Not I18N
    
    /** Not boolean attribute */
    public static final String VAL_NOT_BOOLEAN_ATTRIB = "VAL_NOT_BOOLEAN_ATTRIB";  // Not I18N
    
    /** Please make attribute boolean */
    public static final String FIX_NOT_BOOLEAN_ATTRIB = "FIX_NOT_BOOLEAN_ATTRIB";  // Not I18N
    
    /** Not enumerated attribute */
    public static final String VAL_NOT_ENUM_ATTRIB = "VAL_NOT_ENUM_ATTRIB";  // Not I18N
    
    /** Please choose one of enumerated values */
    public static final String FIX_NOT_ENUM_ATTRIB = "FIX_NOT_ENUM_ATTRIB";  // Not I18N
    
    /** Minimum sub-elements not met */
    public static final String VAL_MIN_ELEM_NOT_MET = "VAL_MIN_ELEM_NOT_MET";  // Not I18N
    
    /** Please add more sub-elements */
    public static final String FIX_MIN_ELEM_NOT_MET = "FIX_MIN_ELEM_NOT_MET";  // Not I18N
    
    /** Missing required sub-element */
    public static final String VAL_MISSING_ELEM = "VAL_MISSING_ELEM";  // Not I18N
    
    /** Please define sub-element */
    public static final String FIX_MISSING_ELEM = "FIX_MISSING_ELEM";  // Not I18N
    
    /** Matching WSDL document not found */
    public static final String VAL_MATCHING_WSDL_NOT_FOUND = "VAL_MATCHING_WSDL_NOT_FOUND";  // Not I18N
    
    /** Please import matching WSDL document */
    public static final String FIX_MATCHING_WSDL_NOT_FOUND = "FIX_MATCHING_WSDL_NOT_FOUND";  // Not I18N

    /**Trying to compensate an invalid scope **/
    public static final String VAL_COMPENSATE_WITH_AN_INVALID_SCOPE =
        "VAL_COMPENSATE_WITH_AN_INVALID_SCOPE";  //Not I18N

    /** Fix compensate with an invalid scope **/
    public static final String FIX_COMPENSATE_WITH_AN_INVALID_SCOPE =
        "FIX_COMPENSATE_WITH_AN_INVALID_SCOPE";  //Not I18N

    /** message type in container key*/
    public static final String VAL_MESSAGETYPE_IN_CONTAINER = "VAL_MESSAGETYPE_IN_CONTAINER";  //Not I18N
    /**message type in container fix*/
    public static final String FIX_MESSAGETYPE_IN_CONTAINER = "FIX_MESSAGETYPE_IN_CONTAINER";  //Not I18N
    
    /**missing variable*/    
    public static final String VAL_MISSING_VARIABLE = "VAL_MISSING_VARIABLE";  //Not I18N
    /**missing variable*/    
    public static final String FIX_MISSING_VARIABLE = "FIX_MISSING_VARIABLE";  //Not I18N
    
    
    /**sequence*/    
    public static final String SEQUENCE_TITLE_NAME = "SEQUENCE_TITLE_NAME";  //Not I18N
    /** missing connection */
    public static final String VAL_MISSING_CONNECTIONS = "VAL_MISSING_CONNECTIONS";  //Not I18N
    /**fix missing connection */
    public static final String FIX_MISSING_CONNECTIONS = "FIX_MISSING_CONNECTIONS";  //Not I18N

    /** correct correlation set */
    public static final String VAL_SEMANTICALLY_CORRECT_CORRELATION_SET =
        "VAL_SEMANTICALLY_CORRECT_CORRELATION_SET";  //Not I18N
    /** correct correlation set fix */
    public static final String FIX_SEMANTICALLY_CORRECT_CORRELATION_SET =
        "FIX_SEMANTICALLY_CORRECT_CORRELATION_SET";  //Not I18N
    /** empty fault name */
    public static final String VAL_FAULT_NAME_CANNOT_BE_EMPTY = "VAL_FAULT_NAME_CANNOT_BE_EMPTY";  //Not I18N
    /**empty fault name fix */
    public static final String FIX_FAULT_NAME_CANNOT_BE_EMPTY = "FIX_FAULT_NAME_CANNOT_BE_EMPTY";  //Not I18N
    /** unknown operation */
    public static final String VAL_UNKNOWN_OPERATION = "VAL_UNKNOWN_OPERATION";  //Not I18N
    /** unknown operation fix */
    public static final String FIX_UNKNOWN_OPERATION = "FIX_UNKNOWN_OPERATION";  //Not I18N
    /** unknown port type */
    public static final String VAL_UNKNOWN_PORT_TYPE = "VAL_UNKNOWN_PORT_TYPE";  //Not I18N
    /** unknown port type fix */
    public static final String FIX_UNKNOWN_PORT_TYPE = "FIX_UNKNOWN_PORT_TYPE";  //Not I18N
    /** minimym needed for assign */
    public static final String VAL_ASSIGN_MUST_HAVE_MINIMUM = "VAL_ASSIGN_MUST_HAVE_MINIMUM";  //Not I18N
    /** min fix for assing */
    public static final String FIX_ASSIGN_MUST_HAVE_MINIMUM = "FIX_ASSIGN_MUST_HAVE_MINIMUM";  //Not I18N
    /**
     * min foreach
     */
    public static final String VAL_FOR_EACH_MUST_HAVE_MIN_ONE_COPY =
        "VAL_FOR_EACH_MUST_HAVE_MIN_ONE_COPY";  //Not I18N
    /** for each min fix */
    public static final String FIX_FOR_EACH_MUST_HAVE_MIN_ONE_COPY =
        "FIX_FOR_EACH_MUST_HAVE_MIN_ONE_COPY";  //Not I18N

    public static final String VAL_INVALID_PARTNERLINKTYPE = "VAL_INVALID_PARTNERLINKTYPE";
    
    public static final String FIX_INVALID_PARTNERLINKTYPE = "FIX_INVALID_PARTNERLINKTYPE";
    
    public static final String VAL_INVALID_PARTNERLINKTYPE_ROLE = "VAL_INVALID_PARTNERLINKTYPE_ROLE";
    
    public static final String FIX_INVALID_PARTNERLINKTYPE_ROLE = "FIX_INVALID_PARTNERLINKTYPE_ROLE";
    
    /**
     * empty scope
     */
    public static final String VAL_EMPTY_SCOPE = "VAL_EMPTY_SCOPE";  //Not I18N
    
    /**
     * empty scope fix
     */
    public static final String FIX_EMPTY_SCOPE = "FIX_EMPTY_SCOPE";  //Not I18N
    
    /**
     * empty while
     */
    public static final String VAL_EMPTY_WHILE = "VAL_EMPTY_WHILE";  //Not I18N

    /**
     * empty while fix
     */
    public static final String FIX_EMPTY_WHILE = "FIX_EMPTY_WHILE";  //Not I18N

    /**
     * empty compensationHandler
     */
    public static final String VAL_EMPTY_COMPENSATION_HANDLER = "VAL_EMPTY_COMPENSATION_HANDLER";  //Not I18N

    /**
     * fix for an empty compensationHandler
     */
    public static final String FIX_EMPTY_COMPENSATION_HANDLER = "FIX_EMPTY_COMPENSATION_HANDLER";  //Not I18N
    
    /**
     * empty terminationHandler
     */
    public static final String VAL_EMPTY_TERMINATION_HANDLER = "VAL_EMPTY_TERMINATION_HANDLER";  //Not I18N

    /**
     * fix for an empty terminationHandler
     */
    public static final String FIX_EMPTY_TERMINATION_HANDLER = "FIX_EMPTY_TERMINATION_HANDLER";  //Not I18N

     /**
      * empty catch
      */
    public static final String VAL_EMPTY_CATCH = "VAL_EMPTY_CATCH";  //Not I18N

    /**
     * fix for an empty catch
     */
     public static final String FIX_EMPTY_CATCH = "FIX_EMPTY_CATCH";  //Not I18N

     /**
      * empty catch
      */
    public static final String VAL_EMPTY_CATCHALL = "VAL_EMPTY_CATCHALL";  //Not I18N

    /**
     * fix for an empty catch
     */
     public static final String FIX_EMPTY_CATCHALL = "FIX_EMPTY_CATCHALL";  //Not I18N
     
     /** No eInsight license for eVison */
     public static final String VAL_NO_EINSIGHT_LICENSE_W_EVISION = "VAL_NO_EINSIGHT_LICENSE_W_EVISION";  // Not I18N
     
     /** Only create instance from Page Link for eVision */
     public static final String FIX_NO_EINSIGHT_LICENSE_W_EVISION = "FIX_NO_EINSIGHT_LICENSE_W_EVISION";  // Not I18N

     private static final String VAL_NO_START_ACTIVITY = "VAL_NO_START_ACTIVITY";

    private static final String FIX_NO_START_ACTIVITY = "FIX_NO_START_ACTIVITY";
        
    
    public static final String VAL_MISSING_ATTRIB_VALUE = "VAL_MISSING_ATTRIB_VALUE";
	
    public static final String FIX_MISSING_ATTRIB_VALUE = "FIX_MISSING_ATTRIB_VALUE";
    
    public static final String VAL_INVALID_CONTAINER_PART = "VAL_INVALID_CONTAINER_PART";
	
    public static final String FIX_INVALID_CONTAINER_PART = "FIX_INVALID_CONTAINER_PART";
    
    public static final String VAL_UNKNOWN_FAULTNAME_IN_CATCH = "VAL_UNKNOWN_FAULTNAME_IN_CATCH"; 
    
    public static final String FIX_UNKNOWN_FAULTNAME_IN_CATCH = "FIX_UNKNOWN_FAULTNAME_IN_CATCH";
    
    /** bpel process namespace validation  */
    public static final String VAL_INVALID_QUALIFIED_NAMESPACE_OF_BPEL_PROCESS = "VAL_INVALID_QUALIFIED_NAMESPACE_OF_BPEL_PROCESS";
    public static final String FIX_INVALID_QUALIFIED_NAMESPACE_OF_BPEL_PROCESS = "FIX_INVALID_QUALIFIED_NAMESPACE_OF_BPEL_PROCESS";
    
    /** bpel process namespace validation  */
    public static final String VAL_INVALID_DEFAULT_NAMESPACE_OF_BPEL_DOCUMENT = "VAL_INVALID_DEFAULT_NAMESPACE_OF_BPEL_DOCUMENT";
    public static final String FIX_INVALID_DEFAULT_NAMESPACE_OF_BPEL_DOCUMENT = "FIX_INVALID_DEFAULT_NAMESPACE_OF_BPEL_DOCUMENT";
    
    /** bpel process namespace validation  */
    public static final String VAL_MISSING_BPEL_PROCESS_NAMESPACE = "VAL_MISSING_BPEL_PROCESS_NAMESPACE";
    public static final String FIX_MISSING_BPEL_PROCESS_NAMESPACE = "FIX_MISSING_BPEL_PROCESS_NAMESPACE";
    
    
    
    /** Creates a new instance of ValidateSupport.
     * @param   valConfig   Validate configuration.
     */
    public ValidateSupport(ValidateConfiguration valConfig) {
        super();
        mValConfig = valConfig;
    }
    
    /** Gets the validate configuration currently used.
     * @return  Validate configuration currently used.
     */
    public ValidateConfiguration getValidateConfiguration() {
        return mValConfig;
    }
    
    /** Sets the validate configuration currently used.
     * @param   valConfig   Validate configuration to use.
     */
    public void setValidateConfiguration(ValidateConfiguration valConfig) {
        mValConfig = valConfig;
    }
    
    /** Adds a to-do event listener.
     * @param   listener    A to-do event listener.
     */
    public void addToDoListener(ToDoListener listener) {
        toDoListeners.add(listener);
    }
    
    /** Removes a to-do event listener.
     * @param   listener    A to-do event listener.
     */
    public void removeToDoListener(ToDoListener listener) {
        toDoListeners.remove(listener);
    }
    
    /** Clears all to-do event listeners.
     */
    public void clearToDoListeners() {
        toDoListeners.clear();
    }
    
    /** Fires to-do events to listeners.
     * @param   toDoEvent   To-do event to fire.
     * @return  <code>true</code> if more events can be accepted by the listener;
     *          <code>false</code> otherwise.
     */
    public boolean fireToDo(ToDoEvent toDoEvent) {
        if (toDoListeners.size() > 0) {
            ToDoListener[] ls = (ToDoListener[]) toDoListeners.toArray(new ToDoListener[toDoListeners.size()]);
            for (int i = 0; i < ls.length; i++) {
                if (!(ls[i].toDoPerformed(toDoEvent))) {
                    return false;
                }
            }
        }
        return true;
    }

    /** Determines if an element is being started or ended.
     * @param   e   The element.
     * @return  <tt>true</tt> if element is being started.
     */
    public boolean isElementStart(XMLElement e) {
        boolean start = false;
        if (originals.isEmpty() || !originals.peek().equals(e)) {
            originals.push(e);
            start = true;
        } else {
            originals.pop();
            start = false;
        }
        return start;
    }
    
    /** Tests if an attribute value is absent.
     * @param   value   Value of attribute.
     * @return  <code>true</code> if value is absent.
     */
    public static boolean isAttributeAbsent(String value) {
        return ((null == value) || (value.trim().length() == 0));
    }
    
    public boolean assertAttributeValueNotEmpty(String value, String attrName, XMLElement source, int category) {
    	if(value == null) {
    		return true;
    	}
    	//just check if value is empty string
    	if(value.trim().equals("")) {
    		return fireToDo(new ToDoEvent(
                    source,
                    category,
                    Severity.ERROR,
                    mMsg.getString(VAL_MISSING_ATTRIB_VALUE, attrName),
                    mMsg.getString(FIX_MISSING_ATTRIB_VALUE, attrName)));
    	}
    	
    	return true;
    }
    /** Asserts that a required attribute is present.
     * @param   value       Value of attribute.
     * @param   name        Name of attribute.
     * @param   source      Source element.
     * @param   category    Category of failure.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    public boolean assertRequiredAttrib(String value, String name, XMLElement source, int category) {
        if (((source instanceof BPELElement)
                && !mValConfig.getBooleanProperty(ValidateConfiguration.BPEL_SYNTAX_ATTRIB_REQUIRED))) {
            return true;
        }
        
        if (isAttributeAbsent(value)) {
            return fireToDo(new ToDoEvent(
                source,
                category,
                Severity.ERROR,
                mMsg.getString(VAL_MISSING_ATTRIB, name),
                mMsg.getString(FIX_MISSING_ATTRIB, name)));
        }
        
        return true;
    }


    /** Asserts that a required attribute is present.
     * @param   value       Value of attribute.
     * @param   name        Name of attribute.
     * @param   source      Source element.
     * @param   category    Category of failure.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    public boolean assertRequiredQNameAttrib(QName value, String name, XMLElement source, int category) {
        if (((source instanceof BPELElement)
                && !mValConfig.getBooleanProperty(ValidateConfiguration.BPEL_SYNTAX_ATTRIB_REQUIRED))) {
            return true;
        }
        
        if (value == null || isAttributeAbsent(value.toString())) {
            return fireToDo(new ToDoEvent(
                source,
                category,
                Severity.ERROR,
                mMsg.getString(VAL_MISSING_ATTRIB, name),
                mMsg.getString(FIX_MISSING_ATTRIB, name)));
        }
        
        return true;
    }
    
    /** Asserts that a required attribute is present.
     * @param   value       Value of attribute.
     * @param   name        Name of attribute.
     * @param   source      Source element.
     * @param   category    Category of failure.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    public boolean assertRequiredQNameAttribArray(QName[] qNames, String name, XMLElement source, int category) {
        if (((source instanceof BPELElement)
                && !mValConfig.getBooleanProperty(ValidateConfiguration.BPEL_SYNTAX_ATTRIB_REQUIRED))) {
            return true;
        }
        
        if (qNames == null ) {
            return fireToDo(new ToDoEvent(
                source,
                category,
                Severity.ERROR,
                mMsg.getString(VAL_MISSING_ATTRIB, name),
                mMsg.getString(FIX_MISSING_ATTRIB, name)));
        }
        
        return true;
    }
    /**
     * @param pMessageType string
     * @param name name
     * @param source source
     * @param category the category
     * @return yes/no
     */
    public boolean isValidMessageType(String pMessageType, String name, 
                                      Variable source, int category) {
        //check if the message type is a valid QName
        BPELDocument lDocument = (BPELDocument) source.getOwnerDocument();
        if (pMessageType == null || pMessageType.trim().length() == 0) {
            return false;
        }
        String lPrefix = NamespaceUtility.getPrefix(pMessageType);
        
        String lNameSpace = lDocument.getNamespace(lPrefix);
        if (Utility.isEmpty(lNameSpace)) {
            return false;
        }

        if (null == lDocument) {
            return true;    // not enough info to determine
        }
        
        Message wsdlMessage = source.getWSDLMessageType();
        if (wsdlMessage != null) {
            return true;
        }

        return fireToDo(new ToDoEvent(source, category, Severity.ERROR, 
                        this.mMsg.getString(VAL_MESSAGETYPE_IN_CONTAINER, pMessageType,
                        ((Variable) source).getName()),
                        this.mMsg.getString(FIX_MESSAGETYPE_IN_CONTAINER)));
    }
    /**
     * Two validation:
     * (a) check if faultName is a valid faultName defined in any parent scope's throw activity.
     * This throw activity can be nested inside another scope at any level.
     * (b) check in faultName is a valid fault wsdl message in a wsdl operation which
     * is invoked by an activity of the parent scope or any nested activity therein.
     * @param value the value
     * @param name the name
     * @param source the source
     * @param category the category
     * @return yes/no
     */
    public boolean assertIsValidFaultName(QName faultName, String name, Catch source, int category) {
        if (source == null) {
            return false;
        }
        
        //okay, a fault name exists. check with wsdl if it is known.
        if (this.isAValidFaultName(source, faultName)) {
            return true;
        }
        return fireToDo(new ToDoEvent(source, category, Severity.ERROR, 
                        this.mMsg.getString(VAL_UNKNOWN_FAULTNAME_IN_CATCH, faultName, source),
                        this.mMsg.getString(FIX_UNKNOWN_FAULTNAME_IN_CATCH)));

    }

    /**
     * @param s the sequence
     * @param name the name
     * @param category error category
     * @return yes/no
     */
    public boolean isAValidSequence(Sequence s, String name, int category) {
        java.util.Map lMap = s.getOtherAttributes();
        if (lMap != null && lMap.containsKey("sbyninc:type")) {
//        if (s.getAttribute("sbyninc:type") != null) {
            if (s.getName() == null) {
                s.setName(this.mMsg.getString(SEQUENCE_TITLE_NAME)); // "Sequence");
            }
            return fireToDo(new ToDoEvent(s, category, Severity.ERROR, 
                        this.mMsg.getString(VAL_MISSING_CONNECTIONS),
                        //"Some connections are missing",
                        //"Please ensure that all nodes are reachable"));
                        this.mMsg.getString(FIX_MISSING_CONNECTIONS)));
        }
        return true;
    }
    
    private String addQuotes(String in) {
        return "\"" + in + "\"";
    }

	/**
	 * 	Two validation:
	     * (a) check if faultName is a valid faultName defined in any parent scope's throw activity.
	     * This throw activity can be nested inside another scope at any level.
	     * (b) check in faultName is a valid fault wsdl message in a wsdl operation which
	     * is invoked by an activity of the parent scope or any nested activity therein.
	 * @param source
	 * @param name
	 * @return
	 */
    private boolean isAValidFaultName(Catch catchElement, QName faultQName) {
    	
    	if(faultQName == null) {
    		return false;
    	}
    	
    	//first check if it is a standard fault name;
    	if(StandardFaults.isStandardFault(faultQName)) {
    		return true;
    	}
    	
    	QName normalizedFaultQName = faultQName;
    	
        //get all the invokes.
        XMLElement elem = null;
        
        List processOrScopeChildren = new ArrayList();
        List invokeAndThrowActivities = new ArrayList();
        
        //Invoke faultHandler shortcut, basically catch can be directly embed without
        //faultHandlers tag in invoke
        if(catchElement.getParent() instanceof Invoke) {
        	elem = (XMLElement) catchElement.getParent();
        	invokeAndThrowActivities.add(elem);
            
        } else {
        	elem = (XMLElement) BPELHelper.getFirstAncestorBPELProcessOrScope(catchElement);
        	
        	processOrScopeChildren.addAll(elem.getChildren());
        	
        	 //now remove the FaultHandlers element from this list of children
        	processOrScopeChildren.remove(catchElement.getParent());
            
        	 //go through the list of children that the parent has
            List tobeRemoved = new ArrayList();
            /**
             * Note that the following logic will fail should multiple
             * CompensationHandlers be possible. The logic below will end up removing
             * innocent objects.
             */
            for (Iterator iter = processOrScopeChildren.iterator(); iter.hasNext();) {
                Object obj = iter.next();
                if (obj instanceof CompensationHandler || obj instanceof TerminationHandler) {
                    tobeRemoved.add(new Integer(processOrScopeChildren.indexOf(obj)));
                }
            }
            for (Iterator iter = tobeRemoved.iterator(); iter.hasNext();) {
                Integer integer = (Integer) iter.next();
                if (integer.intValue() >= 0) {
                	processOrScopeChildren.remove(integer.intValue());
                } else {
                    //should never happen.
                }
            }
            //this.getChildren(list, elem.getChildren());
            this.findAllNestedInvokeAndThrow(invokeAndThrowActivities, processOrScopeChildren);
        }
        
       
        //this.getChildren(list, elem.getChildren());
        Set allFaultQNameStrings = this.getPotentialFaultNames(invokeAndThrowActivities);
        return allFaultQNameStrings.contains(normalizedFaultQName.toString());
        /*
        //now go through all these invokes and find one which has the same fault name
        for (Iterator iter = list.iterator(); iter.hasNext();) {
            Invoke invoke = (Invoke) iter.next();
            String lPortType = invoke.getPortType();
            if (faultNameExistsInWSDL(name, source, lPortType)) {
                return true;
            }
        }
        return false;
        */
    }

	/**
	 * 	Two validation:
	     * (a) check if faultName is a valid faultName defined in any parent scope's throw activity.
	     * This throw activity can be nested inside another scope at any level.
	     * (b) check in faultName is a valid fault wsdl message in a wsdl operation which
	     * is invoked by an activity of the parent scope or any nested activity therein.
	 * @param pInputList
	 * @param pToFill
	 * @param source
	 * @param name
	 */
    private Set getPotentialFaultNames(List invokeAndThrowActivities) {
    	Set allFaultQNameStrings = new HashSet();
    	
        for (Iterator iter = invokeAndThrowActivities.iterator(); iter.hasNext();) {
            Object obj = iter.next();
            if (obj instanceof Invoke) {
                Invoke invoke = (Invoke) obj;
                if (invoke != null) {
                    PortType portType = invoke.getWSDLPortType();
                    if (portType != null) {
                    	List wsdlFaultQNameStrings = getWSDLFaultQNameStrings(portType);
                    	allFaultQNameStrings.addAll(wsdlFaultQNameStrings);
                    }
                    //get the Catches that can be thrown
                    /*
                    Collection catches = invoke.getCatches();
                    for (Iterator iter2 = catches.iterator(); iter2.hasNext();) {
                        Object obj1 = iter2.next();
                        if (obj1 instanceof Catch) {
                            String cn = ((Catch) obj1).getFaultName();
                            if (cn != null || cn.trim().length() != 0) {
                                pToFill.add(cn);
                            }
                        }
                    }
                    */
                }
            } else if (obj instanceof Throw) {
                
                Throw t = (Throw) obj;
                if (t != null) {
                    QName fQname = t.getFaultName();
                    if (fQname != null) {
                    	String str = NamespaceUtility.getQNameAsString(fQname).trim();
                    	if (str.length() != 0) {
                    		allFaultQNameStrings.add(str);
                    	}
                    }
                }
                
            }
        }
        
        return allFaultQNameStrings;
    }
   

    private void findAllNestedInvokeAndThrow(List pList, List pSource) {
        if (pSource == null) {
            return;
        }
        for (Iterator iter = pSource.iterator(); iter.hasNext();) {
            Object obj = iter.next();
            if (!(obj instanceof XMLElement)) {
                continue;
            }
            XMLElement elem = (XMLElement) obj;
            if (elem instanceof Invoke || elem instanceof Throw) {
                pList.add(elem);
            } else if (elem instanceof Sequence 
                    || elem instanceof Scope 
                    || elem instanceof While
                    || elem instanceof Pick
                    || elem instanceof Switch
                    || elem instanceof Flow 
                    || elem instanceof Case
                    || elem instanceof Otherwise
                    || elem instanceof If
                    || elem instanceof ElseIf
                    || elem instanceof Else
                    || elem instanceof OnMessage
                    || elem instanceof OnAlarm
                    || elem instanceof CompensationHandler
                    || elem instanceof TerminationHandler                    
                    || elem instanceof FaultHandlers
                    || elem instanceof Catch) {
                this.findAllNestedInvokeAndThrow(pList, elem.getChildren());
            }
        }
    }


    /**
     * @param pHandler the compensation handler
     * @param category the error type
     * @return yes to continue visiting
     */
    public boolean isValidCompensationHandler(CompensationHandler pHandler, int category) {
        if (pHandler == null) {
             return fireToDo(new ToDoEvent(pHandler, category, Severity.WARNING, 
                        this.mMsg.getString(VAL_EMPTY_COMPENSATION_HANDLER),
                        this.mMsg.getString(FIX_EMPTY_COMPENSATION_HANDLER)));
        }
        List list = pHandler.getChildren();
        for (Iterator iter = list.iterator(); iter.hasNext();) {
            Object obj = iter.next();
            if (obj instanceof Sequence) {
                Sequence s = (Sequence) obj;
                if (this.isAnEmptySequence(s)) {
                 return fireToDo(new ToDoEvent(pHandler, category, Severity.WARNING, 
                        this.mMsg.getString(VAL_EMPTY_COMPENSATION_HANDLER),
                        this.mMsg.getString(FIX_EMPTY_COMPENSATION_HANDLER)));
                }
            }
        }
        return true;
    }
    
    /**
     * @param pHandler the termination handler
     * @param category the error type
     * @return yes to continue visiting
     */
    public boolean isValidTerminationHandler(TerminationHandler pHandler, int category) {
        if (pHandler == null) {
             return fireToDo(new ToDoEvent(pHandler, category, Severity.WARNING, 
                        this.mMsg.getString(VAL_EMPTY_TERMINATION_HANDLER),
                        this.mMsg.getString(FIX_EMPTY_TERMINATION_HANDLER)));
        }
        List list = pHandler.getChildren();
        for (Iterator iter = list.iterator(); iter.hasNext();) {
            Object obj = iter.next();
            if (obj instanceof Sequence) {
                Sequence s = (Sequence) obj;
                if (this.isAnEmptySequence(s)) {
                 return fireToDo(new ToDoEvent(pHandler, category, Severity.WARNING, 
                        this.mMsg.getString(VAL_EMPTY_TERMINATION_HANDLER),
                        this.mMsg.getString(FIX_EMPTY_TERMINATION_HANDLER)));
                }
            }
        }
        return true;
    }

    /**
     * @param pCatch the catch
     * @param category the category
     * @return yes to continue visiting
     */
    public boolean isValidCatch(Catch pCatch, int category) {
        if (pCatch == null) {
             return fireToDo(new ToDoEvent(pCatch, category, Severity.WARNING, 
                        this.mMsg.getString(VAL_EMPTY_CATCH),
                        this.mMsg.getString(FIX_EMPTY_CATCH)));
        }
        List list = pCatch.getChildren();
        for (Iterator iter = list.iterator(); iter.hasNext();) {
            Object obj = iter.next();
            if (obj instanceof Sequence) {
                Sequence s = (Sequence) obj;
                if (this.isAnEmptySequence(s)) {
                    return fireToDo(new ToDoEvent(pCatch, category, Severity.WARNING, 
                        this.mMsg.getString(VAL_EMPTY_CATCH),
                        this.mMsg.getString(FIX_EMPTY_CATCH)));       
                }
            }
        }
        return true;
    }

    /**
     * @param pCatch catchall
     * @param category the error type
     * @return yes to continue validation
     */
    public boolean isValidCatchAll(CatchAll pCatch, int category) {
        if (pCatch == null) {
             return fireToDo(new ToDoEvent(pCatch, category, Severity.WARNING, 
                        this.mMsg.getString(VAL_EMPTY_CATCHALL),
                        this.mMsg.getString(FIX_EMPTY_CATCHALL)));
        }
        List list = pCatch.getChildren();
        for (Iterator iter = list.iterator(); iter.hasNext();) {
            Object obj = iter.next();
            if (obj instanceof Sequence) {
                Sequence s = (Sequence) obj;
                if (this.isAnEmptySequence(s)) {
                    return fireToDo(new ToDoEvent(pCatch, category, Severity.WARNING, 
                        this.mMsg.getString(VAL_EMPTY_CATCHALL),
                        this.mMsg.getString(FIX_EMPTY_CATCHALL)));       
                }
            }
        }
        return true;
    }
    

    private boolean isAnEmptySequence(Sequence s) {
        if (s == null || s.getChildren() == null || s.getChildren().size() == 0) {
            return true;
        }
        return false;
    }

    /**
     * @param pWhile the while
     * @param category typr
     * @return yes to carry on
     */
    public boolean isValidWhile(While pWhile, int category) {
        if (pWhile.getChildren() == null 
                || pWhile.getChildren().size() == 0) {
             return fireToDo(new ToDoEvent(pWhile, category, Severity.WARNING, 
                        this.mMsg.getString(VAL_EMPTY_WHILE),
                        //"Some connections are missing",
                        //"Please ensure that all nodes are reachable"));
                        this.mMsg.getString(FIX_EMPTY_WHILE)));
        } 
        List pList = pWhile.getChildren();
        for (Iterator iter = pList.iterator(); iter.hasNext();) {
            Object obj = iter.next();
            if (obj instanceof Sequence) {
                Sequence s = (Sequence) obj;
                if (this.isAnEmptySequence(s)) {
                //s == null || s.getChildren() == null || s.getChildren().size() == 0) {
                    return fireToDo(new ToDoEvent(pWhile, category, Severity.WARNING, 
                        this.mMsg.getString(VAL_EMPTY_WHILE),
                        this.mMsg.getString(FIX_EMPTY_WHILE)));
                }
            }
        }
        return true;
    }

    /**
     * @param pScope the scope to be tested
     * @param category the errot type
     * @return yes to carry on
     */
    public boolean isValidScope(Scope pScope, int category) {
        if (pScope.getChildren() == null 
        || pScope.getChildren().size() == 0) {
             return fireToDo(new ToDoEvent(pScope, category, Severity.WARNING,
                        this.mMsg.getString(VAL_EMPTY_SCOPE),
                        //"Some connections are missing",
                        //"Please ensure that all nodes are reachable"));
                        this.mMsg.getString(FIX_EMPTY_SCOPE)));
        }
        List pList = pScope.getChildren();
        for (Iterator iter = pList.iterator(); iter.hasNext();) {
            Object obj = iter.next();
            if (obj instanceof Sequence) {
                Sequence s = (Sequence) obj;
                if (this.isAnEmptySequence(s)) {
                //s == null || s.getChildren() == null || s.getChildren().size() == 0) {
                    return fireToDo(new ToDoEvent(pScope, category, Severity.WARNING, 
                        this.mMsg.getString(VAL_EMPTY_SCOPE),
                        this.mMsg.getString(FIX_EMPTY_SCOPE)));
                }
            }
        }
        return true;
    }

    

    private List getWSDLFaultQNameStrings(PortType portType) {
    	List wsdlFaultsQNameStrings = new ArrayList();
    	
        if (portType != null) {
            Collection l = portType.getOperations();
            for (Iterator i2 = l.iterator(); i2.hasNext();) {
                Operation o = (Operation) i2.next();
                Collection faultList = o.getFaults().values();
                for (Iterator i3 = faultList.iterator(); i3.hasNext();) {
                    Fault f = (Fault) i3.next();
                    //This seems to be wrong in old implementation. In WSDL4J,
                    //Fault can only return a local name.
                    if(f.getName() != null) {
                        QName qName =
                            new QName(portType.getQName().getNamespaceURI(),
                                    f.getName());
                    	wsdlFaultsQNameStrings.add(qName.toString());
                    }
                }
            }
        }
        
        return wsdlFaultsQNameStrings;
    }
    
    
    private boolean faultNameExistsInWSDL(String faultQNameString, 
    									  PortType portType) {
        if (faultQNameString == null || faultQNameString.trim().length() == 0 || portType == null) {
            return false;
        }
        String lFaultName = NamespaceUtility.getLocalName(faultQNameString);
        
        if (portType != null) {
            Collection l = portType.getOperations();
            for (Iterator i2 = l.iterator(); i2.hasNext();) {
                Operation o = (Operation) i2.next();
                Collection faultList = o.getFaults().values();
                for (Iterator i3 = faultList.iterator(); i3.hasNext();) {
                    Fault f = (Fault) i3.next();
                    if (lFaultName.equals(f.getName())) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * @param value the value
     * @param name the name
     * @param source the xmlelement
     * @param category the category
     * @return yes/no
     */
    public boolean assertSemanticallyCorrectAttrib(String portTypeQName,
    											   String operationName,	
    											   String name, 
    											   XMLElement source,
    											   PortType portType,
    											   Operation operation,
    											   int category) {
        if (portTypeQName == null) {
            return false;
        }
       
        if (portType != null) {
                if (operation != null) {
                        return true; //as this is being caught elsewhere
                } else {
                    return fireToDo(new ToDoEvent(source, category, Severity.ERROR, 
                            this.mMsg.getString(VAL_UNKNOWN_OPERATION, 
                            					 operationName),
                            this.mMsg.getString(FIX_UNKNOWN_OPERATION)));
                }
         }
        
        //if this point is reached it implies that the portType does not exist.
        return fireToDo(new ToDoEvent(source, category, Severity.ERROR, 
                            this.mMsg.getString(VAL_UNKNOWN_PORT_TYPE, portTypeQName),
                            this.mMsg.getString(FIX_UNKNOWN_PORT_TYPE)));
    }     
    
    /** Asserts that an attribute is a NCName.
     * @param   value       Value of attribute.
     * @param   name        Name of attribute.
     * @param   source      Source element.
     * @param   category    Category of failure.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    public boolean assertNCNameAttrib(String value, String name, XMLElement source, int category) {
        if (((source instanceof BPELElement)
                && !mValConfig.getBooleanProperty(ValidateConfiguration.BPEL_SYNTAX_ATTRIB_NCNAME))) {
            return true;
        }
        
        if (!isAttributeAbsent(value) && !ValidationUtils.isNCName(value)) {
            return fireToDo(new ToDoEvent(
                source,
                category,
                Severity.ERROR,
                mMsg.getString(VAL_NOT_NCNAME_ATTRIB, name),
                mMsg.getString(FIX_NOT_NCNAME_ATTRIB, name)
            ));
        }
        return true;
    }

    private void getBPElementList(XMLElement BP, List pInput) {
        //get the sequence
        List children = BP.getChildren(); 
        for (Iterator iter = children.iterator(); iter.hasNext();) {
            Object obj = iter.next();
            if (obj instanceof Sequence) {
                pInput.addAll(((Sequence) obj).getChildren());
            }
        }
    }

    private void getElementList(XMLElement pScope, List pInput) {
        List children = pScope.getChildren();
        for (Iterator iter = children.iterator(); iter.hasNext();) {
            Object obj = iter.next();
            pInput.addAll(pScope.getChildren());
        }
    }
    /**
     * @param value the value
     * @param name name
     * @param source source
     * @param category category
     * @return yes/no
     */
    public boolean assertScopeNameIsValid(String value, String name, XMLElement source, int category) {
        Compensate lCompensate = (Compensate) source;
   
        //does this compensate have its scope set?
        String lName = lCompensate.getScope();
        if (lName == null || lName.trim().length() == 0) {
            return true; //let this one go
        }
        lName = lName.trim();
        XMLElement lScope =   (XMLElement) lCompensate.getParent().getParent().getParent(); 
        //this could be a compensation or fault handler
        //now get the children of this 
        //Scope lScope = (Scope) lHandler.getParent();
        //get a list of all the activities in scope.
        List list = lScope.getChildren();
        List lScopeNames = new ArrayList();
        List fillList = new ArrayList();
        /*
        if (lScope instanceof Scope) {
            this.getElementList(lScope, fillList);
        } else {*/
        
            this.getBPElementList(lScope, fillList);
        //}
        for (Iterator iter = fillList.iterator(); iter.hasNext();) {
            Object obj = iter.next();
            if (obj instanceof XMLElement) {
                XMLElement lElem = (XMLElement) obj;
                if (lElem instanceof Activity 
                    && (lElem instanceof Scope 
                    || lElem instanceof Invoke)) {
                        String llname = ((Activity) lElem).getName();
                        lScopeNames.add(llname);
                        if (llname.equals(lName)) {
                            return true;
                        }
                    }
            }
        }
        /*
        for (Iterator iter = list.iterator(); iter.hasNext();) {
            Object obj = iter.next();
            if (!(obj instanceof Activity)) {
                continue;
            }
            //get the sequence if any
            if (obj instanceof Sequence) {
                Sequence seq = (Sequence) obj;
                fillList.addAll(seq.getChildren());
            }
        }

        for (Iterator iter = fillList.
            Activity lElement = (Activity) obj;
            if ( lElement instanceof Scope 
                || lElement instanceof Invoke) {
                lScopeNames.add(lElement.getName());
                if (lElement.getName().equals(lName)) {
                    return true;
                }
            }
        }
        */
        //concatenate
        StringBuffer lBuffer = new StringBuffer();
        int i = 0;
        for (Iterator iter = lScopeNames.iterator(); iter.hasNext();) {
            lBuffer.append((String) iter.next());
            if (i != lScopeNames.size() - 1) {
                lBuffer.append(", ");
            }
            i++;
        }
        String lDesc = "Trying to compensate an invalid scope: " + addQuotes(lName);
        String remedy = " Use a scope or activity that is within the scope ";
        remedy += "that the current exception/compensation handler is associated with";
        /*
        return fireToDo(new ToDoEvent(source, category, Severity.ERROR, "Compensate declares " + lName 
                    + " as its scope but the immediate scope does not have any activity with this name",
                    "Use a scope name that is declared in the immediate scope"));
        */
        /*
        return fireToDo(new ToDoEvent(source, category, Severity.ERROR, lDesc,
                        remedy)); 
        */
        return fireToDo(new ToDoEvent(source, category, Severity.ERROR, 
                         mMsg.getString(VAL_COMPENSATE_WITH_AN_INVALID_SCOPE, name),
                            mMsg.getString(FIX_COMPENSATE_WITH_AN_INVALID_SCOPE)
                            ));
    }
    
    /** Asserts that an attribute is a QName list.
     * @param   value       Value of attribute.
     * @param   name        Name of attribute.
     * @param   source      Source element.
     * @param   category    Category of failure.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    public boolean assertQNameArrayAttrib(QName[] qNames, String name, XMLElement source, int category) {
        if (((source instanceof BPELElement)
                && !mValConfig.getBooleanProperty(ValidateConfiguration.BPEL_SYNTAX_ATTRIB_QNAME))) {
            return true;
        }
        
        if (qNames != null) {
        	for(int i =0; i < qNames.length; i++) {
        		if (!assertQNameAttrib(qNames[i], name, source, category)) {
                    return false;
                }
        	}
        	
        }
        return true;
    }
    
    /** Asserts that a required attribute is a QName.
     * @param   value       Value of attribute.
     * @param   name        Name of attribute.
     * @param   source      Source element.
     * @param   category    Category of failure.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    public boolean assertQNameAttrib(String value, String name, XMLElement source, int category) {
        if (((source instanceof BPELElement)
                && !mValConfig.getBooleanProperty(ValidateConfiguration.BPEL_SYNTAX_ATTRIB_QNAME))) {
            return true;
        }
        
        if (!isAttributeAbsent(value)) {
            // First, check if QName compliant
            if (!ValidationUtils.isQName(value)) {
                if (!fireToDo(new ToDoEvent(
                            source,
                            category,
                            Severity.ERROR,
                            mMsg.getString(VAL_NOT_QNAME_ATTRIB, name),
                            mMsg.getString(FIX_NOT_QNAME_ATTRIB, name)
                        ))) {
                    return false;
                }
            }
            String prefix = NamespaceUtility.getPrefix(value);
            if (prefix != null) {
                // Second, check if prefix corresponds to a namespace
                String ns = ((XMLElement) source).getNamespace(prefix);
                if (null == ns) {
                    if (!fireToDo(new ToDoEvent(
                                source,
                                category,
                                Severity.ERROR,
                                mMsg.getString(VAL_PREFIX_NOT_DEFINED, prefix),
                                mMsg.getString(FIX_PREFIX_NOT_DEFINED, prefix)
                            ))) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    /** Asserts that a required attribute is a QName.
     * @param   value       Value of attribute.
     * @param   name        Name of attribute.
     * @param   source      Source element.
     * @param   category    Category of failure.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    public boolean assertQNameAttrib(QName value, String name, XMLElement source, int category) {
        if(value != null) {
        	return assertQNameAttrib(value.toString(), name, source, category);
        }
        return true;
    }
    /** Asserts that a required attribute is a QName. Apart from the QName test, also
     * verify that the prefix is not null
     * @param   value       Value of attribute.
     * @param   name        Name of attribute.
     * @param   source      Source element.
     * @param   category    Category of failure.
     * @param   verifyNullPrefix If set verify that the prefix in the QName is not null
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    public boolean assertQNameAttrib(String value, String name, XMLElement source, 
                                     int category, boolean verifyNullPrefix) {
        boolean valid = assertQNameAttrib(value, name, source, category);
        if (valid && verifyNullPrefix) {
            String prefix = NamespaceUtility.getPrefix(value);
            // Second, check if prefix is not null
            if (prefix == null || "".equals(prefix.trim())) {
                if (!fireToDo(new ToDoEvent
                              (source,
                               category,
                               Severity.ERROR,
                               mMsg.getString(VAL_PREFIX_NULL_QNAME_ATTRIB, name),
                               mMsg.getString(FIX_PREFIX_NULL_QNAME_ATTRIB, name)
                               ))) {
                    return false;
                }
            }
        }

        return true;
    }
    
    /** Asserts that an attribute has a boolean value.
     * @param   value       Value of attribute.
     * @param   name        Name of attribute.
     * @param   source      Source element.
     * @param   category    Category of failure.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    public boolean assertBooleanAttrib(String value, String name, XMLElement source, int category) {
        if (((source instanceof BPELElement)
                && !mValConfig.getBooleanProperty(ValidateConfiguration.BPEL_SYNTAX_ATTRIB_BOOLEAN))) {
            return true;
        }
        
        if (!isAttributeAbsent(value) && !(value.equals("yes") || value.equals("no"))) {
            return fireToDo(new ToDoEvent(
                source,
                category,
                Severity.ERROR,
                mMsg.getString(VAL_NOT_BOOLEAN_ATTRIB, name),
                mMsg.getString(FIX_NOT_BOOLEAN_ATTRIB, name)
            ));
        }
        return true;
    }
    
    /** Asserts that a minimum number of sub-elements is required.
     * @param   num         Number of existing sub-elements.
     * @param   min         Minimum number of sub-elements required.
     * @param   name        Name of sub-element.
     * @param   source      Source element.
     * @param   category    Category of failure.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    public boolean assertMinimumElem(int num, int min, String name, XMLElement source, int category) {
        if (((source instanceof BPELElement)
                && !mValConfig.getBooleanProperty(ValidateConfiguration.BPEL_SYNTAX_ELEM_MIN))) {
            return true;
        }
        
        if (num < min) {
            return fireToDo(new ToDoEvent(
                source,
                category,
                Severity.ERROR,
                mMsg.getString(VAL_MIN_ELEM_NOT_MET, new Integer(min), name, new Integer(num)),
                mMsg.getString(FIX_MIN_ELEM_NOT_MET, new Integer(min - num), name)
            ));
        }
        return true;
    }

    /**
     * check if the assign has the minimum number of elements
     * @param a the assign to be tested
     * @param category the category of error
     * @return yes if the assign is valid
     */
    public boolean hasMinimumElements(Assign a, int category) {
        //does assign have at least one copy?
        Collection list = a.getCopies();
        //list can be null (it happens) 
        if (list == null || list.size() == 0) {
            //check if there is at least one for each
            Collection feList = a.getForEachs();
            if (feList == null || feList.size() == 0) {
                Collection chooseList = a.getChooses();
                if (chooseList == null || chooseList.size() == 0) {
                    String description = "An Assign must have at least one copy or one foreach or one choose";
                    String remedy = "Add a copy or for each or choose";
                    return fireToDo(new ToDoEvent(a, category, Severity.ERROR, 
                            this.mMsg.getString(VAL_ASSIGN_MUST_HAVE_MINIMUM),
                            this.mMsg.getString(FIX_ASSIGN_MUST_HAVE_MINIMUM)));
                }
            }
        }
        return true;
    }

    private boolean isAValidForEach(ForEach fe) {
        //check if this for each has copies
        if (fe == null || fe.getCopySize() == 0) {
            //if this is nested, get all the children
            if (fe.getForEachSize() == 0 && fe.getChooseSize() == 0) {
                return false;
            }   
        }

        if (fe.getForEachSize() > 0) {
            Collection c = fe.getForEachs();
            for (Iterator iter = c.iterator(); iter.hasNext();) {
                ForEach forEach = (ForEach) iter.next();
                //is this a valid 
                if (!this.isAValidForEach(forEach)) {
                    return false;
                }
            }
        }
        if (fe.getChooseSize() > 0) {
            Collection c = fe.getChooses();
            for (Iterator iter = c.iterator(); iter.hasNext();) {
                Choose choose = (Choose) iter.next();
                //is this a valid 
                if (!this.isAValidChoose(choose)) {
                    return false;
                }
            }
        }
        return true;
    }

    private boolean isAValidChoose(Choose c) {
        return true;
        /*
        Collection co = c.getChildren();
        if (co == null || co.getSize() == 0) {
            return false;
        }
        co = c.getWhens();
        for (Iterator iter = c.iterator(); iter.hasNext();) {
            When when = (When) iter.next();
            //is this a valid 
            if (!isAValidWhen(when)) {
                return false;
            }
        }
        Default d = c.getDefault();
        if (d != null) {
            if (!isAValidDefault(d)) {
                return false;
            }
        }
        return true;
        */
    }

    /**
     * @param fe the foreach object
     * @param category the category
     * @return yes if valid for each
     */
    public boolean verifyForEach(ForEach fe, int category) {
        //if (fe != null && fe.getForEachSize() != 0 ) {
        if (!this.isAValidForEach(fe)) {
            //check if the number of copies in
            String description = "For each must have at least one copy or one foreach or one choose";
            String remedy = "Define at least one copy or foreach or choose in For each";
            return fireToDo(new ToDoEvent(fe, category, Severity.ERROR, 
                                    this.mMsg.getString(VAL_FOR_EACH_MUST_HAVE_MIN_ONE_COPY),
                                    this.mMsg.getString(FIX_FOR_EACH_MUST_HAVE_MIN_ONE_COPY)));
        }
        return true;
    }

/*
     public boolean assertSematicallyCorrectCorrelations(int num, int min, String name, 
     Correlations source, int category) {
        Collection list = source.getCorrelations();
        //get the BPELDocument
        BPELDocument lDocument = (BPELDocument) source.getOwnerDocument();
        BPELProcess lProcess = lDocument.getDocumentProcess();
        //get all the correlation sets
        CorrelationSets lSets = lProcess.getCorrelationSets();
        //list of all the [correlation]s known to the document.
        Collection lRefList = lSets.getCorrelationSets();
        //prepare a map based on the name. We will use this as a look up.
        Map lMap = new HashMap();
        for (Iterator iter = lRefList.iterator(); iter.hasNext();) {
            CorrelationSet lC = (CorrelationSet) iter.next();
            lMap.put(lC.getName(), lC);
        }
        for (Iterator iter = list.iterator(); iter.hasNext();) {
            Correlation cor = (Correlation) iter.next();
            String lSet = cor.getSet(); 
            //use this name to do a look up in the previously prepared map.
            Correlation
            
        }

        return true;
    }
*/
    /**
     * 
     * @param pName the input name
     * @param name name
     * @param c the correlation set
     * @param category the kind of error.
     * @return yes/no
     */

    public boolean assertSemanticallyCorrectCorrelationSet(String pName, String name,
                                                           CorrelationSet c, int category) {
        QName[] properties = c.getProperties();
        if(properties != null) {
	        for (int i = 0; i < properties.length; i++) {
	        		QName propertyQName = properties[i];
	                BPELDocument document = (BPELDocument) c.getOwnerDocument();
	                BPELProcess process = document.getDocumentProcess();
	                MessageProperty property = process.getBPELProperty(propertyQName);
	            boolean value = this.assertSemanticallyCorrectCorrelationSet(propertyQName, property, pName, name, c, category);
	             if (!value) {
	                 return value;
	             }
	        }     
        }
        return true;
    }

    /**
     * @param pCorrelationProperty the parsed correlationproperty
     * @param pName the input name
     * @param name name
     * @param c the correlation set
     * @param category the kind of error.
     * @return yes/no
     */
    public boolean assertSemanticallyCorrectCorrelationSet(QName pCorrelationProperty,
    													   MessageProperty property, 	
    													   String pName, 
    													   String name , 
    													   CorrelationSet c, 
    													   int category) {
            
            if (property != null) {
                return true;
            }
            String description = "The correlation set " + pName + " contains property " + pCorrelationProperty;
            description += " that cannot be resolved from WSDL. Please make sure that all WSDL namespaces";
            description += " are resolved by uploading";

            String remedy = "Please check property definition from WSDL";
            return fireToDo(new ToDoEvent(c, category, Severity.ERROR, 
                    this.mMsg.getString(VAL_SEMANTICALLY_CORRECT_CORRELATION_SET, pName, pCorrelationProperty), 
                    this.mMsg.getString(FIX_SEMANTICALLY_CORRECT_CORRELATION_SET)));
    }
    
    /** Asserts that an attribute has an enumerated value.
     * @param   value       Value of attribute.
     * @param   enums       Enumerated values as an array.
     * @param   name        Name of attribute.
     * @param   source      Source element.
     * @param   category    Category of failure.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    public boolean assertEnumeratedAttrib(String value, String[] enums, String name, XMLElement source,
                                          int category) {
       if (((source instanceof BPELElement)
                && !mValConfig.getBooleanProperty(ValidateConfiguration.BPEL_SYNTAX_ATTRIB_ENUMERATED))) {
           return true;
       }
       
       if (!isAttributeAbsent(value) && (enums != null) && (enums.length > 0)) {
           boolean found = false;
           for (int i = 0; i < enums.length; i++) {
               if (Utility.areEqualXMLValues(value, enums[i])) {
                   found = true;
                   break;
               }
           }
           if (!found) {
               StringBuffer enumsb = new StringBuffer();
               for (int i = 0; i < enums.length; i++) {
                   if (i > 0) {
                       enumsb.append(", ");
                   }
                   enumsb.append(enums[i]);
               }
               return fireToDo(new ToDoEvent(
                    source, Category.BPEL_SYNTAX, Severity.ERROR,
                    mMsg.getString(VAL_NOT_ENUM_ATTRIB, name, enumsb.toString()),
                    mMsg.getString(FIX_NOT_ENUM_ATTRIB, enumsb.toString(), name)
               ));
           }
       }
       return true;
    }
    
    /** Asserts that a required sub-element is present.
     * @param   subElem     Sub-element.
     * @param   name        Name of sub-element.
     * @param   source      Source element.
     * @param   category    Category of failure.
     * @return  <code>true</code> if more validations can be made; <code>false</code> otherwise.
     */
    public boolean assertRequiredElement(XMLElement subElem, String name, XMLElement source, int category) {
        if (((source instanceof BPELElement)
                && !mValConfig.getBooleanProperty(ValidateConfiguration.BPEL_SYNTAX_ELEM_REQUIRED))) {
            return true;
        }
        
        if (null == subElem) {
            return fireToDo(new ToDoEvent(
                source,
                category,
                Severity.ERROR,
                mMsg.getString(VAL_MISSING_ELEM, name),
                mMsg.getString(FIX_MISSING_ELEM, name)));
        }
        return true;
    }
    

     /**
     * @param pReceive receive
     * @param category the severity
     * @return yes if the start is enabled.
     */
    public boolean isStartEnabled(Receive pReceive, int category) {
        if (pReceive.getCreateInstance()!= null && pReceive.getCreateInstance().equalsIgnoreCase(YES)) {
            /*
            //make sure at least one correlation set is used.
            Correlations cs = pReceive.getCorrelations();
            if (cs == null || cs.getCorrelationSize() <= 0) {
                return fireToDo(new ToDoEvent(
                    pReceive, category, Severity.ERROR,
                    mMsg.getString(VAL_START_ENABLED_RECEIVE_HAS_ZERO_CORRELATIONSETS),
                    mMsg.getString(FIX_START_ENABLED_RECEIVE_HAS_ZERO_CORRELATIONSETS)));
            }
            */
            return true;
        } 
        return false;
    }

    /**
     * @param pick pick
     * @param category the severity
     * @return yes if the start is enabled.
     */
    public boolean isStartEnabled(Pick pick, int category) {
        if (pick.getCreateInstance()!= null && pick.getCreateInstance().equalsIgnoreCase(YES)) {
            /*
            //make sure at least one correlation set is used.
            Collection messageList = pick.getOnMessages();
            if (messageList == null || messageList.size() == 0) {
                return fireToDo(new ToDoEvent(
                    pick, category, Severity.ERROR,
                    mMsg.getString(VAL_START_ENABLED_PICK_HAS_ZERO_CORRELATIONSETS),
                    mMsg.getString(FIX_START_ENABLED_PICK_HAS_ZERO_CORRELATIONSETS)));
            }
            for (Iterator iter = messageList.iterator(); iter.hasNext();) {
                OnMessage om = (OnMessage) iter.next();
                Correlations cs = om.getCorrelations();
                if (cs != null && cs.getCorrelationSize() > 0) {
                    return true;
                }
            }
            return fireToDo(new ToDoEvent(
                    pick, category, Severity.ERROR,
                    mMsg.getString(VAL_START_ENABLED_PICK_HAS_ZERO_CORRELATIONSETS),
                    mMsg.getString(FIX_START_ENABLED_PICK_HAS_ZERO_CORRELATIONSETS)));
            */
            return true;
        } else {
            return false;
        }
    }

    /**
     * @param category the category
     * @param startEnabledList the list of enabled activities
     * @param pDoc the bpeldocument
     * @return yes to continue
     */
    public boolean handleCreateInstance(BPELProcess p, List startEnabledList, int category) {
        if (startEnabledList != null && startEnabledList.size() == 0) {
            //get the BP
            if (p == null) {
                return true;
            }
            return fireToDo(new ToDoEvent(p, category, ToDoEvent.Severity.ERROR, 
                        this.mMsg.getString(VAL_NO_START_ACTIVITY),
                        this.mMsg.getString(FIX_NO_START_ACTIVITY)));
        }
        return true;
    }
    
    public void assertContainer(XMLElement source, BPELDocument pDoc, String containerQNameStr, int category) {
    	Variables containers = pDoc.getDocumentProcess().getVariables();
    	QName containerQName = NamespaceUtility.resolveAndGetQName(
    				containerQNameStr, source);
    	Variable container = containers.getVariable(containerQName.getLocalPart());
    	if(container == null) {
    		 fireToDo(new ToDoEvent(source, category, ToDoEvent.Severity.ERROR, 
                    this.mMsg.getString(VAL_MISSING_VARIABLE, containerQNameStr),
                    this.mMsg.getString(FIX_MISSING_VARIABLE, containerQNameStr)));
    	}
    
    }
    
    public void assertContainerPart(XMLElement source, 
    								BPELDocument pDoc, 
									String containerNCName, 
									String partNCName, 
									int category) {
    	Variables containers = pDoc.getDocumentProcess().getVariables();
    	
    	Variable container = containers.getVariable(containerNCName);
    	if(container != null) {
    		 //found container, now check if the
    		 // message which this container refers to has
    		 //the given part
    		 QName messageQName = container.getMessageType();
    		 if(messageQName != null) {
    		 	
    		 	String prefix = messageQName.getPrefix();
    		 	
    		 	if(prefix != null) {
    		 		BPELProcess process = pDoc.getDocumentProcess();
    		 		String ns = process.getNamespace(prefix);
    		 		
    		 		if(ns != null) {
    		 			//get the wsdl with this namespace and
    		 			//check for message
    		 			String msgLocalName = messageQName.getLocalPart();
    		 			Collection wsdls = process.getImportedWSDLDefinitions(ns);
    		 			Iterator it = wsdls.iterator();
    		 			while(it.hasNext()) {
    		 				Definition wsdlDefinition = (Definition) it.next();
    		 				Message msg = wsdlDefinition.getMessage(messageQName);
    		 				if(msg != null) {
    		 					//now check if msg has the part
    		 					Part part = msg.getPart(partNCName);
    		 					if(part == null) {
    		 						fireToDo(new ToDoEvent(source, category, ToDoEvent.Severity.ERROR, 
    		 			                    this.mMsg.getString(VAL_INVALID_CONTAINER_PART, partNCName, containerNCName),
    		 			                    this.mMsg.getString(FIX_INVALID_CONTAINER_PART, partNCName, containerNCName)));
    		 					}
    		 				}
    		 			}
    		 		}
    		 	}
    		 	
    		 }
    	}
    
    }
    
    public void assertServiceLinkType(QName serviceLinkTypeQName, PartnerLink p, BPELDocument pDoc, int category) {
    	if(serviceLinkTypeQName == null) {
    		return;
    	}
    	PartnerLinkType partnerLinkType = p.getBPELPartnerLinkType();
    	if(partnerLinkType == null) {
    			fireToDo(new ToDoEvent(p, category, ToDoEvent.Severity.ERROR, 
                        this.mMsg.getString(VAL_INVALID_PARTNERLINKTYPE, serviceLinkTypeQName.toString()),
                        this.mMsg.getString(FIX_INVALID_PARTNERLINKTYPE, serviceLinkTypeQName.toString())));
    	}
    }
    
    public void assertServiceLinkTypeRole(String serviceLinkRole,
    									  PartnerLink p, 
										  BPELDocument pDoc, 
										  int category) {
    	boolean found = false;
    	
    	PartnerLinkType partnerLinkType = p.getBPELPartnerLinkType();
    	
    	if(partnerLinkType != null) {
    		Collection roles = partnerLinkType.getRoles();
			Iterator itRoles = roles.iterator();
			while(itRoles.hasNext()) {
				PartnerLinkRole sRole = (PartnerLinkRole) itRoles.next();
				if(serviceLinkRole.equals(sRole.getName())) {
					found = true;
					break;
				}
			}
    	}
    		
    		if(!found) {
    			fireToDo(new ToDoEvent(p, category, ToDoEvent.Severity.ERROR, 
                        this.mMsg.getString(VAL_INVALID_PARTNERLINKTYPE_ROLE, serviceLinkRole),
                        this.mMsg.getString(FIX_INVALID_PARTNERLINKTYPE_ROLE, serviceLinkRole)));
    		}
    	
    }
    
    public void assertBPELProcessNamespace(BPELProcess p) {
    	//make sure bpel process is from corrrect bpel namespace
    	
    	String qName = p.getQualifiedName();
    	QName processQName = NamespaceUtility.resolveAndGetQName(qName, p);
    	String nsURI = processQName.getNamespaceURI();
    	String prefix = processQName.getPrefix();
    	if(nsURI == null) {
    		if(prefix != null) {
    			nsURI = p.getNamespace(prefix);
    		}
    	}
    	
    	//either process  tag is namespace qualified to process namespace
    	//or default namespace should be process namespace
    	if(nsURI != null) {
    		//error: qualified namespace of process element does not match process namespace
    		if(!nsURI.equals(BPELDocument.BPEL_NAMESPACE)) {
    			fireToDo(new ToDoEvent(p, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR, 
                        this.mMsg.getString(VAL_INVALID_QUALIFIED_NAMESPACE_OF_BPEL_PROCESS, nsURI),
                        this.mMsg.getString(FIX_INVALID_QUALIFIED_NAMESPACE_OF_BPEL_PROCESS, BPELDocument.BPEL_NAMESPACE)));
    			
    		}
    	} else {
    		String defaultNS = p.getDefaultNamespace();
        	if(defaultNS != null) {
        		//error: default namespace does not match process namespace
        		if(!defaultNS.equals(BPELDocument.BPEL_NAMESPACE)) {
        			fireToDo(new ToDoEvent(p, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR, 
                            this.mMsg.getString(VAL_INVALID_DEFAULT_NAMESPACE_OF_BPEL_DOCUMENT, defaultNS),
                            this.mMsg.getString(FIX_INVALID_DEFAULT_NAMESPACE_OF_BPEL_DOCUMENT, BPELDocument.BPEL_NAMESPACE)));
        			
        		}
        		
        	} else {
        		//error: default namespace is not specified,
        		//either default namespace should be process namespace
        		//or each bpel element should be namespace qualified to
        		//process namesace
        		
        		fireToDo(new ToDoEvent(p, ToDoEvent.Category.BPEL_SYNTAX, ToDoEvent.Severity.ERROR, 
                        this.mMsg.getString(VAL_MISSING_BPEL_PROCESS_NAMESPACE, BPELDocument.BPEL_NAMESPACE),
                        this.mMsg.getString(FIX_MISSING_BPEL_PROCESS_NAMESPACE, BPELDocument.BPEL_NAMESPACE)));
        	}
    	}
    }
}
