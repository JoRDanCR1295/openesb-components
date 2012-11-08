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
 * @(#)BPELProcess.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


import java.util.Collection;
import java.util.List;
import java.util.Map;

import javax.wsdl.Message;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeLoader;

import com.sun.bpel.xml.common.model.XMLDocumentElement;
import com.sun.wsdl4j.ext.SchemaTypeLoaderHolder;
import com.sun.wsdl4j.ext.bpel.MessageProperty;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;

/**
 * Describes the &lt;process&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface BPELProcess extends BPELProcessOrScope, 
                                     XMLDocumentElement, 
                                     SingleActivityHolder, 
                                     NamedElement,
                                     SchemaTypeLoaderHolder {
    /** Tag for this element */
    public static final String TAG = "process";
    
    /** The namespace for sun extension element for trace */
    public static final String SUN_BPEL_EXTN_TRACE_NAMESPACE = 
    	"http://www.sun.com/wsbpel/2.0/process/executable/SUNExtension/Trace";
    
    /** The namespace for sun extension element for trace */
    public static final String SUN_BPEL_EXTN_TRANSACTION_NAMESPACE = 
    	"http://www.sun.com/wsbpel/2.0/process/executable/SUNExtension/Transaction";
    
    public static final String SUN_BPEL_EXTN_NAMESPACE = 
    	"http://www.sun.com/wsbpel/2.0/process/executable/SUNExtension/";
    
    /** The namespace for the sun attachment extension*/
    public static final String SUN_BPEL_EXTN_ATTACHMENT_NAMESPACE = 
    	"http://www.sun.com/wsbpel/2.0/process/executable/SUNExtension/Attachment";

    /** The namespace for sun extension element for data handling */
    public static final String SUN_BPEL_EXTN_DATA_HANDLING = 
        "http://www.sun.com/wsbpel/2.0/process/executable/SUNExtension/DataHandling";
    
    /** Enumerated values for an optional ATOMIC_TX_TYPE type */
    public static final String[] ATOMIC_TX_TYPES = {"Supports", "Required"};

    
    
    /** Describes the attributes for this element */
    public interface ATTR extends NamedElement.ATTR {
        
        /** "targetNamespace" attribute token */
        public static final String TARGET_NAMESPACE = "targetNamespace";
        
        /** "queryLanguage" attribute token */
        public static final String QUERY_LANGUAGE = "queryLanguage";
        
        /** "expressionLanguage" attribute token */
        public static final String EXPRESSION_LANGUAGE = "expressionLanguage";
        
        /** "suppressJoinFailure" attribute token */
        public static final String SUPPRESS_JOIN_FAILURE =
            "suppressJoinFailure";
        
        /** "enableInstanceCompensation" attribute token */
        public static final String ENABLE_INSTANCE_COMPENSATION =
            "enableInstanceCompensation";
        
        /** "abstractProcess" attribute token */
        public static final String ABSTRACT_PROCESS = "abstractProcess";
        
        /** "atomic" attribute token. Sun extension */
        public static final String ATOMIC = "atomic";
        
        /** "ignoreMissingFromData" attribute token. Sun extension */
        public static final String IGNORE_MISSING_FROM_DATA = "ignoreMissingFromData";
        
        /** "waitingRequestLifeSpan" attribute token. Sun extension */
        public static final String WAITING_REQUEST_LIFE_SPAN = "waitingRequestLifeSpan";
        
        /** "persistenceOptOut" attribute token. Sun extension */
        public static final String PERSISTENCE_OPT_OUT = "persistenceOptOut";
        
        /** "generateEvents" attribute token. Sun extension */
        public static final String GENERATE_EVENTS = "generateEvents";
        
        /** "atomicTxType" attribute token. Sun extension */
        public static final String ATOMIC_TX_TYPE = "atomicTxType";

        /** "enableLogging" attribute token. Sun extension */
        public static final String ENABLE_LOGGING = "enableLogging";

        /** "extraNDC" attribute token. Sun extension */
        public static final String EXTRA_NDC = "extraNDC";

    }
    
    /** Ordinal position of targetNamespace attribute. */
    public static final int TARGET_NAMESPACE = NAME + 1;
    
    /** Ordinal position of queryLanguage attribute. */
    public static final int QUERY_LANGUAGE = TARGET_NAMESPACE + 1;
    
    /** Ordinal position of expressionLanguage attribute. */
    public static final int EXPRESSION_LANGUAGE = QUERY_LANGUAGE + 1;
    
    /** Ordinal position of suppressJoinFailure attribute. */
    public static final int SUPPRESS_JOIN_FAILURE = EXPRESSION_LANGUAGE + 1;
    
    /** Ordinal position of enableInstanceCompensation attribute. */
    public static final int ENABLE_INSTANCE_COMPENSATION = SUPPRESS_JOIN_FAILURE
                                                           + 1;
    /** Ordinal position of abstractProcess attribute. */
    public static final int ABSTRACT_PROCESS = ENABLE_INSTANCE_COMPENSATION + 1;
    
    /** Ordinal position of atomic attribute. */
    public static final int ATOMIC = ABSTRACT_PROCESS + 1;
    
    /** Ordinal position of ignoreMissingFromData attribute. */
    public static final int IGNORE_MISSING_FROM_DATA = ATOMIC + 1;
    
    /** Ordinal position of waitingRequestLifeSpan attribute. */
    public static final int WAITING_REQUEST_LIFE_SPAN = IGNORE_MISSING_FROM_DATA + 1;
    
    /** ordinal position of persistenceOptOut attribute. */
    public static final int PERSISTENCE_OPT_OUT = WAITING_REQUEST_LIFE_SPAN + 1;
    
    /** ordinal position of generateEvents attribute. */
    public static final int GENERATE_EVENTS = PERSISTENCE_OPT_OUT + 1;
    
    /** ordinal position of atomicTxType attribute. */
    public static final int ATOMIC_TX_TYPE = GENERATE_EVENTS + 1; 

    /** ordinal position of enableLogging attribute. */
    public static final int ENABLE_LOGGING = ATOMIC_TX_TYPE + 1;

    /** ordinal position of extraNDC attribute. */
    public static final int EXTRA_NDC = ENABLE_LOGGING + 1;
    
    /** The xsd namespace uri **/
    public static final String XSD_NAME_SPACE_URI = "http://www.w3.org/2001/XMLSchema";
    
    public static final String XSD_NAME_SPACE_PREFIX = "xsd";
    
    
    /** Setter for property name.
     * @param qName New qName of property name.
     * @param name  New value of property name.
     *
     */
    void setName(String qName, String name);
    
    /** Setter for property targetNamespace.
     * @param qName             New qName of property targetNamespace.
     * @param targetNamespace   New value of property targetNamespace.
     *
     */
    void setTargetNamespace(String qName, String targetNamespace);
    
    /** Getter for property queryLanguage.
     * @return Value of property queryLanguage.
     *
     */
    String getQueryLanguage();
    
    /** Setter for property name.
     * @param queryLanguage  New value of property queryLanguage.
     *
     */
    void setQueryLanguage(String queryLanguage);
    
    /** Setter for property queryLanguage.
     * @param qName         New qName of property queryLanguage.
     * @param queryLanguage New value of property queryLanguage.
     *
     */
    void setQueryLanguage(String qName, String queryLanguage);
    
    /** Getter for property expressionLanguage.
     * @return Value of property expressionLanguage.
     *
     */
    String getExpressionLanguage();
    
    /** Setter for property expressionLanguage.
     * @param expressionLanguage    New value of property expressionLanguage.
     *
     */
    void setExpressionLanguage(String expressionLanguage);
    
    /** Setter for property expressionLanguage.
     * @param qName                 New qName of property expressionLanguage.
     * @param expressionLanguage    New value of property expressionLanguage.
     *
     */
    void setExpressionLanguage(String qName, String expressionLanguage);
    
    /** Getter for property suppressJoinFailure.
     * @return Value of property suppressJoinFailure.
     *
     */
    String getSuppressJoinFailure();
    
    /** Setter for property suppressJoinFailure.
     * @param suppressJoinFailure  New value of property suppressJoinFailure.
     *
     */
    void setSuppressJoinFailure(String suppressJoinFailure);
    
    /** Setter for property suppressJoinFailure.
     * @param qName                 New qName of property suppressJoinFailure.
     * @param suppressJoinFailure   New value of property suppressJoinFailure.
     *
     */
    void setSuppressJoinFailure(String qName, String suppressJoinFailure);
    
    /** Getter for property enableInstanceCompensation.
     * @return Value of property enableInstanceCompensation.
     *
     */
    String getEnableInstanceCompensation();
    
    /** Setter for property enableInstanceCompensation.
     * @param enableInstanceCompensation    New value of property
                                              enableInstanceCompensation.
     *
     */
    void setEnableInstanceCompensation(String enableInstanceCompensation);
    
    /** Setter for property enableInstanceCompensation.
     * @param qName                         New qName of property
     *                                        enableInstanceCompensation.
     * @param enableInstanceCompensation    New value of property
     *                                        enableInstanceCompensation.
     *
     */
    void setEnableInstanceCompensation(String qName,
                                       String enableInstanceCompensation);
    
    /** Getter for property abstractProcess.
     * @return Value of property abstractProcess.
     *
     */
    String getAbstractProcess();

    /** Setter for property abstractProcess.
     * @param abstractProcess New value of property abstractProcess.
     *
     */
    void setAbstractProcess(String abstractProcess);
    
    /** Setter for property abstractProcess.
     * @param qName             New qName of property abstractProcess.
     * @param abstractProcess   New value of property abstractProcess.
     *
     */
    void setAbstractProcess(String qName, String abstractProcess);
    
    /** Getter for property ignoreMissingFromData
     * @return Value of property ignoreMissingFromData
     */
    String getIgnoreMissingFromData();
    
    /** Setter for property ignoreMissingFromData
     * @param ignoreMissingFromData New value of property ignoreMissingFromData
     */
    void setIgnoreMissingFromData(String ignoreMissingFromData);
    
    /** Setter for property ignoreMissingFromData
     * @param qName		New qName of property ignoreMissingFromData
     * @param ignoreMissingFromData	New value of property ignoreMissingFromData
     */
    void setIgnoreMissingFromData(String qName, String ignoreMissingFromData);
    
    /** Getter for property waitingRequestLifeSpan
     * @return Value of property waitingRequestLifeSpan
     */
    String getWaitingRequestLifeSpan();
    
    /** Setter for property waitingRequestLifeSpan
     * @param waitingRequestLifeSpan New value of property waitingRequestLifeSpan
     */
    void setWaitingRequestLifeSpan(String waitingRequestLifeSpan);
    
    /** Setter for property waitingRequestLifeSpan
     * @param qName		New qName of property waitingRequestLifeSpan
     * @param waitingRequestLifeSpan	New value of property waitingRequestLifeSpan
     */
    void setWaitingRequestLifeSpan(String qName, String waitingRequestLifeSpan);
    
    /** Getter for property generateEvents
     * @return Value of property generateEvents
     */
    String getGenerateEvents();
    
    /** Setter for property generateEvents
     * @param waitingRequestLifeSpan New value of property generateEvents
     */
    void setGenerateEvents(String generateEvents);
    
    /** Setter for property generateEvents
     * @param qName		New qName of property generateEvents
     * @param generateEvents	New value of property generateEvents
     */
    void setGenerateEvents(String qName, String generateEvents);
    
    /** Getter for property persistenceOptOut
     * @return Value of property persistenceOptOut
     */
    String getPersistenceOptOut();
    
    /** Setter for property persistenceOptOut
     * @param waitingRequestLifeSpan New value of property persistenceOptOut
     */
    void setPersistenceOptOut(String persistenceOptOut);
    
    /** Setter for property persistenceOptOut
     * @param qName		New qName of property persistenceOptOut
     * @param persistenceOptOut	New value of property persistenceOptOut
     */
    void setPersistenceOptOut(String qName, String persistenceOptOut);
    
    /** Getter for property atomic
     * @return Value of property atomic
     */
    String getAtomic();
    
    /** Setter for property atomic
     * @param atomic New value of property atomic
     */
    void setAtomic(String atomic);
    
    /** Setter for property atomic
     * @param qName		New qName of property atomic
     * @param atomic	New value of property atomic
     */
    void setAtomic(String qName, String atomic);
    
    
    /** Getter for property faultHandlers.
     * @return Value of property faultHandlers.
     *
     */
    FaultHandlers getFaultHandlers();
    
    /** Setter for property faultHandlers.
     * @param faultHandlers New value of property faultHandlers.
     *
     */
    void setFaultHandlers(FaultHandlers faultHandlers);
    
    /** 
     * Get all the source partners - partners associated with receives and 
     * onMessages
     * @return List An arrayList of partner objects
     */
    List getSourcePartners();

    /** 
     * Get all the destination partners - partners associated with invokes
     * @return List An arrayList of partner objects
     */
    List getDestinationPartners();

    /**
     * Get all the invokes in the business process
     * @return List An arrayList of activities
     *  
     */
    List getInvokes();

    /**
     * Get all the activies of a particular type in the business process
     * @param type - Type of activities that needs to be obtained. The type is the 
     * class name of the required activities, e.g. com.sun.bpel.model.Invoke
     * @return List An arrayList of activities
     */
    List getActivities(String type);

    /**
     * Get all the activies of a particular type in the given activity
     * @param type - Type of activities that needs to be obtained. The type is the 
     * class name of the required activities, e.g. com.sun.bpel.model.Invoke
     * @param currentActivity - the activity that is the root of the activity tree
     * @return List An arrayList of activities
     */
    List getActivities(String type, Activity currentActivity);
    
    /**
     * add a new import element.
     * The <import> element is used within a BPEL4WS process to explicitly indicate a dependency on
     *  external XML Schema or WSDL definitions. Any number of <import> elements may appear as
     *  initial children of the <process> element, before any other child element.
     *  
     * @param newImport new Import element.
     */
    void addImport(Import newImport);
    
    /**
     * remove an old import element.
     * The <import> element is used within a BPEL4WS process to explicitly indicate a dependency on
     *  external XML Schema or WSDL definitions. Any number of <import> elements may appear as
     *  initial children of the <process> element, before any other child element.
     *  
     * @param oldImport new Import element.
     */
    void removeImport(Import oldImport);
    
    /**
     * get the list of all import elements.
     * The <import> element is used within a BPEL4WS process to explicitly indicate a dependency on
     *  external XML Schema or WSDL definitions. Any number of <import> elements may appear as
     *  initial children of the <process> element, before any other child element.
     *  
     * @return List list of Import objects
     */
    List getImports();
    
    /**
     * get the list of all import elements matching namespace.
     * The <import> element is used within a BPEL4WS process to explicitly indicate a dependency on
     *  external XML Schema or WSDL definitions. Any number of <import> elements may appear as
     *  initial children of the <process> element, before any other child element.
     *  
     * @return List list of Import objects
     */
    List getImports(String namespace);
    
    /**
     * Returns a collection of all imported WSDLDefinition objects
     * @return collection all imported WSDLDefinition.
     */
    Collection getAllImportedWSDLDefinitions();
    
    /**
     * Returns a collection of imported WSDLDefinition objects which has given targetNamespace
     * @return collection of imported WSDLDefinition matching targetNamespace or empty collection
     */
    Collection getImportedWSDLDefinitions(String targetNamespace);
    
    
    /**
     * Returns a collection of all imported XMLSchema objects
     * @return collection all imported XMLSchema.
     */
    Collection getAllImportedXMLSchemas();
    

    /**
     * Returns a collection of imported XMLSchema objects which has given targetNamespace
     * @return collection of imported XMLSchema matching targetNamespace or empty collection
     */
    Collection getImportedXMLSchemas(String targetNamespace);
    
    /**
     * Get WSDLMessage Given its QName.
     * Look into all imported WSDL for given message
     * @param qName QName of the message
     * @return WSDLMessage
     */
    Message getWSDLMessage(QName qName);
    
    /**
     * Get PortType Given its QName.
     * @param qName the name of the port type
     * @return the port type or null if not found
     */
    PortType getPortType(QName qName);
    
    
    /**
     * Returns a collection of all port types defined
     * in all imported wsdls.
     * @return collection of port types.
     */
    Collection getAllPortTypes();
    
    
    /**
     * Get Variable matching give wsdl message QName.
     * Finds the variable which has messageType attribute
     * refer to WSDLMessage with given message QName.
     * If QName specifies namespace use it to find 
     * WSDLDefinition matching its targetNamespace
     * and then find WSDLMessage with given name.
     * If prefix is passed it first tries to find
     * namespace for prefix. 
     * @param messageQName WSDLMessage QName
     * @return Collection of bpel variable or empty collection
     */
    Collection getBPELVariables(QName messageQName);
    
    /**
     * Given a BPELElement find out all bpel variable which are
     * available. local variable takes precedence (in case of scope which also defines variables) over
     * process level variables. 
     * @param element BPELElement for which all bpel variables are applicable.
     * @return Collection of all bpel variables or empty collection
     */
    Collection getAllBPELVariables(BPELElement element);
    
    /**
     * Get Property object given its QName.
     * @param propertyQName QName of property
     * @return Property object or null
     */
    MessageProperty getBPELProperty(QName propertyQName);
    
    /**
     * Get ElementDecl object given its QName.
     * First looks into imported wsdls.
     * Then looks into imported xsds.
     * @param elementQName
     * @return
     */
    SchemaGlobalElement getXSDElement(QName elementQName);
    
    /**
     * Get XMLType  (Simple or Complex) object given its QName.
     * First looks into imported wsdls.
     * Then looks into imported xsds.
     * @param elementQName
     * @return
     */
    SchemaType getXSDType(QName elementQName);
    
    /** 
     * @param propName
     * @return a collection of propertyaliases that are associated with the 
     * given property name
     */
    Collection<MessagePropertyAlias> getBPELPropertyAlias(QName propName);
    
    /**
     * Sets the schema type loader of this BPEL process. This method should
     * only be used as a overriding mechanism since BPEL model will create
     * the loader once a BPEL document is loaded.
     * 
     * @param loader The schema type loader
     */
    void setSchemaTypeLoader(SchemaTypeLoader loader);
    
    /**
     * Gets the schema type loader of this BPEL document. The type loader is
     * responsible for looking up XML schema components that are reachable
     * from the BPEL process based on BPEL document linking rules.
     *  
     * @return The schema type loader.
     */
    SchemaTypeLoader getSchemaTypeLoader();
    
    void setNMPropToPropAliasMap();
    
    Map<QName, Collection<MessagePropertyAlias>> getNMPropToPropAliasMap();
    
    /** Getter for property atomicTxType
     * @return Value of property atomicTxType
     */
    String getAtomicTxType();
    
    /** Setter for property atomic
     * @param atomic New value of property atomicTxType
     */
    void setAtomicTxType(String atomic);
    
    /** Setter for property atomic
     * @param qName		New qName of property atomicTxType
     * @param atomic	New value of property atomicTxType
     */
    void setAtomicTxType(String qName, String atomic);

    /** Getter for property enableLogging
     * @return Value of property enableLogging
     */
    String getEnableLogging();

    /** Setter for property enableLogging
     * @param enableLogging New value of property enableLogging
     */
    void setEnableLogging(String enableLogging);

    /** Setter for property enableLogging
     * @param qName		New qName of property enableLogging
     * @param enableLogging	New value of property enableLogging
     */
    void setEnableLogging(String qName, String enableLogging);

    /** Getter for property extraNDC
     * @return Value of property extraNDC
     */
    String getExtraNDC();

    /** Setter for property extraNDC
     * @param extraNDC New value of property extraNDC
     */
    void setExtraNDC(String extraNDC);

    /** Setter for property extraNDC
     * @param qName		New qName of property extraNDC
     * @param enableLogging	New value of property extraNDC
     */
    void setExtraNDC(String qName, String extraNDC);

    /**
     * Method added for monitoring API to provide serialized BPEL document
     * 
     * @return
     * @throws SAXException
     */
    String getSerializedBPELDocument();
    
}
