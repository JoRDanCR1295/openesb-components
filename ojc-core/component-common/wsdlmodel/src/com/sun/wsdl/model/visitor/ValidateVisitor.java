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

package com.sun.wsdl.model.visitor;

import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Properties;
import java.util.logging.Logger;


import com.sun.wsdl.model.Binding;
import com.sun.wsdl.model.BindingFault;
import com.sun.wsdl.model.BindingInput;
import com.sun.wsdl.model.BindingOperation;
import com.sun.wsdl.model.BindingOutput;
import com.sun.wsdl.model.Import;
import com.sun.wsdl.model.Operation;
import com.sun.wsdl.model.OperationFault;
import com.sun.wsdl.model.OperationInput;
import com.sun.wsdl.model.OperationOutput;
import com.sun.wsdl.model.Part;
import com.sun.wsdl.model.PortType;
import com.sun.wsdl.model.Service;
import com.sun.wsdl.model.PartnerLinkRole;
import com.sun.wsdl.model.PartnerLinkType;
import com.sun.wsdl.model.ServicePort;
import com.sun.wsdl.model.Types;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.WSDLDocument;
import com.sun.wsdl.model.WSDLMessage;
import com.sun.wsdl.model.bpel.Property;
import com.sun.wsdl.model.bpel.PropertyAlias;
import com.sun.wsdl.model.bpel.Query;
import com.sun.wsdl.model.common.MessageManager;
import com.sun.wsdl.model.common.model.Documentation;
//import com.sun.wsdl.model.common.model.QName;
import com.sun.wsdl.model.common.model.XMLComment;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.XMLProcessingInstruction;
import com.sun.wsdl.model.common.todotask.ToDoEvent;
import com.sun.wsdl.model.common.todotask.ToDoListener;
import com.sun.wsdl.model.common.visitor.ParentChildrenVisitor;
import com.sun.wsdl.model.common.visitor.ValidateConfiguration;
import com.sun.wsdl.model.common.visitor.ValidateSupport;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.extensions.soap.SOAPAddress;
import com.sun.wsdl.model.extensions.soap.SOAPBinding;
import com.sun.wsdl.model.extensions.soap.SOAPBody;
import com.sun.wsdl.model.extensions.soap.SOAPFault;
import com.sun.wsdl.model.extensions.soap.SOAPHeader;
import com.sun.wsdl.model.extensions.soap.SOAPHeaderFault;
import com.sun.wsdl.model.extensions.soap.SOAPOperation;



/**
 * Visits the model nodes and validates them.
 *
 * @author Sun Microsystems
 * TODO: add validation for extensibility elements like bpws: property, propertyAlias
 * in a generic way. someone can register a validator for these and validate them 
 * @version 
 */
public class ValidateVisitor extends AbstractVisitor implements ParentChildrenVisitor {
    
    /** The logger. */
    private static Logger mLogger = Logger.getLogger(ValidateVisitor.class.getName());

    /** MessageManager for localized strings. */    
    private static MessageManager mMsg = MessageManager.getManager(ValidateVisitor.class);
    
    /** Validate configuration singleton. */
    private static ValidateConfiguration mValConfig;

    /** Fault can not be thrown by one-way or notification type operation */
    public static final String VAL_FAULT_NOT_ALLOWED_IN_OPERATION = "VAL_FAULT_NOT_ALLOWED_IN_OPERATION";  // Not I18N
    
    /** Fix 'Fault can not be thrown by one-way or notification type operation'
     by removing faults */
    public static final String FIX_FAULT_NOT_ALLOWED_IN_OPERATION = "FIX_FAULT_NOT_ALLOWED_IN_OPERATION";  // Not I18N

    /** Message not found for operation input */
    public static final String VAL_MESSAGE_NOT_FOUND_IN_OPERATION_INPUT = 
        "VAL_MESSAGE_NOT_FOUND_IN_OPERATION_INPUT"; 

    /** Message not found for operation output */
    public static final String VAL_MESSAGE_NOT_FOUND_IN_OPERATION_OUTPUT = 
        "VAL_MESSAGE_NOT_FOUND_IN_OPERATION_OUTPUT";

    /** Message not found for operation fault */
    public static final String VAL_MESSAGE_NOT_FOUND_IN_OPERATION_FAULT = 
        "VAL_MESSAGE_NOT_FOUND_IN_OPERATION_FAULT";

    /** Fix for message not found for operation input */
    public static final String FIX_MESSAGE_NOT_FOUND_IN_OPERATION_INPUT = 
        "VAL_MESSAGE_NOT_FOUND_IN_OPERATION_FAULT";

    /** Fix for Message not found for operation output */
    public static final String FIX_MESSAGE_NOT_FOUND_IN_OPERATION_OUTPUT = 
        "FIX_MESSAGE_NOT_FOUND_IN_OPERATION_OUTPUT"; 

    /** Fix for Message not found for operation fault */
    public static final String FIX_MESSAGE_NOT_FOUND_IN_OPERATION_FAULT = 
        "FIX_MESSAGE_NOT_FOUND_IN_OPERATION_FAULT"; 

    /** Schema in part not found */
    public static final String VAL_SCHEMA_DEFINED_NOT_FOUND = "VAL_SCHEMA_DEFINED_NOT_FOUND";

    /** Fix for Schema in part not found */
    public static final String FIX_SCHEMA_DEFINED_NOT_FOUND = "FIX_SCHEMA_DEFINED_NOT_FOUND";

    /** Schema is not defined in part */
    public static final String VAL_NO_SCHEMA_DEFINED = "VAL_NO_SCHEMA_DEFINED";

    /** Fix for Schema is not defined in part */
    public static final String FIX_NO_SCHEMA_DEFINED = "FIX_NO_SCHEMA_DEFINED";

    /** partnerLinkType portType does not exist in wsdl file */
    public static final String VAL_NO_PARTNERLINKTYPE_PORTTYPE_DEFINED_IN_WSDL = "VAL_NO_PARTNERLINKTYPE_PORTTYPE_DEFINED_IN_WSDL";

    /** Fix for partnerLinkType portType does not exist in wsdl file */
    public static final String FIX_NO_PARTNERLINKTYPE_PORTTYPE_DEFINED_IN_WSDL = "FIX_NO_PARTNERLINKTYPE_PORTTYPE_DEFINED_IN_WSDL";
    
    /**WSDLMessage has zero parts so it is valid as per wsdl schema but we need a warning*/
    public static final String VAL_WARNING_WSDL_MESSAGE_DOES_NOT_HAVE_ANY_PARTS_DEFINED = "VAL_WARNING_WSDL_MESSAGE_DOES_NOT_HAVE_ANY_PARTS_DEFINED";

    /**WSDLMessage has zero parts so it is valid as per wsdl schema but we need a warning*/
    public static final String FIX_WARNING_WSDL_MESSAGE_DOES_NOT_HAVE_ANY_PARTS_DEFINED = "FIX_WARNING_WSDL_MESSAGE_DOES_NOT_HAVE_ANY_PARTS_DEFINED";

    /** part does not have element or type attribute */
    public static final String VAL_NO_ELEMENT_OR_TYPE_ATTRIBUTE_DEFINED_IN_MESSAGE_PART = "VAL_NO_ELEMENT_OR_TYPE_ATTRIBUTE_DEFINED_IN_PART";

    /** part does not have element or type attribute */
    public static final String FIX_VAL_NO_ELEMENT_OR_TYPE_ATTRIBUTE_DEFINED_IN_PART = "FIX_VAL_NO_ELEMENT_OR_TYPE_ATTRIBUTE_DEFINED_IN_PART";
    
    /**part has element attribute but the referenced element object can not be located*/
    public static final String VAL_ELEMENT_ATTRIBUTE_DEFINED_IN_MESSAGE_PART_IS_NOT_VALID = "VAL_ELEMENT_ATTRIBUTE_DEFINED_IN_MESSAGE_PART_IS_NOT_VALID";

    /**part has element attribute but the referenced element object can not be located*/
    public static final String FIX_ELEMENT_ATTRIBUTE_DEFINED_IN_MESSAGE_PART_IS_NOT_VALID = "FIX_ELEMENT_ATTRIBUTE_DEFINED_IN_MESSAGE_PART_IS_NOT_VALID";

    /**part has type attribute but the referenced type object can not be located*/
    public static final String VAL_TYPE_ATTRIBUTE_DEFINED_IN_MESSAGE_PART_IS_NOT_VALID = "VAL_TYPE_ATTRIBUTE_DEFINED_IN_MESSAGE_PART_IS_NOT_VALID";

    /**part has element attribute but the referenced type object can not be located*/
    public static final String FIX_TYPE_ATTRIBUTE_DEFINED_IN_MESSAGE_PART_IS_NOT_VALID = "FIX_TYPE_ATTRIBUTE_DEFINED_IN_MESSAGE_PART_IS_NOT_VALID";

    /**Binding has wrong or missing PortType */
    public static final String VAL_MISSING_PORTTYPE_IN_BINDING = "VAL_MISSING_PORTTYPE_IN_BINDING";
    public static final String FIX_MISSING_PORTTYPE_IN_BINDING = "FIX_MISSING_PORTTYPE_IN_BINDING";
    
    /**Service Port has wrong or missing Binding */
    public static final String VAL_MISSING_BINDING_IN_SERVICE_PORT = "VAL_MISSING_BINDING_IN_SERVICE_PORT";
    public static final String FIX_MISSING_BINDING_IN_SERVICE_PORT = "FIX_MISSING_BINDING_IN_SERVICE_PORT";
    
    /**Import does not have imported document object */
    public static final String VAL_MISSING_IMPORTED_DOCUMENT = "VAL_MISSING_IMPORTED_DOCUMENT"; 
    public static final String FIX_MISSING_IMPORTED_DOCUMENT = "FIX_MISSING_IMPORTED_DOCUMENT"; 
    
    /** PortType operation input name should be unique across all operation inputs in a port type*/
    public static final String VAL_DUPLICATE_OPRATION_INPUT_NAME_IN_PORTTYPE = "VAL_DUPLICATE_OPRATION_INPUT_NAME_IN_PORTTYPE";
    public static final String FIX_DUPLICATE_OPRATION_INPUT_NAME_IN_PORTTYPE = "FIX_DUPLICATE_OPRATION_INPUT_NAME_IN_PORTTYPE";
    
    /** PortType operation output name should be unique across all operation outputs in a port type*/
    public static final String VAL_DUPLICATE_OPRATION_OUTPUT_NAME_IN_PORTTYPE = "VAL_DUPLICATE_OPRATION_OUTPUT_NAME_IN_PORTTYPE";
    public static final String FIX_DUPLICATE_OPRATION_OUTPUT_NAME_IN_PORTTYPE = "FIX_DUPLICATE_OPRATION_OUTPUT_NAME_IN_PORTTYPE";
    
    
    /** operation falut name should be unique across all operation faults*/
    public static final String VAL_DUPLICATE_OPRATION_FAULT_NAME = "VAL_DUPLICATE_OPRATION_FAULT_NAME";
    public static final String FIX_DUPLICATE_OPRATION_FAULT_NAME = "FIX_DUPLICATE_OPRATION_FAULT_NAME";
    
    /** binding operation name does not match name of portType operations*/
    public static final String VAL_OPERATION_DOES_NOT_EXIST_IN_PORT_TYPE = "VAL_OPERATION_DOES_NOT_EXIST_IN_PORT_TYPE";
    public static final String FIX_OPERATION_DOES_NOT_EXIST_IN_PORT_TYPE = "FIX_OPERATION_DOES_NOT_EXIST_IN_PORT_TYPE";
    
    
    /** Creates a new instance of ValidateVisitor */
    public ValidateVisitor() {
        
        // Find validate configuration
        String valConfigPropFile = System.getProperty("com.stc.wsdlmodel.model.visitor.ValidateConfiguration");
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
            defaults.setProperty(ValidateConfiguration.WSDL_SYNTAX_ATTRIB_REQUIRED, "true");
            defaults.setProperty(ValidateConfiguration.WSDL_SYNTAX_ATTRIB_QNAME, "true");
            defaults.setProperty(ValidateConfiguration.WSDL_SYNTAX_ATTRIB_NCNAME, "false");
            defaults.setProperty(ValidateConfiguration.WSDL_SYNTAX_ATTRIB_BOOLEAN, "true");
            defaults.setProperty(ValidateConfiguration.WSDL_SYNTAX_ELEM_MIN, "true");
            defaults.setProperty(ValidateConfiguration.WSDL_SYNTAX_ELEM_REQUIRED, "true");
            
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
    
    /** Prepares the visitor for use.
     * @param   v   Values to use.
     *              <ol start="0">
     *              <li><code>com.sun.wsdl.model.common.model.bpel.BPELDocument</code> Destination document.</li>
     *              </ol>
     */
    public void prepare(Object[] v) {
        // 1st argument is collection of ToDoListeners
        Collection toDoListeners = (Collection) v[0];
        Iterator iter = toDoListeners.iterator();
        while (iter.hasNext()) {
            getValidateSupport().addToDoListener((ToDoListener) iter.next());
        }
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
    
    /**
     * Visits a WSDL document.
     * @param d the document to visit
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(WSDLDocument d) {
        return true;
    }
    
    /**
     * Visits a WSDL definitions.
     * @param w a WSDL definitions element
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(WSDLDefinitions w) {
        return true;
    }
    
    /** Visits a wsdl:message element.
     * @param   w   a wsdl:message element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(WSDLMessage w) {
        getValidateSupport().assertRequiredAttrib(w.getName(), WSDLMessage.ATTR.NAME,
                                                       w, ToDoEvent.Category.WSDL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(w.getName(), WSDLMessage.ATTR.NAME,
                                                     w, ToDoEvent.Category.WSDL_SYNTAX);

        //if WSDLMessage does not have any part defined, show user a warning.
        if(w.getParts().size() == 0 ) {
        	getValidateSupport().fireToDo
            (new ToDoEvent
             (w, ToDoEvent.Category.WSDL_SEMANTICS, ToDoEvent.Severity.WARNING,
              mMsg.getString(VAL_WARNING_WSDL_MESSAGE_DOES_NOT_HAVE_ANY_PARTS_DEFINED, w.getName()),
              mMsg.getString(FIX_WARNING_WSDL_MESSAGE_DOES_NOT_HAVE_ANY_PARTS_DEFINED, w.getName())));
        }
        return true;
    }
    
    /** Visits a part element.
     * @param   p   a part element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(final Part p) {
        getValidateSupport().assertQNameAttrib(p.getType(), Part.ATTR.TYPE,
                                                    p, ToDoEvent.Category.WSDL_SYNTAX);
        
        
        getValidateSupport().assertQNameAttrib(p.getElement(), Part.ATTR.ELEMENT,
                                                    p, ToDoEvent.Category.WSDL_SYNTAX);
        
    
        //if both element and type attribute are missing then part is invalid
        if(p.getElement() == null && p.getType() == null) {
        	getValidateSupport().fireToDo
            (new ToDoEvent
             (p, ToDoEvent.Category.WSDL_SEMANTICS, ToDoEvent.Severity.ERROR,
              mMsg.getString(VAL_NO_ELEMENT_OR_TYPE_ATTRIBUTE_DEFINED_IN_MESSAGE_PART, p.getName()),
              mMsg.getString(FIX_VAL_NO_ELEMENT_OR_TYPE_ATTRIBUTE_DEFINED_IN_PART)));
        }
        
        //if element attribute is specified and xsd element object can not be resolved
        //then its an error.
        if(p.getElement() != null && p.getXSDElement() == null) {
        	getValidateSupport().fireToDo
            (new ToDoEvent
             (p, ToDoEvent.Category.WSDL_SEMANTICS, ToDoEvent.Severity.ERROR,
              mMsg.getString(VAL_ELEMENT_ATTRIBUTE_DEFINED_IN_MESSAGE_PART_IS_NOT_VALID, p.getName(), p.getElement().toString()),
              mMsg.getString(FIX_ELEMENT_ATTRIBUTE_DEFINED_IN_MESSAGE_PART_IS_NOT_VALID)));
        }
        
        //if type attribute is specified and xsd type object can not be resolved
        //then its an error.
        if(p.getType() != null && p.getXSDType() == null) {
        	getValidateSupport().fireToDo
            (new ToDoEvent
             (p, ToDoEvent.Category.WSDL_SEMANTICS, ToDoEvent.Severity.ERROR,
              mMsg.getString(VAL_TYPE_ATTRIBUTE_DEFINED_IN_MESSAGE_PART_IS_NOT_VALID, p.getName(), p.getType().toString()),
              mMsg.getString(FIX_TYPE_ATTRIBUTE_DEFINED_IN_MESSAGE_PART_IS_NOT_VALID)));
        }
        
        // Validate that the part refers to a message existing
        return true;
    }
    
    /**
     * Visits a portType element.
     * @param portType a portType element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(PortType portType) {
        getValidateSupport().assertRequiredAttrib(portType.getName(), PortType.ATTR.NAME,
                                                       portType, ToDoEvent.Category.WSDL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(portType.getName(), PortType.ATTR.NAME,
                                                     portType, ToDoEvent.Category.WSDL_SYNTAX);
        
        
        //wsdl 1.1 spec validation:
        //2.4.5 Names of Elements within an Operation:
        //The name attribute of the input and output elements provides a unique name 
        //among all input and output elements within the enclosing port type

        //(a)validate if all operation input name are unique
        //(b)validate if all operation output name are unique
        ArrayList inputNames = new ArrayList();
        ArrayList outputNames = new ArrayList();
        Collection operations = portType.getOperations();
        Iterator it = operations.iterator();
        while(it.hasNext()) {
        	Operation operation = (Operation) it.next();
        	OperationInput input = operation.getInput();
        	if(input != null) {
        		String inputName = input.getName();
        		if(inputName != null) {
	        		if(!inputNames.contains(inputName)) {
	        			inputNames.add(inputName);
	        		} else {
	        			//found duplicate input name in this portType operations
	        			getValidateSupport().fireToDo
	                    (new ToDoEvent
	                     (input, ToDoEvent.Category.WSDL_SEMANTICS, ToDoEvent.Severity.ERROR,
	                      mMsg.getString(VAL_DUPLICATE_OPRATION_INPUT_NAME_IN_PORTTYPE, inputName, portType.getName()),
	                      mMsg.getString(FIX_DUPLICATE_OPRATION_INPUT_NAME_IN_PORTTYPE)));
	        		}
        		}
        	}
        	
        	OperationOutput output = operation.getOutput();
        	if(output != null) {
        		String outputName = output.getName();
        		if(outputName != null) {
	        		if(!outputNames.contains(outputName)) {
	        			outputNames.add(outputName);
	        		} else {
	        			//found duplicate output name in this portType operations 
	        			getValidateSupport().fireToDo
	                    (new ToDoEvent
	                     (output, ToDoEvent.Category.WSDL_SEMANTICS, ToDoEvent.Severity.ERROR,
	                      mMsg.getString(VAL_DUPLICATE_OPRATION_OUTPUT_NAME_IN_PORTTYPE, outputName, portType.getName()),
	                      mMsg.getString(FIX_DUPLICATE_OPRATION_OUTPUT_NAME_IN_PORTTYPE)));
	        		}
        		}
        	}
        }
        

        return true;
    }
    
    /**
     * Visits a portType operation element.
     * @param operation a portType operation element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Operation operation) {
        getValidateSupport().assertRequiredAttrib(operation.getName(), Operation.ATTR.NAME,
                                                       operation, ToDoEvent.Category.WSDL_SYNTAX);
        
        getValidateSupport().assertNCNameAttrib(operation.getName(), Operation.ATTR.NAME,
                                                     operation, ToDoEvent.Category.WSDL_SYNTAX);
        
        

        // if the opertion is an one-way operation or a notification operation,
        // then there should be no faults
        if ((operation.getInput() == null || operation.getOutput() == null) 
            && operation.countFaults() > 0) {
            getValidateSupport().fireToDo
                (new ToDoEvent
                 (operation, ToDoEvent.Category.WSDL_SEMANTICS, ToDoEvent.Severity.WARNING,
                  mMsg.getString(VAL_FAULT_NOT_ALLOWED_IN_OPERATION, operation.getName()),
                  mMsg.getString(FIX_FAULT_NOT_ALLOWED_IN_OPERATION, operation.getName())));
        }
          
        //wsdl spec:
        //2.4.5 Names of Elements within an Operation:
        //The name of the fault element is unique within the set of faults defined for the operation
        ArrayList faultNames = new ArrayList();
        
        Collection faults = operation.getFaults();
        Iterator it = faults.iterator();
        while(it.hasNext()) {
        	OperationFault fault = (OperationFault) it.next();
        	String faultName = fault.getName();
        	if(faultName != null) {
        		if(!faultNames.contains(faultName)) {
        			faultNames.add(faultName);
        		} else {
        			//found duplicate output name in this portType operations
        			getValidateSupport().fireToDo
                    (new ToDoEvent
                     (operation, ToDoEvent.Category.WSDL_SEMANTICS, ToDoEvent.Severity.ERROR,
                      mMsg.getString(VAL_DUPLICATE_OPRATION_FAULT_NAME, faultName, operation.getName()),
                      mMsg.getString(FIX_DUPLICATE_OPRATION_FAULT_NAME)));
        		}
        	}
        }
        
        return true;
    }

    /**
     * Visits an operation input element.
     * @param input an operation input element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(OperationInput input) {
        getValidateSupport().assertRequiredAttrib(input.getMessage(), OperationInput.ATTR.MESSAGE,
                                                       input, ToDoEvent.Category.WSDL_SYNTAX);
        
        getValidateSupport().assertQNameAttrib(input.getMessage(), OperationInput.ATTR.MESSAGE,
                                                     input, ToDoEvent.Category.WSDL_SYNTAX);
        
        WSDLMessage message= input.getWSDLMessage();
        if (message == null) {
            // throw validation error
            Operation operation = (Operation) input.getParent();
            getValidateSupport().fireToDo
                (new ToDoEvent
                 (input, ToDoEvent.Category.WSDL_SEMANTICS, ToDoEvent.Severity.ERROR,
                  mMsg.getString(VAL_MESSAGE_NOT_FOUND_IN_OPERATION_INPUT, 
                                 input.getMessage(), operation.getName()),
                  mMsg.getString(FIX_MESSAGE_NOT_FOUND_IN_OPERATION_INPUT, 
                                 input.getMessage(), operation.getName())));

        }

        return true;
    }
    
    /**
     * Visits an operation output element.
     * @param output an operation output element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(OperationOutput output) {
        getValidateSupport().assertRequiredAttrib(output.getMessage(), OperationOutput.ATTR.MESSAGE,
                                                       output, ToDoEvent.Category.WSDL_SYNTAX);
        
        getValidateSupport().assertQNameAttrib(output.getMessage(), OperationOutput.ATTR.MESSAGE,
                                                     output, ToDoEvent.Category.WSDL_SYNTAX);
        

        WSDLMessage message = output.getWSDLMessage();
        if (message == null) {
            // throw validation error
            Operation operation = (Operation) output.getParent();
            getValidateSupport().fireToDo
                (new ToDoEvent
                 (output, ToDoEvent.Category.WSDL_SEMANTICS, ToDoEvent.Severity.ERROR,
                  mMsg.getString(VAL_MESSAGE_NOT_FOUND_IN_OPERATION_OUTPUT, 
                                 output.getMessage(), operation.getName()),
                  mMsg.getString(FIX_MESSAGE_NOT_FOUND_IN_OPERATION_OUTPUT, 
                                 output.getMessage(), operation.getName())));

        }

        return true;
    }
    
    /**
     * Visits an operation fault element.
     * @param fault an operation fault element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(OperationFault fault) {
        getValidateSupport().assertRequiredAttrib(fault.getName(), OperationFault.ATTR.NAME,
                                                       fault, ToDoEvent.Category.WSDL_SYNTAX);
        
        
        getValidateSupport().assertRequiredAttrib(fault.getMessage(), OperationFault.ATTR.MESSAGE,
                                                       fault, ToDoEvent.Category.WSDL_SYNTAX);
        
        getValidateSupport().assertQNameAttrib(fault.getMessage(), OperationFault.ATTR.MESSAGE,
                                                     fault, ToDoEvent.Category.WSDL_SYNTAX);
        

        WSDLMessage message= fault.getWSDLMessage();
        if (message == null) {
            // throw validation error
            Operation operation = (Operation) fault.getParent();
            getValidateSupport().fireToDo
                (new ToDoEvent
                 (fault, ToDoEvent.Category.WSDL_SEMANTICS, ToDoEvent.Severity.ERROR,
                  mMsg.getString(VAL_MESSAGE_NOT_FOUND_IN_OPERATION_FAULT, 
                                 fault.getMessage(), operation.getName()),
                  mMsg.getString(FIX_MESSAGE_NOT_FOUND_IN_OPERATION_FAULT, 
                                 fault.getMessage(), operation.getName())));

        }

        return true;
    }
    
    /**
     * Visits a service element.
     * @param service a service element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Service service) {
        return true;
    }
    
    /**
     * Visits a service port element.
     * @param port a service port element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(ServicePort port) {
    	getValidateSupport().assertNCNameAttrib(port.getName(), 
    			ServicePort.ATTR.NAME, 
				port, 
				ToDoEvent.Category.WSDL_SYNTAX);


		getValidateSupport().assertRequiredAttrib(port.getName(), 
						  ServicePort.ATTR.NAME, 
						  port, 
						  ToDoEvent.Category.WSDL_SYNTAX);
		
		getValidateSupport().assertRequiredAttrib(port.getBinding(), 
						  ServicePort.ATTR.BINDING, 
						  port, 
						  ToDoEvent.Category.WSDL_SYNTAX);

		//verify if binding exists
    	Binding binding = port.getWSDLBinding();
    	if(binding == null) {
    		getValidateSupport().fireToDo
            (new ToDoEvent
             (port, 
              ToDoEvent.Category.WSDL_SEMANTICS, 
              ToDoEvent.Severity.ERROR,
              mMsg.getString(VAL_MISSING_BINDING_IN_SERVICE_PORT, 
            		  		port.getName(), port.getBinding()),
              mMsg.getString(FIX_MISSING_BINDING_IN_SERVICE_PORT)
              ));
    	}

        return true;
    }
    
    /**
     * Visits a service link type element.
     * @param type a service link type element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(PartnerLinkType type) {
        return true;
    }
    
    /**
     * Visits a service link role element.
     * @param role a service link role element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(PartnerLinkRole role) {
    	getValidateSupport().assertRequiredAttrib(role.getName(), PartnerLinkRole.ATTR.NAME,
    			role, ToDoEvent.Category.WSDL_SYNTAX);
    	
    	getValidateSupport().assertRequiredAttrib(role.getPortType(), PartnerLinkRole.ATTR.PORTTYPE,
    			role, ToDoEvent.Category.WSDL_SYNTAX);
    	
    	if(role.getWSDLPortType() == null) {
    		getValidateSupport().fireToDo
            (new ToDoEvent
             (role, ToDoEvent.Category.WSDL_SEMANTICS, ToDoEvent.Severity.ERROR,
              mMsg.getString(VAL_NO_PARTNERLINKTYPE_PORTTYPE_DEFINED_IN_WSDL, 
            		  role.getPortType()),
              mMsg.getString(FIX_NO_PARTNERLINKTYPE_PORTTYPE_DEFINED_IN_WSDL)));

    	}
    	return true;
    }
    
    /**
     * Visits an import element.
     * @param wsdlImport an import element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Import wsdlImport) {
    	
		getValidateSupport().assertRequiredAttrib(wsdlImport.getNamespaceAttr(), 
						   Import.ATTR.NAMESPACE, 
						  wsdlImport, 
						  ToDoEvent.Category.WSDL_SYNTAX);
		
		getValidateSupport().assertRequiredAttrib(wsdlImport.getLocation(), 
						  Import.ATTR.LOCATION, 
						  wsdlImport, 
						  ToDoEvent.Category.WSDL_SYNTAX);
		
		//verify if imported document is available
		Object importedDocument = wsdlImport.getImportedObject();
		
		if(importedDocument == null) {
    		getValidateSupport().fireToDo
            (new ToDoEvent
             (wsdlImport, 
              ToDoEvent.Category.WSDL_SEMANTICS, 
              ToDoEvent.Severity.ERROR,
              mMsg.getString(VAL_MISSING_IMPORTED_DOCUMENT, 
            		  		 wsdlImport.getNamespaceAttr(), 
            		  		 wsdlImport.getLocation()),
              mMsg.getString(FIX_MISSING_IMPORTED_DOCUMENT)
              ));
    	}


        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(Binding)
     */
    public boolean visit(Binding binding) {
    	getValidateSupport().assertNCNameAttrib(binding.getName(), 
												Binding.ATTR.NAME, 
												binding, 
												ToDoEvent.Category.WSDL_SYNTAX);

    	
    	getValidateSupport().assertRequiredAttrib(binding.getName(), 
												  Binding.ATTR.NAME, 
												  binding, 
												  ToDoEvent.Category.WSDL_SYNTAX);
    	
    	getValidateSupport().assertRequiredAttrib(binding.getType(), 
    											  Binding.ATTR.TYPE, 
    											  binding, 
    											  ToDoEvent.Category.WSDL_SYNTAX);
    	
    	
    	
    	//now make sure portType is available
    	PortType portType = binding.getWSDLPortType();
    	if(portType == null) {
    		getValidateSupport().fireToDo
            (new ToDoEvent
             (binding, 
              ToDoEvent.Category.WSDL_SEMANTICS, 
              ToDoEvent.Severity.ERROR,
              mMsg.getString(VAL_MISSING_PORTTYPE_IN_BINDING, 
            		  		binding.getName(), 
            		  		binding.getType()),
              mMsg.getString(FIX_MISSING_PORTTYPE_IN_BINDING)
              ));
    	}

        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPBinding)
     */
    public boolean visit(SOAPBinding sBinding) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingOperation)
     */
    public boolean visit(BindingOperation bindingOp) {
    	getValidateSupport().assertNCNameAttrib(bindingOp.getName(), 
    			BindingOperation.ATTR.NAME, 
    			bindingOp, 
				ToDoEvent.Category.WSDL_SYNTAX);


		getValidateSupport().assertRequiredAttrib(bindingOp.getName(), 
						  BindingOperation.ATTR.NAME, 
						  bindingOp, 
						  ToDoEvent.Category.WSDL_SYNTAX);

    	//wsdl spec:
    	//section 2.5 Bindings:
    	//An operation element within a binding specifies binding information for the 
    	//operation with the same name within the binding's portType
    	
    	//validate if binding operation name is same as porttype operation name
    	XMLNode parent = bindingOp.getParent();
    	if(parent instanceof Binding) {
    		Binding binding = (Binding) parent;
    		PortType portType = binding.getWSDLPortType();
    		String operationName = bindingOp.getName();
    		if(portType != null && operationName != null) {
    			Collection operations = portType.getOperations(operationName);
    			//if no matching operation is found then it is an error
    			if(operations == null || operations.size() == 0) {
    				getValidateSupport().fireToDo
                    (new ToDoEvent
                     (bindingOp, ToDoEvent.Category.WSDL_SEMANTICS, ToDoEvent.Severity.ERROR,
                      mMsg.getString(VAL_OPERATION_DOES_NOT_EXIST_IN_PORT_TYPE, operationName, binding.getName(), portType.getName()),
                      mMsg.getString(FIX_OPERATION_DOES_NOT_EXIST_IN_PORT_TYPE, portType.getName())));
    			}
    		} else {
    			//binding operation's parent should be binding
    			//taken care by schema validation
    		}
    	}
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPOperation)
     */
    public boolean visit(SOAPOperation sOperation) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingInput)
     */
    public boolean visit(BindingInput bindingIn) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingOutput)
     */
    public boolean visit(BindingOutput bindingOut) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingFault)
     */
    public boolean visit(BindingFault bindingFault) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPBody)
     */
    public boolean visit(SOAPBody sBody) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPHeader)
     */
    public boolean visit(SOAPHeader sHeader) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPHeaderFault)
     */
    public boolean visit(SOAPHeaderFault sHeaderFault) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPAddress)
     */
    public boolean visit(SOAPAddress sAddress) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPFault)
     */
    public boolean visit(SOAPFault sFault) {
        return true;
    }
    
    /**
     * @see DocumentationVisitor#visit(Documentation)
     */
    public boolean visit(Documentation doc) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(Types)
     */
    public boolean visit(Types types) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(ExtensibilityElement)
     */
    public boolean visit(ExtensibilityElement ext) {
        return true;
    }
    
    /** Visits a property element.
     * @param   p   a property element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Property p) {
        return true;
    }
    
    /** Visits a propertyAlias element.
     * @param   p   a propertyAlias element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(PropertyAlias p) {
        return true;
    }
    
    /** Visits a Query element.
     * @param   q   a Query element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Query q) {
    	return true;
    }
}
