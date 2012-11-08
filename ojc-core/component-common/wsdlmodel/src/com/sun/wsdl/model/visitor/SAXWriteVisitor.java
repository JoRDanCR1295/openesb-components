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

package com.sun.wsdl.model.visitor;

import java.io.Writer;
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
import com.sun.wsdl.model.common.model.Documentation;
import com.sun.wsdl.model.common.model.XMLComment;
import com.sun.wsdl.model.common.model.XMLProcessingInstruction;
import com.sun.wsdl.model.common.visitor.DocumentationVisitor;
import com.sun.wsdl.model.common.visitor.ParentChildrenParentVisitor;
import com.sun.wsdl.model.common.visitor.SAXWriterSupport;
import com.sun.wsdl.model.common.visitor.XMLWriteVisitorException;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.extensions.soap.SOAPAddress;
import com.sun.wsdl.model.extensions.soap.SOAPBinding;
import com.sun.wsdl.model.extensions.soap.SOAPBody;
import com.sun.wsdl.model.extensions.soap.SOAPFault;
import com.sun.wsdl.model.extensions.soap.SOAPHeader;
import com.sun.wsdl.model.extensions.soap.SOAPHeaderFault;
import com.sun.wsdl.model.extensions.soap.SOAPOperation;

/**
 * Visits the model nodes and marshals them out to a XML stream.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SAXWriteVisitor
    extends AbstractVisitor
    implements ParentChildrenParentVisitor {
    
    /** The logger. */
    private static Logger mLogger = Logger.getLogger(SAXWriteVisitor.class.getName());
    
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
    
    /** Visits a comment section.
     * @param   c   XML comment section
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(XMLComment c) {
        try {
            getWriterSupport().getXmlWriter().comment(c.getValue());
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException(
                "Cannot serialize XML Comment", trw);
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
    
    /**
     * Visits a WSDL document.
     * @param d the document to visit
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(WSDLDocument d) {
        try {
            if (getWriterSupport().isElementStart(d)) {
                getWriterSupport().getXmlWriter().startDocument();
            } else {
                getWriterSupport().getXmlWriter().endDocument();
            }
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException(
                "Cannot serialize WSDL", trw);
        }
        return true;
    }
    
    /**
     * Visits a WSDL definitions.
     * @param w a WSDL definitions element
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(WSDLDefinitions w) {
        getWriterSupport().writeElement(w);
        return true;
    }
    
    /** Visits a wsdl:message element.
     * @param   w   a wsdl:message element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(WSDLMessage w) {
        getWriterSupport().writeElement(w);
        return true;
    }
    
    /** Visits a part element.
     * @param   p   a part element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Part p) {
        getWriterSupport().writeElement(p);
        return true;
   }
    
    /**
     * Visits a portType element.
     * @param portType a portType element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(PortType portType) {
        getWriterSupport().writeElement(portType);
        return true;
    }
    
    /**
     * Visits a portType operation element.
     * @param operation a portType operation element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Operation operation) {
        getWriterSupport().writeElement(operation);
        return true;
    }
    
    /**
     * Visits an operation input element.
     * @param input an operation input element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(OperationInput input) {
        getWriterSupport().writeElement(input);
        return true;
    }
    
    /**
     * Visits an operation output element.
     * @param output an operation output element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(OperationOutput output) {
        getWriterSupport().writeElement(output);
        return true;
    }
    
    /**
     * Visits an operation fault element.
     * @param fault an operation fault element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(OperationFault fault) {
        getWriterSupport().writeElement(fault);
        return true;
    }
    
    /**
     * Visits a service element.
     * @param service a service element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Service service) {
        getWriterSupport().writeElement(service);
        return true;
    }
    
    /**
     * Visits a service port element.
     * @param port a service port element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(ServicePort port) {
        getWriterSupport().writeElement(port);
        return true;
    }
    
    /**
     * Visits a service link type element.
     * @param type a service link type element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(PartnerLinkType type) {
        getWriterSupport().writeElement(type);
        return true;
    }
    
    /**
     * Visits a service link role element.
     * @param role a service link role element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(PartnerLinkRole role) {
        getWriterSupport().writeElement(role);
        return true;
    }
    
    /**
     * Visits an import element.
     * @param wsdlImport an import element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Import wsdlImport) {
        getWriterSupport().writeElement(wsdlImport);
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(Binding)
     */
    public boolean visit(Binding binding) {
        getWriterSupport().writeElement(binding);
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPBinding)
     */
    public boolean visit(SOAPBinding sBinding) {
        getWriterSupport().writeEmptyElement(sBinding);
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingOperation)
     */
    public boolean visit(BindingOperation bindingOp) {
        getWriterSupport().writeElement(bindingOp);
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPOperation)
     */
    public boolean visit(SOAPOperation sOperation) {
        getWriterSupport().writeEmptyElement(sOperation);
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingInput)
     */
    public boolean visit(BindingInput bindingIn) {
        getWriterSupport().writeElement(bindingIn);
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingOutput)
     */
    public boolean visit(BindingOutput bindingOut) {
        getWriterSupport().writeElement(bindingOut);
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingFault)
     */
    public boolean visit(BindingFault bindingFault) {
        getWriterSupport().writeElement(bindingFault);
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPBody)
     */
    public boolean visit(SOAPBody sBody) {
        getWriterSupport().writeEmptyElement(sBody);
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPHeader)
     */
    public boolean visit(SOAPHeader sHeader) {
        getWriterSupport().writeElement(sHeader);
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPHeaderFault)
     */
    public boolean visit(SOAPHeaderFault sHeaderFault) {
        getWriterSupport().writeEmptyElement(sHeaderFault);
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPAddress)
     */
    public boolean visit(SOAPAddress sAddress) {
        getWriterSupport().writeEmptyElement(sAddress);
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPFault)
     */
    public boolean visit(SOAPFault sFault) {
        getWriterSupport().writeEmptyElement(sFault);
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
     * @see WSDLVisitor#visit(Types)
     */
    public boolean visit(Types types) {
        getWriterSupport().writeElement(types);
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(ExtensibilityElement)
     */
    public boolean visit(ExtensibilityElement ext) {
    	 try {
    	 	getWriterSupport().writeElement(ext);
            if (getWriterSupport().isCurrentElement(ext) && ext.getValue() != null) {
                char[] chs = ext.getValue().toCharArray();
                if (ext.isCDATAForm()) {
                    getWriterSupport().getXmlWriter().cdataSection(chs, 0, chs.length);
                } else {
                    getWriterSupport().getXmlWriter().characters(chs, 0, chs.length);
                }
            }
            
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException("Cannot serialize documentation element", trw);
        }
    	
        return true;
    }
    
    /** Visits a property element.
     * @param   p   a property element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Property p) {
        getWriterSupport().writeEmptyElement(p);
        return true;
    }
    
    /** Visits a propertyAlias element.
     * @param   p   a propertyAlias element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(PropertyAlias p) {
        getWriterSupport().writeElement(p);
        return true;
    }
    
    /** Visits a Query element.
     * @param   q  a Query element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Query q) {
    	try {
    	 	getWriterSupport().writeElement(q);
            if (getWriterSupport().isCurrentElement(q) && q.getValue() != null) {
                char[] chs = q.getValue().toCharArray();
                if (q.isCDATAForm()) {
                    getWriterSupport().getXmlWriter().cdataSection(chs, 0, chs.length);
                } else {
                    getWriterSupport().getXmlWriter().characters(chs, 0, chs.length);
                }
            }
            
            //also write end tag see accept method in QueryImpl
            getWriterSupport().writeElement(q);
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException("Cannot serialize for element", trw);
        }
        return true;
    }
   
}
