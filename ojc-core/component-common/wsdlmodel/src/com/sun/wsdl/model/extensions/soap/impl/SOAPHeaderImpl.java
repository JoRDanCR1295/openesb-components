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
 * @(#)SOAPHeaderImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.extensions.soap.impl;

import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.extensions.impl.ExtensibilityElementImpl;
import com.sun.wsdl.model.extensions.soap.SOAPHeader;
import com.sun.wsdl.model.extensions.soap.SOAPHeaderFault;
import com.sun.wsdl.model.visitor.WSDLVisitor;

import java.util.ArrayList;

/**
 * Implements the SOAP header extensibility element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SOAPHeaderImpl extends ExtensibilityElementImpl implements SOAPHeader {
    
    /** Holds list of SOAP header faults */
    protected ArrayList headerFaults = new ArrayList();
    
    /** Creates a new instance of SOAPHeaderImpl */
    public SOAPHeaderImpl() {
        super();
        initSOAPHeader();
    }
    
    /** Constructor for new SOAP header instance.
     * @param   d   Owner document.
     */
    public SOAPHeaderImpl(XMLDocument d) {
        super(d);
        initSOAPHeader();
    }
    
    /** Initializes this class.
     */
    private void initSOAPHeader() {
        setQualifiedName(SOAPHeader.QTAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(SOAPHeader.ATTR.MESSAGE, String.class, true, null),
            new XMLAttributeImpl(SOAPHeader.ATTR.PART, String.class,  true, null),
            new XMLAttributeImpl(SOAPHeader.ATTR.USE, String.class, true, new String[] {"literal", "encoded"}),
            new XMLAttributeImpl(SOAPHeader.ATTR.ENCODING_STYLE, String.class, false, null),
            new XMLAttributeImpl(SOAPHeader.ATTR.NAMESPACE, String.class, false, null)
        };
        childrenTags = new String[] {SOAPHeaderFault.QTAG};
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#getMessage()
     */
    public String getMessage() {
        return xmlAttrs[MESSAGE].getValue();
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#setMessage(java.lang.String)
     */
    public void setMessage(String message) {
        setAttribute(MESSAGE, message);
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#setMessage(java.lang.String,
     * java.lang.String)
     */
    public void setMessage(String qName, String message) {
        setAttribute(MESSAGE, qName, message);
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#getPart()
     */
    public String getPart() {
        return xmlAttrs[PART].getValue();
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#setPart(java.lang.String)
     */
    public void setPart(String parts) {
        setAttribute(PART, parts);
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#setPart(java.lang.String,
     *  java.lang.String)
     */
    public void setPart(String qName, String part) {
        setAttribute(PART, qName, part);
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#getUse()
     */
    public String getUse() {
        return xmlAttrs[USE].getValue();
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#setUse(java.lang.String)
     */
    public void setUse(String use) {
        setAttribute(USE, use);
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#setUse(java.lang.String,
     *  java.lang.String)
     */
    public void setUse(String qName, String use) {
        setAttribute(USE, qName, use);
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#getEncodingStyle()
     */
    public String getEncodingStyle() {
        return xmlAttrs[ENCODING_STYLE].getValue();
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#setEncodingStyle(
     *  java.lang.String)
     */
    public void setEncodingStyle(String style) {
        setAttribute(ENCODING_STYLE, style);
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#setEncodingStyle(java.lang.String,
     *  java.lang.String)
     */
    public void setEncodingStyle(String qName, String style) {
        setAttribute(ENCODING_STYLE, qName, style);
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#getNamespaceURI()
     */
    public String getNamespaceURI() {
        return xmlAttrs[NAMESPACE].getValue();
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#setNamespaceURI(java.lang.String)
     */
    public void setNamespaceURI(String namespace) {
        setAttribute(NAMESPACE, namespace);
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#setNamespaceURI(java.lang.String,
     *  java.lang.String)
     */
    public void setNamespaceURI(String qName, String namespace) {
        setAttribute(NAMESPACE, qName, namespace);
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#addChild(com.sun.wsdl.model.common.model.XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof SOAPHeaderFault) {
            addSOAPHeaderFault((SOAPHeaderFault) c);
        } else {
            super.addChild(c);
        }
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#removeChild(com.sun.wsdl.model.common.model.XMLNode)
     */
    public void removeChild(XMLNode c) {
        if (c instanceof SOAPHeaderFault) {
            removeSOAPHeaderFault((SOAPHeaderFault) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#getSOAPHeaderFault(int)
     */
    public SOAPHeaderFault getSOAPHeaderFault(int index) {
        return (SOAPHeaderFault) headerFaults.get(index);
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#setSOAPHeaderFault(int,
     *  com.sun.wsdl.model.extensions.soap.SOAPHeaderFault)
     */
    public void setSOAPHeaderFault(int index, SOAPHeaderFault fault) {
        if (headerFaults.size() == index) {
            addSOAPHeaderFault(fault);
        } else {
        	SOAPHeaderFault oldHeaderFault = getSOAPHeaderFault(index);
        	headerFaults.set(index, fault);
        	replaceChild(oldHeaderFault, fault);
            
        }
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#getSOAPHeaderFaultSize()
     */
    public int getSOAPHeaderFaultSize() {
        return headerFaults.size();
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#addSOAPHeaderFault(
     *  com.sun.wsdl.model.extensions.soap.SOAPHeaderFault)
     */
    public void addSOAPHeaderFault(SOAPHeaderFault fault) {
    	headerFaults.add(fault);
    	super.addChild(fault);
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#removeSOAPHeaderFault(int)
     */
    public void removeSOAPHeaderFault(int i) {
    	SOAPHeaderFault oldHeaderFault = getSOAPHeaderFault(i);
    	headerFaults.remove(i);
    	super.removeChild(oldHeaderFault);
        
    }
    
    /**
     * @see com.sun.wsdl.model.extensions.soap.SOAPHeader#removeSOAPHeaderFault(
     *  com.sun.wsdl.model.extensions.soap.SOAPHeaderFault)
     */
    public boolean removeSOAPHeaderFault(SOAPHeaderFault fault) {
    	boolean result = headerFaults.remove(fault);
        super.removeChild(fault);
        return result;
    }
    
    /** @see com.sun.wsdl.model.extensions.soap.SOAPHeader#indexOfSOAPHeaderFault(
     *  com.sun.wsdl.model.extensions.soap.SOAPHeaderFault)
     */
    public int indexOfSOAPHeaderFault(XMLNode fault) {
        return headerFaults.indexOf(fault);
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#accept(com.sun.wsdl.model.common.model.visitor.Visitor)
     */
    public boolean accept(Visitor w) {
        WSDLVisitor v = morphVisitor(w);
        
        if (traverseParentFirst(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }

        if (!superAccept(v)) {
            return false;
        }

        if (traverseParentLast(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        return true;
    }
}
