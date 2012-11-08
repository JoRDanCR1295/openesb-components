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
 * @(#)PartnerLinkRoleImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;
 
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

import com.sun.wsdl.model.NamespaceUtility;
import com.sun.wsdl.model.PortType;
import com.sun.wsdl.model.PartnerLinkRole;
import com.sun.wsdl.model.WSDLDocument;
import javax.xml.namespace.QName;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.extensions.impl.ExtensibilityElementImpl;
import com.sun.wsdl.model.visitor.WSDLVisitor;
 

 
/**
 * Implements the SLT &lt;role&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class PartnerLinkRoleImpl
    extends ExtensibilityElementImpl
    implements PartnerLinkRole {
        
    /**
     * The logger.
     */
    private static Logger mLogger =
        Logger.getLogger(PartnerLinkRoleImpl.class.getName());
        
    /**
     * List of portType.
     */
    private List mPortTypes;
    
    /**
     * Creates a new instance of PartnerLinkRoleImpl.
     * @param   d   Owner document.
     */
    public PartnerLinkRoleImpl(XMLDocument d) {
        super(d);
        mPortTypes = new ArrayList();
        initServiceLinkRole();
    }
    
    /**
     * Initializes this class.
     */
    private void initServiceLinkRole() {
        owningNamespace = WSDLDocument.WSDL_SLNK_NAMESPACE;
        owningNamespacePrefix = WSDLDocument.WSDL_SLNK_PREFIX;
        
        setLocalName(PartnerLinkRole.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.NAME, String.class, false, null),
            new XMLAttributeImpl(ATTR.PORTTYPE, String.class, false, null)
        };
        
    }
    
    /**
     * Getter for property name.
     * @return Value of property name.
     */
    public String getName() {
        return xmlAttrs[NAME].getValue();
    }
    
    /**
     * Setter for property name.
     * @param name   New value of property name.
     */
    public void setName(String name) {
        setAttribute(NAME, name);
    }
    
    
    public QName getPortType() {
    	String portTypeQName = xmlAttrs[PORTTYPE].getValue();
    	if(portTypeQName != null) {
    		return NamespaceUtility.resolveAndGetQName(portTypeQName, this);
    	}
		return null;
	}

	public void setPortType(QName portType) {
		if(portType != null) {
			/*setAttribute(NAME, portType.toString());*/
    		setAttribute(NAME, 
    				NamespaceUtility.getQNameAsString(portType));
			
		} else {
			setAttribute(NAME, null);
		}
	}

	/**
     * Get the PortType this SeriviceLinkPortType refers to
     * @return PortType
     */
    public PortType getWSDLPortType() {
    	PortType portType = null;
    	WSDLDocument doc = (WSDLDocument) this.getOwnerDocument();
    	if(doc != null && this.getPortType() != null) {
    		portType = doc.getDocumentDefinitions().getPortType(this.getPortType());
    	}
    	return portType;
    }


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
