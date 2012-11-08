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
 * @(#)OperationFaultImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;
 
import java.util.logging.Logger;

import com.sun.wsdl.model.NamespaceUtility;
import com.sun.wsdl.model.OperationFault;
import com.sun.wsdl.model.WSDLHelper;
import com.sun.wsdl.model.WSDLMessage;
import javax.xml.namespace.QName;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.visitor.WSDLVisitor;
 

 
/**
 * Implements the portType operation &lt;false&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class OperationFaultImpl
    extends NamedWSDLElementImpl
    implements OperationFault {
        
    /**
     * The logger.
     */
    private static Logger mLogger =
        Logger.getLogger(OperationFaultImpl.class.getName());
    
    
    /**
     * Creates a new instance of OperationFaultImpl.
     * @param   d   Owner document.
     */
    public OperationFaultImpl(XMLDocument d) {
        super(d);
        
        initOperationFault();
    }
    
    /**
     * Initializes this class.
     */
    private void initOperationFault() {
        setLocalName(OperationFault.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(OperationFault.ATTR.NAME, String.class, false, null),
            new XMLAttributeImpl(OperationFault.ATTR.MESSAGE, String.class, false, null)
        };
        childrenTags = new String[] {
        };
    }
    
    
    /**
     * Getter for property message.
     * @return Value of property message.
     */
    public QName getMessage() {
        String messageQNameStr = xmlAttrs[MESSAGE].getValue();
        if(messageQNameStr != null) {
        	return NamespaceUtility.resolveAndGetQName(messageQNameStr, this);
        }
        
        return null;
    }
    
    /**
     * Setter for property message.
     * @param message   New value of property message.
     */
    public void setMessage(String message) {
        setAttribute(MESSAGE, message);
    }
    
    
    
    public WSDLMessage getWSDLMessage() {
		return WSDLHelper.getMatchingWSDLMessage(getMessage(), this);
	}

	/** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor w) {
    	WSDLVisitor v = morphVisitor(w);
        if (traverseParentFirst(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        
        if (!super.accept(v)) {
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
