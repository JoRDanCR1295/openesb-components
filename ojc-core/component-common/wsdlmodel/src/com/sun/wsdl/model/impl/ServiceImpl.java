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
 * @(#)ServiceImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;
 
 

import com.sun.wsdl.model.Service;
import com.sun.wsdl.model.ServicePort;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.visitor.WSDLVisitor;


import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.util.logging.Logger;
 

 
/**
 * Implements the &lt;service&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class ServiceImpl
    extends NamedWSDLExtensibleElementImpl
    implements Service {
        
    /**
     * The logger.
     */
    private static Logger mLogger =
        Logger.getLogger(ServiceImpl.class.getName());
        
    /**
     * List of ports.
     */
    private List mPorts;
    
    /**
     * Creates a new instance of ServiceImpl.
     * @param   d   Owner document.
     */
    public ServiceImpl(XMLDocument d) {
        super(d);
        mPorts = new ArrayList();
        initService();
    }
    
    /**
     * Initializes this class.
     */
    private void initService() {
        setLocalName(Service.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(Service.ATTR.NAME, String.class, false, null)
        };
        childrenTags = new String[] {
            ServicePort.TAG
        };
    }
    
    /**
	 * add a node at the end of a collection.
	 * This will be called while parsing this XMLNode.
	 * This method should always add this child at the end of 
	 * the collection because we want to preserve the order
	 * in which this node occured in the xml.
	 * This method should also set the sibliing sequence order
	 * before adding to collection if it different than the
	 * default Interger.MAX_VALUE
	 * @param c
	 */
	public void addChildAtTail(XMLNode c) {
		if (c instanceof ServicePort) {
			addPortAtTail((ServicePort) c);
        } else {
            super.addChildAtTail(c);
        }
	}
	
    /**
     * Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        if (c instanceof ServicePort) {
            addPort((ServicePort) c);
        } else {
            super.addChild(c);
        }
    }
    
    /**
     * Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
        if (c instanceof ServicePort) {
            removePort((ServicePort) c);
        } else {
            super.removeChild(c);
        }
    }
   
    
    /**
     * Adds a new port.
     * @param port the port to add
     */
    public void addPort(ServicePort port) {
    	mPorts.add(port);
    	super.addChild(2, port);
    }
    
    public void addPortAtTail(ServicePort port) {
    	mPorts.add(port);
    	super.addChildAtTheEnd(port, 2);
    }
    
    
    /**
     * Removes the given port.
     * @param port to be removed
     */
    public void removePort(ServicePort port) {
    	mPorts.remove(port);
    	super.removeChild(port);
    }
    

    /**
     * Gets the list of all ports.
     * @return a read-only collection of ServicePorts.
     */
    public Collection getPorts() {
        return Collections.unmodifiableCollection(mPorts);
    }
    
    /**
     * Accepts a visitor to perform some work on the element.
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
