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
 * @(#)PartnerLinkTypeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;
 
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import com.sun.wsdl.model.PartnerLinkRole;
import com.sun.wsdl.model.PartnerLinkType;
import com.sun.wsdl.model.WSDLDocument;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.extensions.impl.ExtensibilityElementImpl;
import com.sun.wsdl.model.visitor.WSDLVisitor;
 

 
/**
 * Implements the SLT &lt;serviceLinkType&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class PartnerLinkTypeImpl
    extends ExtensibilityElementImpl
    implements PartnerLinkType {
        
    /**
     * The logger.
     */
    private static Logger mLogger =
        Logger.getLogger(PartnerLinkTypeImpl.class.getName());
        
    /**
     * List of roles.
     */
    private List mRoles;
    
    /**
     * Creates a new instance of PartnerLinkTypeImpl.
     * @param   d   Owner document.
     */
    public PartnerLinkTypeImpl(XMLDocument d) {
        super(d);
        mRoles = new ArrayList();
        initServiceLinkType();
    }
    
    /**
     * Initializes this class.
     */
    private void initServiceLinkType() {
        owningNamespace = WSDLDocument.WSDL_SLNK_NAMESPACE;
        owningNamespacePrefix = WSDLDocument.WSDL_SLNK_PREFIX;
        
        setLocalName(PartnerLinkType.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.NAME, String.class, false, null)
        };
        childrenTags = new String[] {
            PartnerLinkRole.TAG
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
    
    /**
     * Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        if (c instanceof PartnerLinkRole) {
            addRole((PartnerLinkRole) c);
        } else {
            super.addChild(c);
        }
    }
    
    /**
     * Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
        if (c instanceof PartnerLinkRole) {
            removeRole((PartnerLinkRole) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /**
     * Getter for property role.
     * @param index index of the property to get
     * @return Value of property role.
     *
     */
    public PartnerLinkRole getRole(int index) {
        return (PartnerLinkRole) mRoles.get(index);
    }
    
    
    public PartnerLinkRole getRole(String roleName) {
    	if(roleName == null) {
    		return null;
    	}
    	
		PartnerLinkRole sLinkRole = null;
    	Iterator it = mRoles.iterator();
		
    	while(it.hasNext()) {
			PartnerLinkRole role = (PartnerLinkRole) it.next();
			if(roleName.equals(role.getName())) {
				sLinkRole = role;
				break;
			}
		}
    	
		return sLinkRole;
	}
	
    /**
     * Adds a new role.
     * @param role the role to add
     */
    public void addRole(PartnerLinkRole role) {
        super.addChild(role);
        mRoles.add(role);
    }
    
    /**
     * Removes the role at the specified position.
     * @param index the position of the role to remove
     */
    public void removeRole(int index) {
        PartnerLinkRole role = getRole(index);
        super.removeChild(role);
        mRoles.remove(index);
    }
    
    /**
     * Removes the given role.
     * @param role to be removed
     */
    public void removeRole(PartnerLinkRole role) {
        super.removeChild(role);
        mRoles.remove(role);
    }
    
    /**
     * @see PartnerLinkType#indexOfRole
     */
    public int indexOfRole(XMLNode role) {
        return mRoles.indexOf(role);
    }

    /**
     * Gets the list of all roles.
     * @return a read-only collection of ServiceLinkRoles.
     */
    public Collection getRoles() {
        return Collections.unmodifiableCollection(mRoles);
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
