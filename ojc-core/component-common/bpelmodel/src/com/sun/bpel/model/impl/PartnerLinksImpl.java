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
 * @(#)PartnerLinksImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.PartnerLinks;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;


/**
 * Implements the &lt;partners&gt; element.
 *
 * @author Sun Microsystems
 */
public class PartnerLinksImpl extends BPELElementImpl implements PartnerLinks {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -2359300649536230968L;
    
    /** Holds the partner sub-elements. */
    private ArrayList partners = new ArrayList();
    
    /** Creates a new instance of PartnerLinksImpl.
     */
    public PartnerLinksImpl() {
        super();
        initPartners();
    }
    
    /** Creates a new instance of PartnerLinksImpl.
     * @param   d   Owner document.
     */
    public PartnerLinksImpl(XMLDocument d) {
        super(d);
        initPartners();
    }
    
    /** Initializes this class.
     */
    private void initPartners() {
        setLocalName(PartnerLinks.TAG);
        childrenTags = new String[] {
            PartnerLink.TAG
        };
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof PartnerLink) {
            addPartnerLink((PartnerLink) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof PartnerLink) {
            removePartnerLink((PartnerLink) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /** name based getter for property partner.
     * @param name name of the partner.
     * @return Value of the property with given <CODE>name</CODE>.
     *
     */
    public PartnerLink getPartnerLink(String name) {
    	PartnerLink partner = null;
    	Iterator it = partners.iterator();
    	
    	while(it.hasNext()) {
    		PartnerLink p = (PartnerLink) it.next();
    		
    		if(name != null && name.equals(p.getName())) {
    			partner = p; 
    			break;
    		}
    	}
    	
    	return partner;
    }
    
    /** @see PartnerLinks#getPartner
     */
    public PartnerLink getPartnerLink(int index) {
        return (PartnerLink) partners.get(index);
    }
    
    
    /** @see PartnerLinks#addPartnerLink(PartnerLink)
     */
    public synchronized void addPartnerLink(PartnerLink partner) {
    	partners.add(partner);
    	super.addChild(1, partner);
    }
    
    /** @see PartnerLinks#addPartner(int, PartnerLink)
     */
    public synchronized void addPartner(int index, PartnerLink partner) {
        if ((index < 0) || (index > partners.size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + partners.size());
        } else if (index == partners.size()) {
            addPartnerLink(partner);
        } else {
        	PartnerLink oldPartner = getPartnerLink(index);
        	partners.add(index, partner);
            super.addChild(1, oldPartner, partner);
            
        }
    }
    
    /** @see PartnerLinks#clearPartners
     */
    public synchronized void clearPartners() {
        while (partners.size() > 0) {
            removePartnerLink(0);  // stays at 0 because array elements keep shifting to the left
        }
    }
    
    /** @see PartnerLinks#removePartnerLink(int)
     */
    public synchronized void removePartnerLink(int i) {
        removePartnerLink(getPartnerLink(i));
    }
    
    /** @see PartnerLinks#removePartnerLink(PartnerLink)
     */
    public synchronized boolean removePartnerLink(PartnerLink p) {
    	boolean result = partners.remove(p);
    	super.removeChild(p);
    	return result;
    }
    
    /** @see PartnerLinks#getPartnersSize
     */
    public int getPartnerLinksSize() {
        return partners.size();
    }
    
    
    /** @see PartnerLinks#getPartners
     */
    public synchronized Collection getPartnerLinks() {
        return Collections.unmodifiableCollection((ArrayList) partners.clone());
    }
    
    /** @see XMLNode#accept
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
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
