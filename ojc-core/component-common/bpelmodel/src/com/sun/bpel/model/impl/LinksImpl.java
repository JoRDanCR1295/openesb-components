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
 * @(#)LinksImpl.java 
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

import com.sun.bpel.model.Link;
import com.sun.bpel.model.Links;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;links&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class LinksImpl extends BPELElementImpl implements Links {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 6018680361134643693L;
    
    /** Hold link element list. */
    private ArrayList links = new ArrayList();
    
    /** Creates a new instance of LinksImpl */
    public LinksImpl() {
        super();
        initLinks();
    }
    
    /** Creates a new instance of LinksImpl
     * @param   d   Owner document.
     */
    public LinksImpl(XMLDocument d) {
        super(d);
        initLinks();
    }
    
    /** Initializes this class.
     */
    private void initLinks() {
        setLocalName(Links.TAG);
        childrenTags = new String[] {
            Link.TAG
        };
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Link) {
            addLink((Link) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Link) {
            removeLink((Link) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see Links#getLink
     */
    public Link getLink(int i) {
        return (Link) links.get(i);
    }
    
    /** @see Links#setLink
     */
    public synchronized void setLink(int i, Link l) {
        if (links.size() == i) {
            addLink(l);
        } else {
            replaceChild(1, (Link) links.get(i), l);
            links.set(i, l);
        }
    }
    
    /** @see Links#addLink(Link)
     */
    public synchronized void addLink(Link l) {
        super.addChild(1, l);
        links.add(l);
    }
    
    /** @see Links#addLink(int, Link)
     */
    public synchronized void addLink(int index, Link l) {
        if ((index < 0) || (index > links.size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + links.size());
        } else if (index == links.size()) {
            addLink(l);
        } else {
            super.addChild(1, getLink(index), l);
            links.add(index, l);
        }
    }
    
    /** @see Links#clearLinks
     */
    public synchronized void clearLinks() {
        while (links.size() > 0) {
            removeLink(0);  // stays at 0 because array elements keep shifting to the left
        }
    }
    
    /** @see Links#removeLink(int)
     */
    public synchronized void removeLink(int i) {
        removeLink(getLink(i));
    }
    
    /** @see Links#removeLink(Link)
     */
    public synchronized boolean removeLink(Link l) {
        super.removeChild(l);
        return links.remove(l);
    }
    
    /** @see Links#getLinkSize
     */
    public int getLinkSize() {
        return links.size();
    }
    
    /** @see Links#indexOfLink
     */
    public int indexOfLink(XMLNode link) {
        return links.indexOf(link);
    }
    
    /** @see Links#getLinks
     */
    public synchronized Collection getLinks() {
        return Collections.unmodifiableCollection((ArrayList) links.clone());
    }
    
    
    public Link getLink(String linkName) {
    	if(linkName == null) {
    		return null;
    	}
    	
    	Link link = null;
		Iterator it = links.iterator();
		while(it.hasNext()) {
			Link l = (Link) it.next();
			if(linkName.equals(l.getName())) {
				link = l;
				break;
			}
		}
		
		return link;
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
