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
 * @(#)BPELExtensibleElementImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import com.sun.bpel.model.BPELExtensibleElement;
import com.sun.bpel.model.extensions.ExtensibilityElement;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;

/**
 * Implements the base for BPEL Extensibility Elements.
 *
 * @author Sun Microsystems
 * @version 
 */
public class BPELExtensibleElementImpl extends BPELElementImpl implements BPELExtensibleElement {
    
    /** Holds the list of extensibility elements. */
    protected ArrayList extensibilityElems = new ArrayList();
    
    /** Creates a new instance of BPELExtensibleElementImpl. */
    public BPELExtensibleElementImpl() {
        super();
        initBPELExtensibleElement();
    }
    
    /** Creates a new instance of BPELExtensibleElementImpl.
     * @param   d   Owner document.
     */
    public BPELExtensibleElementImpl(XMLDocument d) {
        super(d);
        initBPELExtensibleElement();
    }
    
    /** Initializes this class. */
    private void initBPELExtensibleElement() {
        childrenTags = null;
    }
    
    /** @see com.sun.bpel.model.common.model.XMLNode#addChild(com.sun.bpel.model.common.model.XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof ExtensibilityElement) {
            addExtensibilityElement((ExtensibilityElement) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see com.sun.bpel.model.common.model.XMLNode#removeChild(com.sun.bpel.model.common.model.XMLNode)
     */
    public void removeChild(XMLNode c) {
        if (c instanceof ExtensibilityElement) {
            removeExtensibilityElement((ExtensibilityElement) c);
        } else {
            super.removeChild(c);
        }
    }    
        
    /**
     * @see com.sun.bpel.model.BPELExtensibleElement#getExtensibilityElement(int)
     */
    public ExtensibilityElement getExtensibilityElement(int i) {
        return (ExtensibilityElement) extensibilityElems.get(i);
    }
    
    
    /**
     * @see com.sun.bpel.model.BPELExtensibleElement#setExtensibilityElement(int,
     *  com.sun.bpel.model.extensions.ExtensibilityElement)
     */
    public void setExtensibilityElement(int i, ExtensibilityElement elem) {
        if (extensibilityElems.size() == i) {
            addExtensibilityElement(elem);
        } else {
            replaceChild(getExtensibilityElement(i), elem);
            extensibilityElems.set(i, elem);
        }
    }
    
    /**
     * @see com.sun.bpel.model.BPELExtensibleElement#addExtensibilityElement(
     *  com.sun.bpel.model.extensions.ExtensibilityElement)
     */
    public void addExtensibilityElement(ExtensibilityElement elem) {
        super.addChild(elem);
        extensibilityElems.add(elem);
    }
    
    /**
     * @see com.sun.bpel.model.BPELExtensibleElement#removeExtensibilityElement(int)
     */
    public void removeExtensibilityElement(int i) {
        super.removeChild(getExtensibilityElement(i));
        extensibilityElems.remove(i);
    }
    
    /**
     * @see com.sun.bpel.model.BPELExtensibleElement#removeExtensibilityElement(
     *  com.sun.bpel.model.extensions.ExtensibilityElement)
     */
    public boolean removeExtensibilityElement(ExtensibilityElement elem) {
        super.removeChild(elem);
        return extensibilityElems.remove(elem);
    }
    
    /**
     * @see com.sun.bpel.model.BPELExtensibleElement#getExtensibilityElementsSize()
     */
    public int getExtensibilityElementsSize() {
        return extensibilityElems.size();
    }
    
    /**
     * @see com.sun.bpel.model.BPELExtensibleElement#getExtensibilityElements()
     */
    public Collection getExtensibilityElements() {
        return Collections.unmodifiableCollection(extensibilityElems);
    }
    
    /** @see com.sun.bpel.model.BPELExtensibleElement#indexOfExtensibilityElement(
     *  com.sun.bpel.model.common.model.XMLNode)
     */
    public int indexOfExtensibilityElement(XMLNode elem) {
        return extensibilityElems.indexOf(elem);
    }
}
