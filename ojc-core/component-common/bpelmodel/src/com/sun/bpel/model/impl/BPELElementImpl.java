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
 * @(#)BPELElementImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;


import java.util.ArrayList;
import java.util.List;

import com.sun.bpel.model.BPELDocumentation;
import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.extensions.Trace;
import com.sun.bpel.model.visitor.SAXWriteVisitorService;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.VisitorService;



/**
 * Provides a base implementation for BPEL XML elements.
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class BPELElementImpl extends BaseElementImpl implements BPELElement {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 6217630788366432166L;
    
    private List documentations = new ArrayList();
    
    private Trace mTrace = null;
    
    /** Visitor service to use for serializing */
    private static VisitorService visitorService = new SAXWriteVisitorService();
    
    /** Creates a new instance of BPELElementImpl */
    public BPELElementImpl() {
        super();
    }
    
    /** Creates a new instance of BPELElementImpl.
     * @param   d   Owner document.
     */
    public BPELElementImpl(XMLDocument d) {
        super(d);
    }
    
    /**
     * @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof BPELDocumentation) {
        	addDocumentation((BPELDocumentation) c);
        } else {
            super.supperAddChild(c);
        }
    }
    
    /**
     * @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof BPELDocumentation) {
        	removeDocumentation((BPELDocumentation) c);
        } else {
            super.supperRemoveChild(c);
        }
    }
    
    public void addDocumentation(BPELDocumentation documentation) {
    	documentations.add(documentation);
    	super.addChild(0, documentation);
    }
    
    public void removeDocumentation(BPELDocumentation documentation) {
    	documentations.remove(documentation);
    	super.supperRemoveChild(documentation);
    }
    
    public List getDocumentations() {
    	return documentations;
    }

	public Trace getTrace() {
		return mTrace;
	}

	public void setTrace(Trace trace) {
		super.addChild(trace);
		mTrace = trace;
	}
}
