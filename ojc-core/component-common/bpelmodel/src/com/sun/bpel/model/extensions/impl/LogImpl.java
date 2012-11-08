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
package com.sun.bpel.model.extensions.impl;

import javax.xml.namespace.QName;

import com.sun.bpel.model.From;
import com.sun.bpel.model.extensions.Log;
import com.sun.bpel.model.impl.BPELElementImpl;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.NamespaceUtility;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.visitor.Visitor;

/*
 * @(#)LogImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
public class LogImpl extends BPELElementImpl implements Log {

	/* */
	private static final long serialVersionUID = 1629181853423601199L;
	
	/** QName object for SeeBeyond Private extension line label */
    public static final QName LOG_QNAME = NamespaceUtility.getQName(
    		XMLElement.SBYNBPEL_RUNTIME_EXTN_NAMESPACE, Log.TAG, XMLElement.SBYNBPEL_RUNTIME_EXTN_PREFIX);

	/* */
	private From mFrom;
	
    /** Creates a new instance of LogImpl */
    public LogImpl() {
        super();
        initLog();
    }
    
    /** Creates a new instance of LogImpl.
     * @param   d   Owner document.
     */
    public LogImpl(XMLDocument d) {
        super(d);
        initLog();
    }
	
    /** Initializes this class.
     */
    private void initLog() {
    	setLocalName(Log.TAG);
        setQualifiedName(LOG_QNAME);
    	xmlAttrs = new XMLAttribute[] {
    			new XMLAttributeImpl(ATTR.LEVEL, String.class, false, null),
    			new XMLAttributeImpl(ATTR.LOCATION, String.class, true, null)
    	};
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
	
	/*
	 * (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Log#getFrom()
	 */
	public From getFrom() {
		return mFrom;
	}

	/*
	 * (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Log#getLevel()
	 */
	public String getLevel() {
		String level = xmlAttrs[LEVEL].getValue();
		if (level == null) {
			return DEFAULT_LEVEL;
		} 
		return level;
	}

	/*
	 * (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Log#getLocation()
	 */
	public String getLocation() {
		String location = xmlAttrs[LOCATION].getValue();
		if (location == null) {
			return DEFAULT_LOCATION;
		}
		return location; 
	}

	/*
	 * (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Log#setFrom(com.sun.bpel.model.From)
	 */
	public void setFrom(From from) {
		super.addChild(from);
		mFrom = from;
	}

	/*
	 * (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Log#setLevel(java.lang.String)
	 */
	public void setLevel(String level) {
		setAttribute(LEVEL, level);
	}

	/*
	 * (non-Javadoc)
	 * @see com.sun.bpel.model.extensions.Log#setLocation(java.lang.String)
	 */
	public void setLocation(String location) {
		setAttribute(LOCATION, location);
	}

}
