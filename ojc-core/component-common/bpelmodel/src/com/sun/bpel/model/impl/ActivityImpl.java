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
 * @(#)ActivityImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.Source;
import com.sun.bpel.model.Target;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;




/**
 * Base class for all activity elements.
 *
 * @author Sun Microsystems
 * @version 
 */
public class ActivityImpl extends BPELElementImpl implements Activity {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -8434240394235591720L;
    
    /** Holds standard XML attributes for BPEL activities. */
    protected XMLAttribute[] standardXmlAttrs;
    
    /** Holds value of property targets. */
    private ArrayList targets = new ArrayList();
    
    /** Holds value of property sources. */
    private ArrayList sources = new ArrayList();
    
    /** Instantiates a activity element object.
     */
    public ActivityImpl() {
        super();
        initActivity();
    }
    
    /** Instantiates a activity element object.
     * @param   d   Owner document.
     */
    public ActivityImpl(XMLDocument d) {
        super(d);
        initActivity();
    }
    
    /** Initializes this class. */
    private void initActivity() {
        standardXmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(Activity.ATTR.NAME, String.class, true, null),
            new XMLAttributeImpl(Activity.ATTR.JOIN_CONDITION, String.class, true, null),
            new XMLAttributeImpl(Activity.ATTR.SUPPRESS_JOIN_FAILURE, String.class, true,  
            		XMLAttribute.BOOLEAN_ENUM_VALS),
            new XMLAttributeImpl(Activity.ATTR.GENERATE_EVENTS, String.class, true, XMLAttribute.BOOLEAN_ENUM_VALS)
        };
        assert(NUM_STANDARD_ATTRS == standardXmlAttrs.length);
        xmlAttrs = standardXmlAttrs;
        childrenTags = ACTIVITY_TAGS;
    }
   
    /** @see com.sun.bpel.model.Activity#getName()
     */
    public String getName() {
        return standardXmlAttrs[NAME].getValue();
    }
    
    /** @see com.sun.bpel.model.Activity#setName(java.lang.String)
     */
    public void setName(String name) {
        setAttribute(NAME, name);
    }
    
    /** @see com.sun.bpel.model.Activity#setName(java.lang.String, java.lang.String)
     */
    public void setName(String qName, String name) {
        setAttribute(NAME, qName, name);
    }
    
    /** @see com.sun.bpel.model.Activity#getGenerateEvents()
     */
    public String getGenerateEvents() {
        return standardXmlAttrs[GENERATE_EVENTS].getValue();
    }
    
    /** @see com.sun.bpel.model.Activity#setGenerateEvents(java.lang.String)
     */
    public void setGenerateEvents(String generateEvents) {
        setAttribute(GENERATE_EVENTS, generateEvents);
    }
    
    /** @see com.sun.bpel.model.Activity#getJoinCondition()
     */
    public String getJoinCondition() {
        return standardXmlAttrs[JOIN_CONDITION].getValue();
    }
    
    /** @see com.sun.bpel.model.Activity#setJoinCondition(java.lang.String)
     */
    public void setJoinCondition(String joinCondition) {
        setAttribute(JOIN_CONDITION, joinCondition);
    }
   
    
    /** @see com.sun.bpel.model.Activity#getSuppressJoinFailure()
     */
    public String getSuppressJoinFailure() {
        return standardXmlAttrs[SUPPRESS_JOIN_FAILURE].getValue();
    }
    
    /** @see com.sun.bpel.model.Activity#setSuppressJoinFailure(java.lang.String)
     */
    public void setSuppressJoinFailure(String suppressJoinFailure) {
        setAttribute(SUPPRESS_JOIN_FAILURE, suppressJoinFailure);
    }
   
    
    /** @see com.sun.bpel.model.common.model.XMLNode#addChild(com.sun.bpel.model.common.model.XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Target) {
            addTarget((Target) c);
        } else if (c instanceof Source) {
            addSource((Source) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see com.sun.bpel.model.common.model.XMLNode#removeChild(com.sun.bpel.model.common.model.XMLNode)
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Target) {
            removeTarget((Target) c);
        } else if (c instanceof Source) {
            removeSource((Source) c);
        } else {
            super.removeChild(c);
        }
    }
    
    
    
    /** @see com.sun.bpel.model.Activity#addTarget(
     *  com.sun.bpel.model.Target)
     */
    public void addTarget(Target t) {
        super.addChild(1, t);
        targets.add(t);
    }
    
    
    /** @see com.sun.bpel.model.Activity#removeTarget(
     *  com.sun.bpel.model.Target)
     */
    public boolean removeTarget(Target t) {
        super.removeChild(t);
        return targets.remove(t);
    }
    
    /** @see com.sun.bpel.model.Activity#getTargetSize()
     */
    public int getTargetSize() {
        return targets.size();
    }
    
    
   
    /** @see com.sun.bpel.model.Activity#addSource(
     *  com.sun.bpel.model.Source)
     */
    public void addSource(Source s) {
        super.addChild(2, s);
        sources.add(s);
    }
    
    
    /** @see com.sun.bpel.model.Activity#removeSource(
     *  com.sun.bpel.model.Source)
     */
    public boolean removeSource(Source s) {
        super.removeChild(s);
        return sources.remove(s);
    }
    
    /** @see com.sun.bpel.model.Activity#getSourceSize()
     */
    public int getSourceSize() {
        return sources.size();
    }

	public Collection getSources() {
		return sources;
	}

	public Collection getTargets() {
		return targets;
	}

	public Source getSource(String linkName) {
		if(linkName == null) {
			return null;
		}
		
		Source source = null;
		Iterator it = sources.iterator();
		while(it.hasNext()) {
			Source s = (Source) it.next();
			if(linkName.equals(s.getLinkName())) {
				source = s;
				break;
			}
		}
		return source;
	}

	public Target getTarget(String linkName) {
		if(linkName == null) {
			return null;
		}
		
		Target target = null;
		Iterator it = targets.iterator();
		while(it.hasNext()) {
			Target t = (Target) it.next();
			if(linkName.equals(t.getLinkName())) {
				target = t;
				break;
			}
		}
		return target;
	}
}
