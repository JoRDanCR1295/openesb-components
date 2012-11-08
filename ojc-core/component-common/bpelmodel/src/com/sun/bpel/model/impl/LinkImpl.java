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
 * @(#)LinkImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.Collection;
import java.util.Iterator;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.Flow;
import com.sun.bpel.model.Link;
import com.sun.bpel.model.Links;
import com.sun.bpel.model.MultipleActivityHolder;
import com.sun.bpel.model.Source;
import com.sun.bpel.model.Target;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.Visitor;


/**
 * Implements the &lt;link&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class LinkImpl extends BPELElementImpl implements Link {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -3106670169418355681L;
    
    /** Creates a new instance of LinkImpl */
    public LinkImpl() {
        super();
        initLink();
    }
    
    /** Creates a new instance of LinkImpl.
     * @param   d   Owner document.
     */
    public LinkImpl(XMLDocument d) {
        super(d);
        initLink();
    }
    
    /** Initializes this class.
     */
    private void initLink() {
        setLocalName(Link.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.NAME, String.class, false, null)
        };
    }
    
    /** Getter for property name.
     * @return  Value of property name.
     *
     */
    public String getName() {
        return xmlAttrs[NAME].getValue();
    }
    
    /** Setter for property name.
     * @param   n   Value of property name.
     *
     */
    public void setName(String n) {
        setAttribute(NAME, n);
    }
    
    /** Setter for property name.
     * @param   qName   Qualified name.
     * @param   n       Value of property name.
     */
    public void setName(String qName, String n) {
        setAttribute(NAME, qName, n);
    }
    

    
	public Source getSource() {
		Source source = null;
		Links links = (Links) getParent();
		if(links != null) {
			Flow flow = (Flow)links.getParent();
			source = getMatchingSource(flow);
		}
		return source;
	}

	public Target getTarget() {
		Target target = null;
		Links links = (Links) getParent();
		if(links != null) {
			Flow flow = (Flow)links.getParent();
			target = getMatchingTarget(flow);
		}
		return target;
	}

	public Activity getSourceActivity() {
		Activity sActivity = null;
		Links links = (Links) getParent();
		if(links != null) {
			Flow flow = (Flow)links.getParent();
			Source source = getMatchingSource(flow);
			if(source != null) {
				sActivity = (Activity) source.getParent();
			}
		}
		return sActivity;
	}

	public Activity getTargetActivity() {
		Activity tActivity = null;
		Links links = (Links) getParent();
		if(links != null) {
			Flow flow = (Flow)links.getParent();
			Target target = getMatchingTarget(flow);
			if(target != null) {
				tActivity = (Activity) target.getParent();
			}
		}
		return tActivity;
	}

	/** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
        return v.visit(this);
    }
    
    private Source getMatchingSource(MultipleActivityHolder mActivity) {
    	Source source = null;
    	if(mActivity != null) {
			Collection activities = mActivity.getActivities();
			if(activities != null) {
				Iterator it = activities.iterator();
				while(it.hasNext()) {
					Activity act = (Activity) it.next();
					Source s = act.getSource(getName());
					if(source != null) {
						source = s;
						break;
					} else if (act instanceof MultipleActivityHolder) {
						return getMatchingSource((MultipleActivityHolder) act);
					}
				}
			}
		}
    	return source;
    }
    
    private Target getMatchingTarget(MultipleActivityHolder mActivity) {
    	Target target = null;
    	if(mActivity != null) {
			Collection activities = mActivity.getActivities();
			if(activities != null) {
				Iterator it = activities.iterator();
				while(it.hasNext()) {
					Activity act = (Activity) it.next();
					Target t = act.getTarget(getName());
					if(target != null) {
						target = t;
						break;
					} else if (act instanceof MultipleActivityHolder) {
						return getMatchingTarget((MultipleActivityHolder) act);
					}
				}
			}
		}
    	return target;
    }
}
