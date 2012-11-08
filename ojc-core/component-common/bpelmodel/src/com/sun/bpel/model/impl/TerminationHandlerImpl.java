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
 * @(#)TerminationHandlerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.TerminationHandler;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;terminationHandler&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class TerminationHandlerImpl extends BPELElementImpl
    implements TerminationHandler {
    
    /** serialVersionUID for this class */
    //static final long serialVersionUID = -5757902783080111756L;
    
    /** Holds the activity */
    private Activity activity;
    
    /** Creates a new instance of TerminationHandlerImpl */
    public TerminationHandlerImpl() {
        super();
        initTerminationHandler();
    }
    
    /** Creates a new instance of TerminationHandlerImpl.
     * @param   d   Owner document.
     */
    public TerminationHandlerImpl(XMLDocument d) {
        super(d);
        initTerminationHandler();
    }
    
    /** Initializes this class.
     */
    private void initTerminationHandler() {
        setLocalName(TerminationHandler.TAG);
        childrenTags = new String[] {
            Activity.TAG
        };
    }
    
    /** Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        if (c instanceof Activity) {
            setActivity((Activity) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Activity) {
            setActivity(null);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see SingleActivityHolder#getActivity
     */
    public Activity getActivity() {
        return activity;
    }
    
    /** @see SingleActivityHolder#setActivity
     */
    public void setActivity(Activity a) {
        super.replaceChild(1, activity, a);
        activity = a;
    }
    
    /** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
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
