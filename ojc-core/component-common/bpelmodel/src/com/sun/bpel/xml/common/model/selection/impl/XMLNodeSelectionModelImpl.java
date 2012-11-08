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
 * @(#)XMLNodeSelectionModelImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.common.model.selection.impl;

import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.model.selection.XMLNodeSelectionEvent;
import com.sun.bpel.xml.common.model.selection.XMLNodeSelectionListener;
import com.sun.bpel.xml.common.model.selection.XMLNodeSelectionModel;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 *
 * @author Sun Microsystems
 */
public class XMLNodeSelectionModelImpl implements XMLNodeSelectionModel {
    
    /** A list of event listeners for this component. */
    private Set listenerSet = new HashSet();

    private XMLDocument mOwnerDocument;
    
    private XMLNode[] mSelected;
    
    /** Creates a new instance of XMLNodeSelectionModelImpl */
    public XMLNodeSelectionModelImpl(XMLDocument owner) {
        this.mOwnerDocument = owner;
    }
    
    
    /**
     * Get the document whose selection is managed by this
     */
    public XMLDocument getOwnerDocument() {
        return this.mOwnerDocument;
    }
    
    /**
     * select a XMLNode in selection model. It will fire event to notify all
     * registered XMLNodeSelectionListener
     * @param source XMLNode
     */
    public void setSelected(XMLNode[] source) {
        XMLNode[] oldSelection = this.mSelected;
        this.mSelected = source;
        this.fireSelectionChanged(oldSelection, this.mSelected);
    }
    
    /**
     * get currently selected XMLNode in this selection model.
     */
    public XMLNode[] getSelected() {
        return this.mSelected;
    }
    
    /**
     * add selection listener
     * @param l XMLNodeSelectionListener
     */
    public void addXMLNodeSelectionListener(XMLNodeSelectionListener l) {
        listenerSet.add(l);
    }
    
    /**
     * remove selection listener
     * @param l XMLNodeSelectionListener
     */
    public void removeXMLNodeSelectionListener(XMLNodeSelectionListener l) {
        listenerSet.remove(l);
    }
    
    
    private void fireSelectionChanged(XMLNode[] oldSelection, XMLNode[] newSelection) {
    	XMLDocument owner = getOwnerDocument();
    	if(owner != null && !owner.isEnableEvents()) {
    		return;
    	}
    	
    	XMLNodeSelectionEvent event = 
        	new XMLNodeSelectionEvent(this, 
					  oldSelection, 
					  newSelection);
    	
    	Iterator it = listenerSet.iterator();
        while(it.hasNext()) {
	      	Object listener = it.next();
	      	((XMLNodeSelectionListener)listener).selectionChanged(event);
        }
       
     }
}
