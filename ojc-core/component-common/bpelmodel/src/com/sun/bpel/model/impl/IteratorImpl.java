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
 * @(#)IteratorImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.FinalCounterValue;
import com.sun.bpel.model.Iterator;
import com.sun.bpel.model.StartCounterValue;
import com.sun.bpel.model.wsdlmodel.impl.XMLElementImpl;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class IteratorImpl extends XMLElementImpl implements Iterator {

	/** Holds value of property FinalCounterValue. */
    private FinalCounterValue mFinalCounterValue;
    
    /** Holds value of property StartCounterValue. */
    private StartCounterValue mStartCounterValue;
    
    
	/** Creates a new instance of IteratorImpl */
    public IteratorImpl() {
        super();
        initIterator();
    }

    /** Creates a new instance of IteratorImpl */
    public IteratorImpl(XMLDocument d) {
        super(d);
        initIterator();
    }
    
    private void initIterator() {
    	setLocalName(Iterator.TAG);
    }

    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof StartCounterValue) {
            setStartCounterValue((StartCounterValue) c);
        } else if (c instanceof FinalCounterValue) {
            setFinalCounterValue((FinalCounterValue) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof StartCounterValue) {
            setStartCounterValue(null);
        } else if (c instanceof FinalCounterValue) {
            setFinalCounterValue(null);
        } else {
            super.removeChild(c);
        }
    }
    
	public FinalCounterValue getFinalCounterValue() {
		return this.mFinalCounterValue;
	}

	public StartCounterValue getStartCounterValue() {
		return this.mStartCounterValue;
	}

	public void setStartCounterValue(StartCounterValue sValue) {
		StartCounterValue oldCounter = this.mStartCounterValue;
		this.mStartCounterValue = sValue;
		super.replaceChild(0, oldCounter, sValue);
        
	}
	
	public void setFinalCounterValue(FinalCounterValue fValue) {
		FinalCounterValue oldCounter = this.mFinalCounterValue;
		super.replaceChild(1, oldCounter, fValue);
        this.mFinalCounterValue = fValue;
	}
    
    
}
