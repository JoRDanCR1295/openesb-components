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
 * @(#)RVariableImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlBoolean;
import org.apache.xmlbeans.XmlDate;
import org.apache.xmlbeans.XmlDateTime;
import org.apache.xmlbeans.XmlDecimal;
import org.apache.xmlbeans.XmlDouble;
import org.apache.xmlbeans.XmlFloat;
import org.apache.xmlbeans.XmlInteger;
import org.apache.xmlbeans.XmlString;
import org.apache.xmlbeans.XmlTime;

import com.sun.bpel.model.impl.VariableImpl;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;

/**
 * Runtime Variable implementation
 *
 * @author Sun Microsystems
 */
public class RVariableImpl extends VariableImpl implements RVariable {
    private static final long serialVersionUID = 3256727264572813369L;

    /** Holds uniqueId which is unique for this variable in a business process */
    private long mUniqueId;

    /** Holds containing scope's (scope or process) unique id */
    private long mScopeUniqueId;

    private boolean isBoolean = false;
    private boolean isString = false;
    private boolean isNumber = false;
    private boolean isDate = false;
    private boolean isTime = false;
    private boolean isInteger = false;
    
    /**
     * Creates a new RVariableImpl object.
     *
     * @param bpeldoc runtime BPEL document
     * @param uniqueId unique ID
     */
    public RVariableImpl(RBPELDocumentImpl bpeldoc, long uniqueId) {
        super(bpeldoc);
        mUniqueId = uniqueId;
    }

    /**
     * gets unique ID
     *
     * @return long unique ID
     */
    public long getUniqueId() {
        return mUniqueId;
    }

    /**
     * sets scope ID
     *
     * @param scopeUniqueid scope ID
     */
    public void setScopeId(long scopeUniqueid) {
        mScopeUniqueId = scopeUniqueid;
    }

    public void initMembers() {
        super.initMembers();
        SchemaType type = getXSDType();
        if (type == null) {
            return;
        }
        
        	
        if (XmlDecimal.type.isAssignableFrom(type)
                || XmlDouble.type.isAssignableFrom(type)
                || XmlFloat.type.isAssignableFrom(type)) {
            isNumber = true;
            if (XmlInteger.type.isAssignableFrom(type)) {
            	isInteger = true;
            }
        } else if (XmlDateTime.type.isAssignableFrom(type)) {
            isDate = true;
        } else if (XmlDate.type.isAssignableFrom(type)) {
            isDate = true;
        } else if (XmlTime.type.isAssignableFrom(type)) {
            isTime = true;
        } else if (XmlBoolean.type.isAssignableFrom(type)) {
            isBoolean = true;
        } else if (XmlString.type.isAssignableFrom(type)) {
            isString = true;
        }
    }

    /**
     * gets scope ID
     *
     * @return long scope ID
     */
    public long getScopeId() {
        return mScopeUniqueId;
    }

    public boolean isBoolean() {
        return isBoolean;
    }

    public boolean isString() {
        return isString;
    }

    public boolean isNumber() {
        return isNumber;
    }
    
    public boolean isInteger() {
    	return isInteger;
    }
    
    public boolean isDateTime() {
        return isDate;
    }

	public boolean isDate() {
		return isDate;
	}
    
    public boolean isTime() {
        return isTime;
    }
}
