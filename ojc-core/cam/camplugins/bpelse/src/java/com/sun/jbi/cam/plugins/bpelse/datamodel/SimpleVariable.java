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
 * @(#)SimpleVariable.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.datamodel;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * Entity class Variable
 * 
 * @author nnahata
 */
public class SimpleVariable implements Serializable {
	
	protected VariablePK variablePK;
	
    private Boolean booleanValue = null;
    private String stringValue = null;
    private BigDecimal numericValue = null;
    private Timestamp timestampVal =  null;
    
    /** Creates a new instance of Variable */
    public SimpleVariable() {
    }

    public SimpleVariable(String bpid, long id, String stringvalue, 
            BigDecimal numericvalue, Boolean booleanvalue,
            Timestamp timestampVal) {
    	this.variablePK = new VariablePK(id, bpid);
    	this.stringValue = stringvalue;
    	this.booleanValue = booleanvalue;
    	this.numericValue = numericvalue;
        this.timestampVal = timestampVal;
    }
    
    
    public String getStringValue() {
        return this.stringValue;
    }
    
    public BigDecimal getNumericValue() {
        return this.numericValue;
    }

    public Boolean getBooleanValue() {
        return this.booleanValue;
    }
    
    public void setStringValue(String value) {
        this.stringValue = value;
    }
    
    public void setNumericValue(BigDecimal value) {
        this.numericValue = value;
    }
    
    public void setBooleanValue(Boolean value) {
        this.booleanValue = value;
    }
    
    public Timestamp  getTimestamp(){
    	return timestampVal;
    }
    
    public void setTimestamp(Timestamp timestampVal){
    	this.timestampVal = timestampVal;
    }

    
    public String getBpid(){
    	return variablePK.getBpid();
    }
    
    public long getId(){
    	return variablePK.getId();
    }
    
    /**
     * Returns a hash code value for the object.  This implementation computes 
     * a hash code value based on the id fields in this object.
     * @return a hash code value for this object.
     */
    @Override
    public int hashCode() {
        int hash = 0;
        hash += (this.variablePK != null ? this.variablePK.hashCode() : 0);
        return hash;
    }

    /**
     * Determines whether another object is equal to this Variable.  The result is 
     * <code>true</code> if and only if the argument is not null and is a Variable object that 
     * has the same id field values as this object.
     * @param object the reference object with which to compare
     * @return <code>true</code> if this object is the same as the argument;
     * <code>false</code> otherwise.
     */
    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof Variable)) {
            return false;
        }
        Variable other = (Variable)object;
        if (this.variablePK != other.variablePK && (this.variablePK == null || !this.variablePK.equals(other.variablePK))) return false;
        return true;
    }

    /**
     * Returns a string representation of the object.  This implementation constructs 
     * that representation based on the id fields.
     * @return a string representation of the object.
     */
    @Override
    public String toString() {
        return "com.sun.jbi.cam.components.bpelse.db.Variable[variablePK=" + variablePK + "]";
    }    
}
