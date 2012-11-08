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
 * @(#)VariablePK.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.datamodel;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Embeddable;

/**
 * Primary Key class VariablePK for entity class Variable
 * 
 * @author nnahata
 */
@Embeddable
public class VariablePK implements Serializable {

    @Column(name = "BPID", nullable = false)
    private String bpid;

    @Column(name = "ID", nullable = false)
    private long id;
    
    /** Creates a new instance of VariablePK */
    public VariablePK() {
    }

    /**
     * Creates a new instance of VariablePK with the specified values.
     * @param id the id of the VariablePK
     * @param bpid the bpid of the VariablePK
     */
    public VariablePK(long id, String bpid) {
        this.id = id;
        this.bpid = bpid;
    }

    /**
     * Gets the bpid of this VariablePK.
     * @return the bpid
     */
    public String getBpid() {
        return this.bpid;
    }

    /**
     * Sets the bpid of this VariablePK to the specified value.
     * @param bpid the new bpid
     */
    public void setBpid(String bpid) {
        this.bpid = bpid;
    }

    /**
     * Gets the id of this VariablePK.
     * @return the id
     */
    public long getId() {
        return this.id;
    }

    /**
     * Sets the id of this VariablePK to the specified value.
     * @param id the new id
     */
    public void setId(long id) {
        this.id = id;
    }

    /**
     * Returns a hash code value for the object.  This implementation computes 
     * a hash code value based on the id fields in this object.
     * @return a hash code value for this object.
     */
    @Override
    public int hashCode() {
        int hash = 0;
        hash += (int)id;
        hash += (this.bpid != null ? this.bpid.hashCode() : 0);
        return hash;
    }

    /**
     * Determines whether another object is equal to this VariablePK.  The result is 
     * <code>true</code> if and only if the argument is not null and is a VariablePK object that 
     * has the same id field values as this object.
     * @param object the reference object with which to compare
     * @return <code>true</code> if this object is the same as the argument;
     * <code>false</code> otherwise.
     */
    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof VariablePK)) {
            return false;
        }
        VariablePK other = (VariablePK)object;
        if (this.id != other.id) return false;
        if (this.bpid != other.bpid && (this.bpid == null || !this.bpid.equals(other.bpid))) return false;
        return true;
    }

    /**
     * Returns a string representation of the object.  This implementation constructs 
     * that representation based on the id fields.
     * @return a string representation of the object.
     */
    @Override
    public String toString() {
        return "com.sun.jbi.cam.components.bpelse.db.VariablePK[id=" + id + ", bpid=" + bpid + "]";
    }
    
}
