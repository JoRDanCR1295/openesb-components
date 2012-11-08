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
 * @(#)ScopesnapshotPK.java 
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
 * Primary Key class ScopesnapshotPK for entity class Scopesnapshot
 * 
 * @author nnahata
 */
@Embeddable
public class ScopesnapshotPK implements Serializable {

    @Column(name = "BPID", nullable = false)
    private String bpid;

    @Column(name = "ID", nullable = false)
    private long id;

    @Column(name = "VARID", nullable = false)
    private long varid;

    @Column(name = "ITERATION", nullable = false)
    private long iteration;
    
    /** Creates a new instance of ScopesnapshotPK */
    public ScopesnapshotPK() {
    }

    /**
     * Creates a new instance of ScopesnapshotPK with the specified values.
     * @param iteration the iteration of the ScopesnapshotPK
     * @param varid the varid of the ScopesnapshotPK
     * @param id the id of the ScopesnapshotPK
     * @param bpid the bpid of the ScopesnapshotPK
     */
    public ScopesnapshotPK(long iteration, long varid, long id, String bpid) {
        this.iteration = iteration;
        this.varid = varid;
        this.id = id;
        this.bpid = bpid;
    }

    /**
     * Gets the bpid of this ScopesnapshotPK.
     * @return the bpid
     */
    public String getBpid() {
        return this.bpid;
    }

    /**
     * Sets the bpid of this ScopesnapshotPK to the specified value.
     * @param bpid the new bpid
     */
    public void setBpid(String bpid) {
        this.bpid = bpid;
    }

    /**
     * Gets the id of this ScopesnapshotPK.
     * @return the id
     */
    public long getId() {
        return this.id;
    }

    /**
     * Sets the id of this ScopesnapshotPK to the specified value.
     * @param id the new id
     */
    public void setId(long id) {
        this.id = id;
    }

    /**
     * Gets the varid of this ScopesnapshotPK.
     * @return the varid
     */
    public long getVarid() {
        return this.varid;
    }

    /**
     * Sets the varid of this ScopesnapshotPK to the specified value.
     * @param varid the new varid
     */
    public void setVarid(long varid) {
        this.varid = varid;
    }

    /**
     * Gets the iteration of this ScopesnapshotPK.
     * @return the iteration
     */
    public long getIteration() {
        return this.iteration;
    }

    /**
     * Sets the iteration of this ScopesnapshotPK to the specified value.
     * @param iteration the new iteration
     */
    public void setIteration(long iteration) {
        this.iteration = iteration;
    }

    /**
     * Returns a hash code value for the object.  This implementation computes 
     * a hash code value based on the id fields in this object.
     * @return a hash code value for this object.
     */
    @Override
    public int hashCode() {
        int hash = 0;
        hash += (int)iteration;
        hash += (int)varid;
        hash += (int)id;
        hash += (this.bpid != null ? this.bpid.hashCode() : 0);
        return hash;
    }

    /**
     * Determines whether another object is equal to this ScopesnapshotPK.  The result is 
     * <code>true</code> if and only if the argument is not null and is a ScopesnapshotPK object that 
     * has the same id field values as this object.
     * @param object the reference object with which to compare
     * @return <code>true</code> if this object is the same as the argument;
     * <code>false</code> otherwise.
     */
    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof ScopesnapshotPK)) {
            return false;
        }
        ScopesnapshotPK other = (ScopesnapshotPK)object;
        if (this.iteration != other.iteration) return false;
        if (this.varid != other.varid) return false;
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
        return "com.sun.jbi.cam.components.bpelse.db.ScopesnapshotPK[iteration=" + iteration + ", varid=" + varid + ", id=" + id + ", bpid=" + bpid + "]";
    }
    
}
