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
 * @(#)Scopesnapshot.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.datamodel;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.JoinColumns;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 * Entity class Scopesnapshot
 * 
 * @author nnahata
 */
/*
@Entity
@Table(name = "SCOPESNAPSHOT")
@NamedQueries( {
        @NamedQuery(name = "Scopesnapshot.findByBpid", query = "SELECT s FROM Scopesnapshot s WHERE s.scopesnapshotPK.bpid = :bpid"),
        @NamedQuery(name = "Scopesnapshot.findById", query = "SELECT s FROM Scopesnapshot s WHERE s.scopesnapshotPK.id = :id"),
        @NamedQuery(name = "Scopesnapshot.findByVarid", query = "SELECT s FROM Scopesnapshot s WHERE s.scopesnapshotPK.varid = :varid"),
        @NamedQuery(name = "Scopesnapshot.findByIteration", query = "SELECT s FROM Scopesnapshot s WHERE s.scopesnapshotPK.iteration = :iteration")
    })
    */
public class Scopesnapshot implements Serializable {

    /**
     * EmbeddedId primary key field
     */
    @EmbeddedId
    protected ScopesnapshotPK scopesnapshotPK;

    @Lob
    @Column(name = "VARVALUE")
    private String varvalue;

    @JoinColumn(name = "BPID", referencedColumnName = "ID", insertable = false, updatable = false)
    @ManyToOne
    private State state;

    @JoinColumns(value =  {
            @JoinColumn(name = "BPID", referencedColumnName = "BPID", insertable = false, updatable = false),
            @JoinColumn(name = "VARID", referencedColumnName = "ID", insertable = false, updatable = false)
        })
    @ManyToOne
    private Variable variable;
    
    /** Creates a new instance of Scopesnapshot */
    public Scopesnapshot() {
    }

    /**
     * Creates a new instance of Scopesnapshot with the specified values.
     * @param scopesnapshotPK the scopesnapshotPK of the Scopesnapshot
     */
    public Scopesnapshot(ScopesnapshotPK scopesnapshotPK) {
        this.scopesnapshotPK = scopesnapshotPK;
    }

    /**
     * Creates a new instance of ScopesnapshotPK with the specified values.
     * @param iteration the iteration of the ScopesnapshotPK
     * @param varid the varid of the ScopesnapshotPK
     * @param id the id of the ScopesnapshotPK
     * @param bpid the bpid of the ScopesnapshotPK
     */
    public Scopesnapshot(long iteration, long varid, long id, String bpid) {
        this.scopesnapshotPK = new ScopesnapshotPK(iteration, varid, id, bpid);
    }

    /**
     * Gets the scopesnapshotPK of this Scopesnapshot.
     * @return the scopesnapshotPK
     */
    public ScopesnapshotPK getScopesnapshotPK() {
        return this.scopesnapshotPK;
    }

    /**
     * Sets the scopesnapshotPK of this Scopesnapshot to the specified value.
     * @param scopesnapshotPK the new scopesnapshotPK
     */
    public void setScopesnapshotPK(ScopesnapshotPK scopesnapshotPK) {
        this.scopesnapshotPK = scopesnapshotPK;
    }

    /**
     * Gets the varvalue of this Scopesnapshot.
     * @return the varvalue
     */
    public String getVarvalue() {
        return this.varvalue;
    }

    /**
     * Sets the varvalue of this Scopesnapshot to the specified value.
     * @param varvalue the new varvalue
     */
    public void setVarvalue(String varvalue) {
        this.varvalue = varvalue;
    }

    /**
     * Gets the state of this Scopesnapshot.
     * @return the state
     */
    public State getState() {
        return this.state;
    }

    /**
     * Sets the state of this Scopesnapshot to the specified value.
     * @param state the new state
     */
    public void setState(State state) {
        this.state = state;
    }

    /**
     * Gets the variable of this Scopesnapshot.
     * @return the variable
     */
    public Variable getVariable() {
        return this.variable;
    }

    /**
     * Sets the variable of this Scopesnapshot to the specified value.
     * @param variable the new variable
     */
    public void setVariable(Variable variable) {
        this.variable = variable;
    }

    /**
     * Returns a hash code value for the object.  This implementation computes 
     * a hash code value based on the id fields in this object.
     * @return a hash code value for this object.
     */
    @Override
    public int hashCode() {
        int hash = 0;
        hash += (this.scopesnapshotPK != null ? this.scopesnapshotPK.hashCode() : 0);
        return hash;
    }

    /**
     * Determines whether another object is equal to this Scopesnapshot.  The result is 
     * <code>true</code> if and only if the argument is not null and is a Scopesnapshot object that 
     * has the same id field values as this object.
     * @param object the reference object with which to compare
     * @return <code>true</code> if this object is the same as the argument;
     * <code>false</code> otherwise.
     */
    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof Scopesnapshot)) {
            return false;
        }
        Scopesnapshot other = (Scopesnapshot)object;
        if (this.scopesnapshotPK != other.scopesnapshotPK && (this.scopesnapshotPK == null || !this.scopesnapshotPK.equals(other.scopesnapshotPK))) return false;
        return true;
    }

    /**
     * Returns a string representation of the object.  This implementation constructs 
     * that representation based on the id fields.
     * @return a string representation of the object.
     */
    @Override
    public String toString() {
        return "com.sun.jbi.cam.components.bpelse.db.Scopesnapshot[scopesnapshotPK=" + scopesnapshotPK + "]";
    }
    
}
