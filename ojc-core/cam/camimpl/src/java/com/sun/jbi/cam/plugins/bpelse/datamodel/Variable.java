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
 * @(#)Variable.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.datamodel;

import java.io.Serializable;
import java.util.Collection;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * Entity class Variable
 * 
 * @author nnahata
 */
/*
@Entity
@Table(name = "VARIABLE")
@NamedQueries( {
        @NamedQuery(name = "Variable.findByBpid", query = "SELECT v FROM Variable v WHERE v.variablePK.bpid = :bpid"),
        @NamedQuery(name = "Variable.findById", query = "SELECT v FROM Variable v WHERE v.variablePK.id = :id")
    })
    */
public class Variable implements Serializable {

    /**
     * EmbeddedId primary key field
     */
    @EmbeddedId
    protected VariablePK variablePK;

    @Lob
    @Column(name = "VALUE")
    private String value;

    @OneToMany(cascade = CascadeType.ALL, mappedBy = "variable")
    private Collection<Scopesnapshot> scopesnapshotCollection;

    @JoinColumn(name = "BPID", referencedColumnName = "ID", insertable = false, updatable = false)
    @ManyToOne
    private State state;
    
    /** Creates a new instance of Variable */
    public Variable() {
    }

    /**
     * Creates a new instance of Variable with the specified values.
     * @param variablePK the variablePK of the Variable
     */
    public Variable(VariablePK variablePK) {
        this.variablePK = variablePK;
    }

    /**
     * Creates a new instance of VariablePK with the specified values.
     * @param id the id of the VariablePK
     * @param bpid the bpid of the VariablePK
     */
    public Variable(long id, String bpid) {
        this.variablePK = new VariablePK(id, bpid);
    }

    /**
     * Gets the variablePK of this Variable.
     * @return the variablePK
     */
    public VariablePK getVariablePK() {
        return this.variablePK;
    }

    /**
     * Sets the variablePK of this Variable to the specified value.
     * @param variablePK the new variablePK
     */
    public void setVariablePK(VariablePK variablePK) {
        this.variablePK = variablePK;
    }

    /**
     * Gets the value of this Variable.
     * @return the value
     */
    public String getValue() {
        return this.value;
    }

    /**
     * Sets the value of this Variable to the specified value.
     * @param value the new value
     */
    public void setValue(String value) {
        this.value = value;
    }

    /**
     * Gets the scopesnapshotCollection of this Variable.
     * @return the scopesnapshotCollection
     */
    public Collection<Scopesnapshot> getScopesnapshotCollection() {
        return this.scopesnapshotCollection;
    }

    /**
     * Sets the scopesnapshotCollection of this Variable to the specified value.
     * @param scopesnapshotCollection the new scopesnapshotCollection
     */
    public void setScopesnapshotCollection(Collection<Scopesnapshot> scopesnapshotCollection) {
        this.scopesnapshotCollection = scopesnapshotCollection;
    }

    /**
     * Gets the state of this Variable.
     * @return the state
     */
    public State getState() {
        return this.state;
    }

    /**
     * Sets the state of this Variable to the specified value.
     * @param state the new state
     */
    public void setState(State state) {
        this.state = state;
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
