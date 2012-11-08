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
 * @(#)State.java 
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
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * Entity class State
 * 
 * @author nnahata
 */
/*
@Entity
@Table(name = "STATE")
@NamedQueries( {
        @NamedQuery(name = "State.findById", query = "SELECT s FROM State s WHERE s.id = :id"),
        @NamedQuery(name = "State.findByBpelid", query = "SELECT s FROM State s WHERE s.bpelid = :bpelid"),
        @NamedQuery(name = "State.findByEngineid", query = "SELECT s FROM State s WHERE s.engineid = :engineid"),
        @NamedQuery(name = "State.findByStatus", query = "SELECT s FROM State s WHERE s.status = :status")
    })
    */
public class State implements Serializable {

    @Id
    @Column(name = "STATEID", nullable = false)
    private String stateid;

    @Column(name = "BPELID")
    private String bpelid;

    @Column(name = "ENGINEID")
    private String engineid;

    @Column(name = "STATUS")
    private String status;

    @OneToMany(cascade = CascadeType.ALL, mappedBy = "state")
    private Collection<Scopesnapshot> scopesnapshotCollection;

    @OneToMany(cascade = CascadeType.ALL, mappedBy = "state")
    private Collection<Variable> variableCollection;
    
    /** Creates a new instance of State */
    public State() {
    }

      /** Creates a new instance of State */
    public State(String stateid, String bpelid, String engineid, String status) {
        this.stateid = stateid;
        this.bpelid = bpelid;
        this.engineid = engineid;
        this.status = status;
    }
    
    /**
     * Creates a new instance of State with the specified values.
     * @param id the id of the State
     */
    public State(String id) {
        this.stateid = id;
    }

    /**
     * Gets the id of this State.
     * @return the id
     */
    public String getId() {
        return this.stateid;
    }

    /**
     * Sets the id of this State to the specified value.
     * @param id the new id
     */
    public void setId(String id) {
        this.stateid = id;
    }

    /**
     * Gets the bpelid of this State.
     * @return the bpelid
     */
    public String getBpelid() {
        return this.bpelid;
    }

    /**
     * Sets the bpelid of this State to the specified value.
     * @param bpelid the new bpelid
     */
    public void setBpelid(String bpelid) {
        this.bpelid = bpelid;
    }

    /**
     * Gets the engineid of this State.
     * @return the engineid
     */
    public String getEngineid() {
        return this.engineid;
    }

    /**
     * Sets the engineid of this State to the specified value.
     * @param engineid the new engineid
     */
    public void setEngineid(String engineid) {
        this.engineid = engineid;
    }

    /**
     * Gets the status of this State.
     * @return the status
     */
    public String getStatus() {
        return this.status;
    }

    /**
     * Sets the status of this State to the specified value.
     * @param status the new status
     */
    public void setStatus(String status) {
        this.status = status;
    }

    /**
     * Gets the scopesnapshotCollection of this State.
     * @return the scopesnapshotCollection
     */
    public Collection<Scopesnapshot> getScopesnapshotCollection() {
        return this.scopesnapshotCollection;
    }

    /**
     * Sets the scopesnapshotCollection of this State to the specified value.
     * @param scopesnapshotCollection the new scopesnapshotCollection
     */
    public void setScopesnapshotCollection(Collection<Scopesnapshot> scopesnapshotCollection) {
        this.scopesnapshotCollection = scopesnapshotCollection;
    }

    /**
     * Gets the variableCollection of this State.
     * @return the variableCollection
     */
    public Collection<Variable> getVariableCollection() {
        return this.variableCollection;
    }

    /**
     * Sets the variableCollection of this State to the specified value.
     * @param variableCollection the new variableCollection
     */
    public void setVariableCollection(Collection<Variable> variableCollection) {
        this.variableCollection = variableCollection;
    }

    /**
     * Returns a hash code value for the object.  This implementation computes 
     * a hash code value based on the id fields in this object.
     * @return a hash code value for this object.
     */
    @Override
    public int hashCode() {
        int hash = 0;
        hash += (this.stateid != null ? this.stateid.hashCode() : 0);
        return hash;
    }

    /**
     * Determines whether another object is equal to this State.  The result is 
     * <code>true</code> if and only if the argument is not null and is a State object that 
     * has the same id field values as this object.
     * @param object the reference object with which to compare
     * @return <code>true</code> if this object is the same as the argument;
     * <code>false</code> otherwise.
     */
    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof State)) {
            return false;
        }
        State other = (State)object;
        if (this.stateid != other.stateid && (this.stateid == null || 
                !this.stateid.equals(other.stateid))) return false;
        return true;
    }

    /**
     * Returns a string representation of the object.  This implementation constructs 
     * that representation based on the id fields.
     * @return a string representation of the object.
     */
    @Override
    public String toString() {
        return "com.sun.jbi.cam.components.bpelse.db.State[id=" + stateid + "]";
    }
    
}
