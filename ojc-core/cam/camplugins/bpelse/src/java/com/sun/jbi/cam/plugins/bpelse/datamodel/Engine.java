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
 * @(#)Engine.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.datamodel;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 * Entity class Engine
 * 
 * @author nnahata
 */
/*
@Entity
@Table(name = "ENGINE")
@NamedQueries( {
        @NamedQuery(name = "Engine.findById", query = "SELECT e FROM Engine e WHERE e.id = :id"),
        @NamedQuery(name = "Engine.findByLocation", query = "SELECT e FROM Engine e WHERE e.location = :location"),
        @NamedQuery(name = "Engine.findByExpiration", query = "SELECT e FROM Engine e WHERE e.expiration = :expiration")
    })
    */
public class Engine implements Serializable {

    @Id
    @Column(name = "ID", nullable = false)
    private String id;

    @Column(name = "LOCATION")
    private String location;

    @Column(name = "EXPIRATION")
    private Long expiration;
    
    /** Creates a new instance of Engine */
    public Engine() {
    }

    /**
     * Creates a new instance of Engine with the specified values.
     * @param id the id of the Engine
     */
    public Engine(String id) {
        this.id = id;
    }

    /**
     * Gets the id of this Engine.
     * @return the id
     */
    public String getId() {
        return this.id;
    }

    /**
     * Sets the id of this Engine to the specified value.
     * @param id the new id
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * Gets the location of this Engine.
     * @return the location
     */
    public String getLocation() {
        return this.location;
    }

    /**
     * Sets the location of this Engine to the specified value.
     * @param location the new location
     */
    public void setLocation(String location) {
        this.location = location;
    }

    /**
     * Gets the expiration of this Engine.
     * @return the expiration
     */
    public Long getExpiration() {
        return this.expiration;
    }

    /**
     * Sets the expiration of this Engine to the specified value.
     * @param expiration the new expiration
     */
    public void setExpiration(Long expiration) {
        this.expiration = expiration;
    }

    /**
     * Returns a hash code value for the object.  This implementation computes 
     * a hash code value based on the id fields in this object.
     * @return a hash code value for this object.
     */
    @Override
    public int hashCode() {
        int hash = 0;
        hash += (this.id != null ? this.id.hashCode() : 0);
        return hash;
    }

    /**
     * Determines whether another object is equal to this Engine.  The result is 
     * <code>true</code> if and only if the argument is not null and is a Engine object that 
     * has the same id field values as this object.
     * @param object the reference object with which to compare
     * @return <code>true</code> if this object is the same as the argument;
     * <code>false</code> otherwise.
     */
    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof Engine)) {
            return false;
        }
        Engine other = (Engine)object;
        if (this.id != other.id && (this.id == null || !this.id.equals(other.id))) return false;
        return true;
    }

    /**
     * Returns a string representation of the object.  This implementation constructs 
     * that representation based on the id fields.
     * @return a string representation of the object.
     */
    @Override
    public String toString() {
        return "com.sun.jbi.cam.components.bpelse.db.Engine[id=" + id + "]";
    }
    
}
