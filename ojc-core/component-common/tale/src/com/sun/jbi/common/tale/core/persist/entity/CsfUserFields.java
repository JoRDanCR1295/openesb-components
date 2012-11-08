/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 *
 * @author echou
 */
@Entity
@Table(name = "CSF_USER_FIELDS")
@NamedQueries({@NamedQuery(name = "CsfUserFields.findByMesgId", query = "SELECT c FROM CsfUserFields c WHERE c.csfUserFieldsPK.mesgId = :mesgId"), @NamedQuery(name = "CsfUserFields.findBySequenceId", query = "SELECT c FROM CsfUserFields c WHERE c.csfUserFieldsPK.sequenceId = :sequenceId"), @NamedQuery(name = "CsfUserFields.findByFieldName", query = "SELECT c FROM CsfUserFields c WHERE c.fieldName = :fieldName"), @NamedQuery(name = "CsfUserFields.findByFieldValue", query = "SELECT c FROM CsfUserFields c WHERE c.fieldValue = :fieldValue")})
public class CsfUserFields implements Serializable {
    private static final long serialVersionUID = 1L;
    @EmbeddedId
    protected CsfUserFieldsPK csfUserFieldsPK;
    @Column(name = "FIELD_NAME")
    private String fieldName;
    @Column(name = "FIELD_VALUE")
    private String fieldValue;

    /* relationship */
    @ManyToOne(cascade = { CascadeType.PERSIST } )
    @JoinColumn(name="MESG_ID", referencedColumnName="MESG_ID", insertable=false, updatable=false)
    private CsfCmeLog csfCmeLog;
    
    public CsfUserFields() {
    }

    public CsfUserFields(CsfUserFieldsPK csfUserFieldsPK) {
        this.csfUserFieldsPK = csfUserFieldsPK;
    }

    public CsfUserFields(String mesgId, short sequenceId) {
        this.csfUserFieldsPK = new CsfUserFieldsPK(mesgId, sequenceId);
    }
    
    public CsfUserFields(CsfCmeLog csfCmeLog, short sequenceId) {
        this.csfCmeLog = csfCmeLog;
        this.csfUserFieldsPK = new CsfUserFieldsPK(csfCmeLog.getMesgId(), sequenceId);
    }

    public CsfUserFieldsPK getCsfUserFieldsPK() {
        return csfUserFieldsPK;
    }

    public void setCsfUserFieldsPK(CsfUserFieldsPK csfUserFieldsPK) {
        this.csfUserFieldsPK = csfUserFieldsPK;
    }

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public String getFieldValue() {
        return fieldValue;
    }

    public void setFieldValue(String fieldValue) {
        this.fieldValue = fieldValue;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (csfUserFieldsPK != null ? csfUserFieldsPK.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfUserFields)) {
            return false;
        }
        CsfUserFields other = (CsfUserFields) object;
        if ((this.csfUserFieldsPK == null && other.csfUserFieldsPK != null) || (this.csfUserFieldsPK != null && !this.csfUserFieldsPK.equals(other.csfUserFieldsPK))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfUserFields[csfUserFieldsPK=" + csfUserFieldsPK + "]";
    }

    /**
     * @return the csfCmeLog
     */
    public CsfCmeLog getCsfCmeLog() {
        return csfCmeLog;
    }

    /**
     * @param csfCmeLog the csfCmeLog to set
     */
    public void setCsfCmeLog(CsfCmeLog csfCmeLog) {
        this.csfCmeLog = csfCmeLog;
    }

}
