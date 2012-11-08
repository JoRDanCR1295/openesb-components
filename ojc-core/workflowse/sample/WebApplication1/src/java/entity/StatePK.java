/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package entity;

import java.io.Serializable;
import javax.persistence.Basic;
import javax.persistence.Column;
import javax.persistence.Embeddable;

/**
 *
 * @author mbhasin
 */
@Embeddable
public class StatePK implements Serializable {
    @Basic(optional = false)
    @Column(name = "instance_id")
    private int instanceId;
    @Basic(optional = false)
    @Column(name = "state_id")
    private String stateId;

    public StatePK() {
    }

    public StatePK(int instanceId, String stateId) {
        this.instanceId = instanceId;
        this.stateId = stateId;
    }

    public int getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(int instanceId) {
        this.instanceId = instanceId;
    }

    public String getStateId() {
        return stateId;
    }

    public void setStateId(String stateId) {
        this.stateId = stateId;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (int) instanceId;
        hash += (stateId != null ? stateId.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof StatePK)) {
            return false;
        }
        StatePK other = (StatePK) object;
        if (this.instanceId != other.instanceId) {
            return false;
        }
        if ((this.stateId == null && other.stateId != null) || (this.stateId != null && !this.stateId.equals(other.stateId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "entity.StatePK[instanceId=" + instanceId + ", stateId=" + stateId + "]";
    }

}
