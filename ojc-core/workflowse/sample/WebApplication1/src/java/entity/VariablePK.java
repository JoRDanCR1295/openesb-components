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
public class VariablePK implements Serializable {
    @Basic(optional = false)
    @Column(name = "variable_id")
    private int variableId;
    @Basic(optional = false)
    @Column(name = "instance_id")
    private String instanceId;
    @Basic(optional = false)
    @Column(name = "state_id")
    private String stateId;

    public VariablePK() {
    }

    public VariablePK(int variableId, String instanceId, String stateId) {
        this.variableId = variableId;
        this.instanceId = instanceId;
        this.stateId = stateId;
    }

    public int getVariableId() {
        return variableId;
    }

    public void setVariableId(int variableId) {
        this.variableId = variableId;
    }

    public String getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(String instanceId) {
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
        hash += (int) variableId;
        hash += (instanceId != null ? instanceId.hashCode() : 0);
        hash += (stateId != null ? stateId.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof VariablePK)) {
            return false;
        }
        VariablePK other = (VariablePK) object;
        if (this.variableId != other.variableId) {
            return false;
        }
        if ((this.instanceId == null && other.instanceId != null) || (this.instanceId != null && !this.instanceId.equals(other.instanceId))) {
            return false;
        }
        if ((this.stateId == null && other.stateId != null) || (this.stateId != null && !this.stateId.equals(other.stateId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "entity.VariablePK[variableId=" + variableId + ", instanceId=" + instanceId + ", stateId=" + stateId + "]";
    }

}
