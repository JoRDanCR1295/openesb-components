/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package entity;

import java.io.Serializable;
import javax.persistence.Basic;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 *
 * @author mbhasin
 */
@Entity
@Table(name = "state")
@NamedQueries({@NamedQuery(name = "State.findAll", query = "SELECT s FROM State s"), @NamedQuery(name = "State.findByInstanceId", query = "SELECT s FROM State s WHERE s.statePK.instanceId = :instanceId"), @NamedQuery(name = "State.findByStateId", query = "SELECT s FROM State s WHERE s.statePK.stateId = :stateId"), @NamedQuery(name = "State.findByUserId", query = "SELECT s FROM State s WHERE s.userId = :userId"), @NamedQuery(name = "State.findByStatus", query = "SELECT s FROM State s WHERE s.status = :status")})
public class State implements Serializable {
    private static final long serialVersionUID = 1L;
    @EmbeddedId
    protected StatePK statePK;
    @Column(name = "user_id")
    private String userId;
    @Basic(optional = false)
    @Column(name = "status")
    private String status;

    public State() {
    }

    public State(StatePK statePK) {
        this.statePK = statePK;
    }

    public State(StatePK statePK, String status) {
        this.statePK = statePK;
        this.status = status;
    }

    public State(int instanceId, String stateId) {
        this.statePK = new StatePK(instanceId, stateId);
    }

    public StatePK getStatePK() {
        return statePK;
    }

    public void setStatePK(StatePK statePK) {
        this.statePK = statePK;
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (statePK != null ? statePK.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof State)) {
            return false;
        }
        State other = (State) object;
        if ((this.statePK == null && other.statePK != null) || (this.statePK != null && !this.statePK.equals(other.statePK))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "entity.State[statePK=" + statePK + "]";
    }

}
