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
@Table(name = "variable")
@NamedQueries({@NamedQuery(name = "Variable.findAll", query = "SELECT v FROM Variable v"), @NamedQuery(name = "Variable.findByVariableId", query = "SELECT v FROM Variable v WHERE v.variablePK.variableId = :variableId"), @NamedQuery(name = "Variable.findByInstanceId", query = "SELECT v FROM Variable v WHERE v.variablePK.instanceId = :instanceId"), @NamedQuery(name = "Variable.findByStateId", query = "SELECT v FROM Variable v WHERE v.variablePK.stateId = :stateId"), @NamedQuery(name = "Variable.findByValue", query = "SELECT v FROM Variable v WHERE v.value = :value")})
public class Variable implements Serializable {
    private static final long serialVersionUID = 1L;
    @EmbeddedId
    protected VariablePK variablePK;
    @Basic(optional = false)
    @Column(name = "value")
    private String value;

    public Variable() {
    }

    public Variable(VariablePK variablePK) {
        this.variablePK = variablePK;
    }

    public Variable(VariablePK variablePK, String value) {
        this.variablePK = variablePK;
        this.value = value;
    }

    public Variable(int variableId, String instanceId, String stateId) {
        this.variablePK = new VariablePK(variableId, instanceId, stateId);
    }

    public VariablePK getVariablePK() {
        return variablePK;
    }

    public void setVariablePK(VariablePK variablePK) {
        this.variablePK = variablePK;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (variablePK != null ? variablePK.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof Variable)) {
            return false;
        }
        Variable other = (Variable) object;
        if ((this.variablePK == null && other.variablePK != null) || (this.variablePK != null && !this.variablePK.equals(other.variablePK))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "entity.Variable[variablePK=" + variablePK + "]";
    }

}
