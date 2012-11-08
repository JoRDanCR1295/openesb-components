/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package entity;

import java.io.Serializable;
import javax.persistence.Basic;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 *
 * @author mbhasin
 */
@Entity
@Table(name = "escalation_assignment")
@NamedQueries({@NamedQuery(name = "EscalationAssignment.findAll", query = "SELECT e FROM EscalationAssignment e"), @NamedQuery(name = "EscalationAssignment.findById", query = "SELECT e FROM EscalationAssignment e WHERE e.id = :id"), @NamedQuery(name = "EscalationAssignment.findByTaskId", query = "SELECT e FROM EscalationAssignment e WHERE e.taskId = :taskId"), @NamedQuery(name = "EscalationAssignment.findByUserId", query = "SELECT e FROM EscalationAssignment e WHERE e.userId = :userId"), @NamedQuery(name = "EscalationAssignment.findByGroupId", query = "SELECT e FROM EscalationAssignment e WHERE e.groupId = :groupId")})
public class EscalationAssignment implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Basic(optional = false)
    @Column(name = "id")
    private Integer id;
    @Basic(optional = false)
    @Column(name = "task_id")
    private String taskId;
    @Column(name = "user_id")
    private String userId;
    @Column(name = "group_id")
    private String groupId;

    public EscalationAssignment() {
    }

    public EscalationAssignment(Integer id) {
        this.id = id;
    }

    public EscalationAssignment(Integer id, String taskId) {
        this.id = id;
        this.taskId = taskId;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getTaskId() {
        return taskId;
    }

    public void setTaskId(String taskId) {
        this.taskId = taskId;
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getGroupId() {
        return groupId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (id != null ? id.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof EscalationAssignment)) {
            return false;
        }
        EscalationAssignment other = (EscalationAssignment) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "entity.EscalationAssignment[id=" + id + "]";
    }

}
