/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package entity;

import java.io.Serializable;
import java.util.Date;
import javax.persistence.Basic;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

/**
 *
 * @author mbhasin
 */
@Entity
@Table(name = "instance")
@NamedQueries({@NamedQuery(name = "Instance.findAll", query = "SELECT i FROM Instance i"), 
                @NamedQuery(name = "Instance.findByInstanceId", query = "SELECT i FROM Instance i WHERE i.instanceId = :instanceId"), 
                @NamedQuery(name = "Instance.findByTaskId", query = "SELECT i FROM Instance i WHERE i.taskId = :taskId"),
                @NamedQuery(name = "Instance.findByMsgexId", query = "SELECT i FROM Instance i WHERE i.msgexId = :msgexId"),
                @NamedQuery(name = "Instance.findByInputData", query = "SELECT i FROM Instance i WHERE i.inputData = :inputData"),
                @NamedQuery(name = "Instance.findByCreated", query = "SELECT i FROM Instance i WHERE i.created = :created"),
                @NamedQuery(name = "Instance.findByCompleted", query = "SELECT i FROM Instance i WHERE i.completed = :completed"),
                @NamedQuery(name = "Instance.findByOutcomeCode", query = "SELECT i FROM Instance i WHERE i.outcomeCode = :outcomeCode")})
public class Instance implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Basic(optional = false)
    @Column(name = "instance_id")
    private Integer instanceId;
    @Basic(optional = false)
    @Column(name = "task_id")
    private String taskId;
    @Basic(optional = false)
    @Column(name = "msgex_id")
    private String msgexId;
    @Basic(optional = false)
    @Column(name = "input_data")
    private String inputData;
    @Basic(optional = false)
    @Column(name = "created")
    @Temporal(TemporalType.TIMESTAMP)
    private Date created;
    @Basic(optional = false)
    @Column(name = "completed")
    @Temporal(TemporalType.TIMESTAMP)
    private Date completed;
    @Basic(optional = false)
    @Column(name = "outcome_code")
    private String outcomeCode;

    public Instance() {
    }

    public Instance(Integer instanceId) {
        this.instanceId = instanceId;
    }

    public Instance(Integer instanceId, String taskId, String msgexId, String inputData, Date created, Date completed, String outcomeCode) {
        this.instanceId = instanceId;
        this.taskId = taskId;
        this.msgexId = msgexId;
        this.inputData = inputData;
        this.created = created;
        this.completed = completed;
        this.outcomeCode = outcomeCode;
    }

    public Integer getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(Integer instanceId) {
        this.instanceId = instanceId;
    }

    public String getTaskId() {
        return taskId;
    }

    public void setTaskId(String taskId) {
        this.taskId = taskId;
    }

    public String getMsgexId() {
        return msgexId;
    }

    public void setMsgexId(String msgexId) {
        this.msgexId = msgexId;
    }

    public String getInputData() {
        return inputData;
    }

    public void setInputData(String inputData) {
        this.inputData = inputData;
    }

    public Date getCreated() {
        return created;
    }

    public void setCreated(Date created) {
        this.created = created;
    }

    public Date getCompleted() {
        return completed;
    }

    public void setCompleted(Date completed) {
        this.completed = completed;
    }

    public String getOutcomeCode() {
        return outcomeCode;
    }

    public void setOutcomeCode(String outcomeCode) {
        this.outcomeCode = outcomeCode;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (instanceId != null ? instanceId.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof Instance)) {
            return false;
        }
        Instance other = (Instance) object;
        if ((this.instanceId == null && other.instanceId != null) || (this.instanceId != null && !this.instanceId.equals(other.instanceId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "entity.Instance[instanceId=" + instanceId + "]";
    }

}
