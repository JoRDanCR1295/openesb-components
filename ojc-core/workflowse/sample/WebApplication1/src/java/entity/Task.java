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
@Table(name = "task")
@NamedQueries({@NamedQuery(name = "Task.findAll", query = "SELECT t FROM Task t"), @NamedQuery(name = "Task.findByTaskId", query = "SELECT t FROM Task t WHERE t.taskId = :taskId"), @NamedQuery(name = "Task.findByTaskdefId", query = "SELECT t FROM Task t WHERE t.taskdefId = :taskdefId"), @NamedQuery(name = "Task.findByTitle", query = "SELECT t FROM Task t WHERE t.title = :title"), @NamedQuery(name = "Task.findByPriority", query = "SELECT t FROM Task t WHERE t.priority = :priority"), @NamedQuery(name = "Task.findByEscalationDeadline", query = "SELECT t FROM Task t WHERE t.escalationDeadline = :escalationDeadline"), @NamedQuery(name = "Task.findByEscalationDuration", query = "SELECT t FROM Task t WHERE t.escalationDuration = :escalationDuration"), @NamedQuery(name = "Task.findByCreated", query = "SELECT t FROM Task t WHERE t.created = :created"), @NamedQuery(name = "Task.findByCompleted", query = "SELECT t FROM Task t WHERE t.completed = :completed"), @NamedQuery(name = "Task.findByOutcomeCode", query = "SELECT t FROM Task t WHERE t.outcomeCode = :outcomeCode")})
public class Task implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Basic(optional = false)
    @Column(name = "task_id")
    private Integer taskId;
    @Basic(optional = false)
    @Column(name = "taskdef_id")
    private String taskdefId;
    @Basic(optional = false)
    @Column(name = "title")
    private String title;
    @Basic(optional = false)
    @Column(name = "priority")
    private String priority;
    @Basic(optional = false)
    @Column(name = "escalation_deadline")
    private String escalationDeadline;
    @Basic(optional = false)
    @Column(name = "escalation_duration")
    private String escalationDuration;
    @Basic(optional = false)
    @Column(name = "created")
    @Temporal(TemporalType.TIMESTAMP)
    private Date created;
    @Basic(optional = false)
    @Column(name = "completed")
    private String completed;
    @Basic(optional = false)
    @Column(name = "outcome_code")
    private String outcomeCode;

    public Task() {
    }

    public Task(Integer taskId) {
        this.taskId = taskId;
    }

    public Task(Integer taskId, String taskdefId, String title, String priority, String escalationDeadline, String escalationDuration, Date created, String completed, String outcomeCode) {
        this.taskId = taskId;
        this.taskdefId = taskdefId;
        this.title = title;
        this.priority = priority;
        this.escalationDeadline = escalationDeadline;
        this.escalationDuration = escalationDuration;
        this.created = created;
        this.completed = completed;
        this.outcomeCode = outcomeCode;
    }

    public Integer getTaskId() {
        return taskId;
    }

    public void setTaskId(Integer taskId) {
        this.taskId = taskId;
    }

    public String getTaskdefId() {
        return taskdefId;
    }

    public void setTaskdefId(String taskdefId) {
        this.taskdefId = taskdefId;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getPriority() {
        return priority;
    }

    public void setPriority(String priority) {
        this.priority = priority;
    }

    public String getEscalationDeadline() {
        return escalationDeadline;
    }

    public void setEscalationDeadline(String escalationDeadline) {
        this.escalationDeadline = escalationDeadline;
    }

    public String getEscalationDuration() {
        return escalationDuration;
    }

    public void setEscalationDuration(String escalationDuration) {
        this.escalationDuration = escalationDuration;
    }

    public Date getCreated() {
        return created;
    }

    public void setCreated(Date created) {
        this.created = created;
    }

    public String getCompleted() {
        return completed;
    }

    public void setCompleted(String completed) {
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
        hash += (taskId != null ? taskId.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof Task)) {
            return false;
        }
        Task other = (Task) object;
        if ((this.taskId == null && other.taskId != null) || (this.taskId != null && !this.taskId.equals(other.taskId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "entity.Task[taskId=" + taskId + "]";
    }

}
