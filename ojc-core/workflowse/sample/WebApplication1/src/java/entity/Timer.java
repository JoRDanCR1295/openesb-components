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
@Table(name = "timer")
@NamedQueries({@NamedQuery(name = "Timer.findAll", query = "SELECT t FROM Timer t"), @NamedQuery(name = "Timer.findByTaskId", query = "SELECT t FROM Timer t WHERE t.taskId = :taskId"), @NamedQuery(name = "Timer.findByXpath", query = "SELECT t FROM Timer t WHERE t.xpath = :xpath"), @NamedQuery(name = "Timer.findByDuedate", query = "SELECT t FROM Timer t WHERE t.duedate = :duedate"), @NamedQuery(name = "Timer.findByStatus", query = "SELECT t FROM Timer t WHERE t.status = :status")})
public class Timer implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Basic(optional = false)
    @Column(name = "task_id")
    private Integer taskId;
    @Basic(optional = false)
    @Column(name = "xpath")
    private String xpath;
    @Basic(optional = false)
    @Column(name = "duedate")
    private String duedate;
    @Basic(optional = false)
    @Column(name = "status")
    private String status;

    public Timer() {
    }

    public Timer(Integer taskId) {
        this.taskId = taskId;
    }

    public Timer(Integer taskId, String xpath, String duedate, String status) {
        this.taskId = taskId;
        this.xpath = xpath;
        this.duedate = duedate;
        this.status = status;
    }

    public Integer getTaskId() {
        return taskId;
    }

    public void setTaskId(Integer taskId) {
        this.taskId = taskId;
    }

    public String getXpath() {
        return xpath;
    }

    public void setXpath(String xpath) {
        this.xpath = xpath;
    }

    public String getDuedate() {
        return duedate;
    }

    public void setDuedate(String duedate) {
        this.duedate = duedate;
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
        hash += (taskId != null ? taskId.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof Timer)) {
            return false;
        }
        Timer other = (Timer) object;
        if ((this.taskId == null && other.taskId != null) || (this.taskId != null && !this.taskId.equals(other.taskId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "entity.Timer[taskId=" + taskId + "]";
    }

}
