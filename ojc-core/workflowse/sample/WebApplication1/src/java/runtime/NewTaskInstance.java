/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package runtime;

import entity.Instance;

/**
 *
 * @author mbhasin
 */
public class NewTaskInstance {

    private Instance taskInstance;
    private boolean selected;
    private int taskInstanceId;
    private String title = "New Task";
    private int priority = 5;
    private String escalationDate = "2009-02-31T10:47";
    private String escalationDutation = "P0Y0M0DT0H3M0S";
    private String createdDate = "2009-01-31T10:47";
    private String status;

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public NewTaskInstance(Instance taskInstance) {
        this.taskInstance = taskInstance;
    }

    /**
     * @return the taskInstance
     */
    public Instance getTaskInstance() {
        return taskInstance;
    }

    /**
     * @param taskInstance the taskInstance to set
     */
    public void setTaskInstance(Instance taskInstance) {
        this.taskInstance = taskInstance;
    }

    /**
     * @return the selected
     */
    public boolean isSelected() {
        return selected;
    }

    /**
     * @param selected the selected to set
     */
    public void setSelected(boolean selected) {
        this.selected = selected;
    }

    /**
     * @return the taskInstanceId
     */
    public int getTaskInstanceId() {
        return taskInstance.getInstanceId();
    }

    /**
     * @param taskInstanceId the taskInstanceId to set
     */
    public void setTaskInstanceId(int taskInstanceId) {
        this.taskInstanceId = taskInstanceId;
    }

    /**
     * @return the title
     */
    public String getTitle() {
        return title;
    }

    /**
     * @param title the title to set
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * @return the priority
     */
    public int getPriority() {
        return priority;
    }

    /**
     * @param priority the priority to set
     */
    public void setPriority(int priority) {
        this.priority = priority;
    }

    /**
     * @return the escalationDate
     */
    public String getEscalationDate() {
        return escalationDate;
    }

    /**
     * @param escalationDate the escalationDate to set
     */
    public void setEscalationDate(String escalationDate) {
        this.escalationDate = escalationDate;
    }

    /**
     * @return the escalationDutation
     */
    public String getEscalationDutation() {
        return escalationDutation;
    }

    /**
     * @param escalationDutation the escalationDutation to set
     */
    public void setEscalationDutation(String escalationDutation) {
        this.escalationDutation = escalationDutation;
    }

    /**
     * @return the createdDate
     */
    public String getCreatedDate() {
        return createdDate;
    }

    /**
     * @param createdDate the createdDate to set
     */
    public void setCreatedDate(String createdDate) {
        this.createdDate = createdDate;
    }
}
