/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package selection;

import com.icesoft.faces.component.ext.HtmlSelectBooleanCheckbox;
import java.awt.event.ActionEvent;
import javax.faces.context.FacesContext;
import javax.faces.event.ValueChangeEvent;
import javax.faces.model.SelectItem;
import javax.servlet.http.HttpSession;

/**
 *
 * @author mbhasin
 */
public class TaskFilterActionsBean {

    public static final String STATUS_FILTER = "Show All Tasks";
    public static final String CLAIMED_TASKS = "Claimed Tasks";
    public static final String DB_FLAG_CLAIMED = "Claimed";

    public static final String UNCLAIMED_TASKS = "Unclaimed Tasks";
    public static final String DB_FLAG_UNCLAIMED = "Unclaimed";

    public static final String ESCALATED_TASKS = "Escalated Tasks";
    public static final String DB_FLAG_ESCALATED = "Escalated";

    public static final String ASSIGNED_TO_ME = "Assigned To Me";
    public static final String ASSIGNED_TO_GROUP = "Assigned To Group";
    public static final String EXPIRATION_FILTER = "Expiration Filter";
    public static final String EXPIRATION_FILTER1 = "Expiring in 1 Day";
    public static final String EXPIRATION_FILTER2 = "Expiring in 1 Week";
    public static final String USERASSIGNED_CHECKBX_DEFAULT = "true";
    public static final String GROUP_ASSIGNED_CHECKBX_DEFAULT = "true";
    public static final String STATUS = "Status";
    private String statusSelectedComponent;
    private String expirationSelectedComponent;
    private boolean userAssigned = Boolean.valueOf(USERASSIGNED_CHECKBX_DEFAULT);
    private boolean groupAssigned = Boolean.valueOf(GROUP_ASSIGNED_CHECKBX_DEFAULT);
    private static final SelectItem[] STATUS_FILTER_ITEMS = new SelectItem[]{
        new SelectItem(STATUS_FILTER),
        new SelectItem(CLAIMED_TASKS),
        new SelectItem(UNCLAIMED_TASKS),
        new SelectItem(ESCALATED_TASKS)
    };
    private static final SelectItem[] EXPIRATION_FILTER_ITEMS = new SelectItem[]{
        new SelectItem(EXPIRATION_FILTER),
        new SelectItem(EXPIRATION_FILTER1),
        new SelectItem(EXPIRATION_FILTER2)
    };

    public TaskFilterActionsBean() {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        HttpSession session = (HttpSession) facesContext.getExternalContext().getSession(true);
        session.setAttribute(TaskFilterActionsBean.ASSIGNED_TO_ME, USERASSIGNED_CHECKBX_DEFAULT);
        session.setAttribute(TaskFilterActionsBean.ASSIGNED_TO_GROUP, GROUP_ASSIGNED_CHECKBX_DEFAULT);
    }

    /**
     * Gets the option items for component types.
     *
     * @return array of component type items
     */
    public SelectItem[] getStatusFilterItems() {
        return STATUS_FILTER_ITEMS;
    }

    public SelectItem[] getExpirationFilterItems() {
        return EXPIRATION_FILTER_ITEMS;
    }

    /**
     * @return the statusSelectedComponent
     */
    public String getStatusSelectedComponent() {
        return statusSelectedComponent;
    }

    /**
     * @param statusSelectedComponent the statusSelectedComponent to set
     */
    public void setStatusSelectedComponent(String statusSelectedComponent) {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        HttpSession session = (HttpSession) facesContext.getExternalContext().getSession(true);

        if (statusSelectedComponent.equals(STATUS_FILTER)) {
            session.setAttribute(STATUS_FILTER, STATUS_FILTER);
        } else {
            session.setAttribute(STATUS_FILTER, statusSelectedComponent);
        }

        this.statusSelectedComponent = statusSelectedComponent;
    }

    public String getExpirationSelectedComponent() {
        return expirationSelectedComponent;
    }

    public void setExpirationSelectedComponent(String expirationSelectedComponent) {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        HttpSession session = (HttpSession) facesContext.getExternalContext().getSession(true);
        session.setAttribute(EXPIRATION_FILTER, expirationSelectedComponent);
        this.expirationSelectedComponent = expirationSelectedComponent;
    }

    public void effectChangeListener(ValueChangeEvent event) {
        HtmlSelectBooleanCheckbox checkbox = (HtmlSelectBooleanCheckbox) event.getComponent();
        if (checkbox != null) {
            String id = checkbox.getId();
            String newValue = event.getNewValue().toString();
            if (id != null) {
                FacesContext facesContext = FacesContext.getCurrentInstance();
                HttpSession session = (HttpSession) facesContext.getExternalContext().getSession(true);
                if (id.equals("userAssigned")) {
                    session.setAttribute(TaskFilterActionsBean.ASSIGNED_TO_ME, newValue);
                }
                if (id.equals("groupAssigned")) {
                    session.setAttribute(TaskFilterActionsBean.ASSIGNED_TO_GROUP, newValue);
                }
            }
        }
    }

    public boolean isGroupAssigned() {
        return groupAssigned;
    }

    public void setGroupAssigned(boolean groupAssigned) {
        this.groupAssigned = groupAssigned;
    }

    public boolean isUserAssigned() {
        return userAssigned;
    }

    public void setUserAssigned(boolean userAssigned) {
        this.userAssigned = userAssigned;
    }
}
