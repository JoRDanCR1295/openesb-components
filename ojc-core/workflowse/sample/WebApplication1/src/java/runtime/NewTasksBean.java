/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package runtime;

import com.icesoft.faces.component.ext.RowSelectorEvent;
import entity.Instance;
import java.util.ArrayList;
import java.util.List;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpSession;
import persistence.PersistenceManager;
import selection.TaskFilterActionsBean;

/**
 *
 * @author mbhasin
 */
public class NewTasksBean {

    private ArrayList newTasks = new ArrayList();
    private ArrayList selectedTasks = new ArrayList();

    public NewTasksBean() {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        HttpSession session = (HttpSession) facesContext.getExternalContext().getSession(true);
        session.setAttribute(TaskFilterActionsBean.STATUS_FILTER, TaskFilterActionsBean.STATUS_FILTER);
    }

    public ArrayList getTaskList() {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        HttpSession session = (HttpSession) facesContext.getExternalContext().getSession(true);
        String userId = (String) session.getAttribute(ButtonsAndLinks.USER);
        String groupId = (String) session.getAttribute(ButtonsAndLinks.GROUP);

        String taskStatusFilter = (String) session.getAttribute(TaskFilterActionsBean.STATUS_FILTER);
        String selfTasksOnlyFilter = (String) session.getAttribute(TaskFilterActionsBean.ASSIGNED_TO_ME);
        boolean bSelfTasks = selfTasksOnlyFilter != null && selfTasksOnlyFilter.equals("true") ? true : false;

        String groupTasksOnlyFilter = (String) session.getAttribute(TaskFilterActionsBean.ASSIGNED_TO_GROUP);
        boolean bGroupTasks = groupTasksOnlyFilter != null && groupTasksOnlyFilter.equals("true") ? true : false;

        ArrayList resultList = null;
        if (bSelfTasks && bGroupTasks) {
            resultList = getTasksForStatus(userId, groupId, taskStatusFilter);
        } else if (bSelfTasks && !bGroupTasks) {
            resultList = getTasksForStatus(userId, null, taskStatusFilter);
        } else if (!bSelfTasks && bGroupTasks) {
            resultList = getTasksForStatus(null, groupId, taskStatusFilter);
        }
        return resultList;
    }

    public void setNewTasks(ArrayList newTasks) {
        this.newTasks = newTasks;
    }

    /**
     * SelectionListener bound to the ice:rowSelector component.  Called
     * when a row is selected in the UI.
     *
     * @param event from the ice:rowSelector component
     */
    public void rowSelectionListener(RowSelectorEvent event) {
        // clear our list, so that we can build a new one
        newTasks.clear();

        // build the new selected list
        NewTaskInstance task;
        for (int i = 0, max = newTasks.size(); i < max; i++) {
            task = (NewTaskInstance) newTasks.get(i);
            if (task.isSelected()) {
                selectedTasks.add(task);
            }
        }
    }

    private ArrayList getTasksForStatus(String userId, String groupId, String taskStatusFilter) {
        PersistenceManager manager = new PersistenceManager();
        List statusList = new ArrayList();
        if (taskStatusFilter.equals(TaskFilterActionsBean.STATUS_FILTER)) {
            statusList = getStatusListForAllTasks();
        } else {
            statusList = new ArrayList();
            if (taskStatusFilter.equals(TaskFilterActionsBean.CLAIMED_TASKS)) {
                statusList.add(TaskFilterActionsBean.DB_FLAG_CLAIMED);
            }
            if (taskStatusFilter.equals(TaskFilterActionsBean.UNCLAIMED_TASKS)) {
                statusList.add(TaskFilterActionsBean.DB_FLAG_UNCLAIMED);
            }
            if (taskStatusFilter.equals(TaskFilterActionsBean.ESCALATED_TASKS)) {
                statusList.add(TaskFilterActionsBean.DB_FLAG_ESCALATED);
            }
        }
        return manager.getTaskList(userId, groupId, statusList);
    }

    private ArrayList getStatusListForAllTasks() {
        ArrayList statusList = new ArrayList();
        statusList.add(TaskFilterActionsBean.DB_FLAG_CLAIMED);
        statusList.add(TaskFilterActionsBean.DB_FLAG_UNCLAIMED);
        statusList.add(TaskFilterActionsBean.DB_FLAG_ESCALATED);
        return statusList;
    }
}
