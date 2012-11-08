package runtime;

import com.icesoft.faces.component.ext.HtmlCommandLink;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.servlet.http.HttpSession;
import selection.TaskFilterActionsBean;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
/**
 *
 * @author mbhasin
 */
public class ButtonsAndLinks {

    public static String USER = "user";
    public static String GROUP = "group";

    public void submitButtonListener(ActionEvent event) {

        UIComponent comp = event.getComponent();
        String userId = null, groupId = null;

        if (comp instanceof HtmlCommandLink) {
            HtmlCommandLink commandLink = (HtmlCommandLink) event.getComponent();
            String value = (String) commandLink.getValue();

            if (value.equals("Login")) {
                String id = commandLink.getId();

                int index = id.indexOf("-");

                if (index != -1) {
                    userId = id.substring(0, id.indexOf("-"));
                    groupId = id.substring(id.indexOf("-") + 1);
                } else {
                    userId = id;
                    groupId = null;
                }

                if (userId != null) {
                    FacesContext facesContext = FacesContext.getCurrentInstance();
                    HttpSession session = (HttpSession) facesContext.getExternalContext().getSession(true);
                    session.setAttribute(USER, userId);
                    session.setAttribute(GROUP, groupId);
                }
            } else if (value.equals("Logout")) {
                FacesContext facesContext = FacesContext.getCurrentInstance();
                HttpSession session = (HttpSession) facesContext.getExternalContext().getSession(true);
                session.setAttribute(USER, null);
                session.setAttribute(GROUP, null);
                session.setAttribute(TaskFilterActionsBean.ASSIGNED_TO_ME, TaskFilterActionsBean.USERASSIGNED_CHECKBX_DEFAULT);
                session.setAttribute(TaskFilterActionsBean.ASSIGNED_TO_GROUP, TaskFilterActionsBean.GROUP_ASSIGNED_CHECKBX_DEFAULT);
                session.setAttribute(TaskFilterActionsBean.STATUS_FILTER, TaskFilterActionsBean.STATUS_FILTER);
            }
        }
    }

    public String getLoggedUser() {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        HttpSession session = (HttpSession) facesContext.getExternalContext().getSession(true);
        return (String) session.getAttribute(ButtonsAndLinks.USER);
    }
}
