/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package persistence;

import entity.Instance;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import runtime.NewTaskInstance;

/**
 *
 * @author mbhasin
 */
public class PersistenceManager {

    public ArrayList getTaskList(String userId, String groupId, List<String> statusList) {

        String assignmentQuery = getAssignmentQueryPart(userId, groupId);
        String statusQuery = getTaskStatusQueryPart(statusList);

        String strQuery = "SELECT distinct i.instance_id, t.title, t.priority, t.escalation_deadline, t.escalation_duration, t.created, s.status" +
                " FROM task t, instance i, assignment a, state s" +
                " where i.task_id = t.task_id" +
                " and i.instance_id = s.instance_id" +
                " and t.task_id = a.task_id" +
                " and (" + assignmentQuery + ")" +
                " and (" + statusQuery + ") " +
                " and s.current = 'Y'";

        System.out.print(strQuery);
        DBConnection dbConn = new DBConnection();
        Connection conn = dbConn.getDBConnection();
        ResultSet rs = null;
        Statement stmt = null;
        ArrayList list = new ArrayList();

        try {
            stmt = conn.createStatement();
            rs = stmt.executeQuery(strQuery);

            NewTaskInstance taskInstance = null;
            Instance instance = null;
            while (rs.next()) {
                instance = new Instance();
                instance.setInstanceId(rs.getInt(1));

                taskInstance = new NewTaskInstance(instance);
                taskInstance.setTitle(rs.getString(2));
                taskInstance.setPriority(rs.getInt(3));
                taskInstance.setEscalationDate(rs.getString(4));
                taskInstance.setEscalationDutation(rs.getString(5));
                taskInstance.setCreatedDate(rs.getString(6));
                taskInstance.setStatus(rs.getString(7));
                list.add(taskInstance);
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            dbConn.closeResultset(rs);
            dbConn.closeConnection();
        }

        return list;
    }

    private String getAssignmentQueryPart(String userId, String groupId) {
        StringBuffer assignmentQuery = new StringBuffer();

        if ((userId != null) && (groupId != null)) {
            assignmentQuery.append(getUserIDQueryPart(userId));
            assignmentQuery.append(" or ");
            assignmentQuery.append(getGroupIDQueryPart(groupId));
        } else if ((userId != null) && (groupId == null)) {
            assignmentQuery.append(getUserIDQueryPart(userId));
        } else if ((userId == null) && (groupId != null)) {
            assignmentQuery.append(getGroupIDQueryPart(groupId));

        }
        return assignmentQuery.toString();
    }

    private String getUserIDQueryPart(String userId) {
        return " a.user_id = '" + userId + "' ";
    }

    private String getGroupIDQueryPart(String groupId) {
        return " a.group_id = '" + groupId + "' ";
    }

    private String getTaskStatusQueryPart(List statusList) {
        StringBuffer statuses = new StringBuffer();

        for (Iterator<String> i = statusList.iterator(); i.hasNext();) {
            statuses.append("s.status = '");
            statuses.append(i.next());
            if (i.hasNext()) {
                statuses.append("' or ");
            } else {
                statuses.append("'");
            }
        }
        return statuses.toString();
    }
}

