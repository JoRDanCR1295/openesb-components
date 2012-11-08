/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.workflow.servlets;

import com.sun.workflow.client.QueryType;
import com.sun.workflow.client.TaskStatus;
import com.sun.workflow.client.TaskType;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 *
 * @author mei
 */
public class QuerySampleServlet extends HttpServlet {


    /**
     * Processes requests for both HTTP <code>GET</code> and <code>POST</code> methods.
     * @param request servlet request
     * @param response servlet response
     */
    protected void processRequest(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        response.setContentType("text/xml;charset=UTF-8");
        PrintWriter out = response.getWriter();
        try {
            String userId = request.getUserPrincipal().getName();

            com.sun.workflow.client.TaskCommonService service = new com.sun.workflow.client.TaskCommonService();
            com.sun.workflow.client.TaskCommonPortType port = service.getTaskCommonPort();
            // TODO initialize WS operation arguments here
            com.sun.workflow.client.QueryType queryString = new QueryType();
            com.sun.workflow.client.UsersType users = new com.sun.workflow.client.UsersType();

            users.getUser().add("john");
            users.getUser().add("dale");
            queryString.setUsers(users);

            com.sun.workflow.client.GroupsType groups = new com.sun.workflow.client.GroupsType();

            groups.getGroup().add("CustomerServiceRep");
            groups.getGroup().add("Manager");

            queryString.setGroups(groups);

            queryString.setType("FILTERED");

            queryString.getTaskStatus().add(TaskStatus.ASSIGNED);
            queryString.getTaskStatus().add(TaskStatus.ESCALATED);
            queryString.getTaskStatus().add(TaskStatus.COMPLETED);

            // TODO process result here
            com.sun.workflow.client.TaskListType result = port.getTaskList(queryString, userId, 0, 0);
            List<TaskType> tasks = result.getTask();
            out.print("<taskDetails>");
            for (TaskType task : tasks) {
                out.print("<task>");
                out.print("<id>");
                out.print(task.getTaskId());
                out.print("</id>");

                out.print("<title>");
                out.print(task.getTitle() == null ? "" : task.getTitle());
                out.print("</title>");

                out.print("<submittedOn>");
                out.print(task.getSubmittedDate().toString());
                out.print("</submittedOn>");

                out.print("<priority>");
                out.print(task.getPriority());
                out.print("</priority>");

                out.print("<status>");
                out.print(task.getStatus().value());
                out.print("</status>");

                out.print("<assignedTo>");
                out.print(task.getAssignedTo() == null ? "" : task.getAssignedTo());
                out.print("</assignedTo>");

                out.print("<claimedBy>");
                out.print(task.getClaimedBy() == null ? "" : task.getClaimedBy());
                out.print("</claimedBy>");

                out.print("</task>");
            }
            out.print("</taskDetails>");
        } catch (Exception ex) {
            ex.printStackTrace(out);
        } finally {
            out.close();
        }
    }
    // <editor-fold defaultstate="collapsed" desc="HttpServlet methods. Click on the + sign on the left to edit the code.">
    /** 
     * Handles the HTTP <code>GET</code> method.
     * @param request servlet request
     * @param response servlet response
     */
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        processRequest(request, response);
    }

    /** 
     * Handles the HTTP <code>POST</code> method.
     * @param request servlet request
     * @param response servlet response
     */
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        processRequest(request, response);
    }

    /** 
     * Returns a short description of the servlet.
     */
    public String getServletInfo() {
        return "Short description";
    }// </editor-fold>
}
