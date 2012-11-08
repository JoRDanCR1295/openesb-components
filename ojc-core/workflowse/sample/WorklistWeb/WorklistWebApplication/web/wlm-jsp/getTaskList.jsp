<%@page contentType="application/xml"%>
<%@page pageEncoding="UTF-8"%>

<%-- start web service invocation --%>
<%@page import="java.util.List"%>
<%@page import="com.sun.workflow.client.*"%>
<%@page import="com.sun.workflow.xml.*"%>


<%!
    public boolean isNull(String text) {
        if (text == null || text.trim().length() == 0) {
            return true;
        } else {
            return false;
        }
    }
%>

<%
        try {
            String userId = request.getUserPrincipal().getName();
            String startIndexStr = request.getParameter("start");
            String pageSizeStr = request.getParameter("pageSize");
            String searchStr = request.getParameter("searchString");
            String searchStatues = request.getParameter("searchStatues");
            String searchUsers = request.getParameter("searchUsers");
            String searchGroups = request.getParameter("searchGroups");
            String searchType = request.getParameter("searchType");

            int startIndex = Integer.parseInt(startIndexStr);
            int pageSize = Integer.parseInt(pageSizeStr);

            com.sun.workflow.client.TaskCommonService service = new com.sun.workflow.client.TaskCommonService();
            com.sun.workflow.client.TaskCommonPortType port = service.getTaskCommonPort();
            // TODO initialize WS operation arguments here
            com.sun.workflow.client.QueryType queryString = new QueryType();
            queryString.setSearchString(searchStr);

            if (searchStr != null && searchStr.trim().length() > 0 && isNull(searchStatues) && isNull(searchUsers) && isNull(searchGroups)) {
                queryString.setType("TEXTSEARCH");
            } else {

                SortField sort = null;
                Direction dir = null;

                if (request.getParameter("sort") != null) {
                    sort = SortField.fromValue(request.getParameter("sort"));
                }
                if (request.getParameter("dir") != null) {
                    dir = Direction.fromValue(request.getParameter("dir"));
                }

                if (sort == null) {
                    sort = SortField.ID;
                }
                if (dir == null) {
                    dir = Direction.ASC;
                }
                queryString.setSort(sort);
                queryString.setDir(dir);

                if (searchType.equals("DEFAULT")) {
                    queryString.setType("DEFAULT");
                } else {
                    queryString.setType("FILTERED");
                }

                if (!isNull(searchStatues)) {
                    searchStatues = searchStatues.trim();
                    String[] strings = searchStatues.split("\\s+");
                    if (strings != null && strings.length > 0) {
                        for (int i = 0; i < strings.length; i++) {
                            if (strings[i].equals("Assigned")) {
                                queryString.getTaskStatus().add(TaskStatus.ASSIGNED);
                            } else if (strings[i].equals("Claimed")) {
                                queryString.getTaskStatus().add(TaskStatus.CLAIMED);
                            } else if (strings[i].equals("Escalated")) {
                                queryString.getTaskStatus().add(TaskStatus.ESCALATED);
                            } else if (strings[i].equals("Completed")) {
                                queryString.getTaskStatus().add(TaskStatus.COMPLETED);
                            } else if (strings[i].equals("Failed")) {
                                queryString.getTaskStatus().add(TaskStatus.FAILED);
                            }
                        }
                    }
                }

                if (!isNull(searchUsers)) {
                    searchUsers = searchUsers.trim();
                    String[] strings = searchUsers.split("\\s+");
                    if (strings != null && strings.length > 0) {
                        com.sun.workflow.client.UsersType users = new com.sun.workflow.client.UsersType();
                        for (int i = 0; i < strings.length; i++) {
                            users.getUser().add(strings[i]);
                        }
                        queryString.setUsers(users);
                    }
                }

                if (!isNull(searchGroups)) {
                    searchGroups = searchGroups.trim();
                    String[] strings = searchGroups.split("\\s+");
                    if (strings != null && strings.length > 0) {
                        com.sun.workflow.client.GroupsType groups = new com.sun.workflow.client.GroupsType();
                        for (int i = 0; i < strings.length; i++) {
                            groups.getGroup().add(strings[i]);
                        }
                        queryString.setGroups(groups);
                    }
                }
            }
            // TODO process result here
            com.sun.workflow.client.TaskListType result = port.getTaskList(queryString, userId, startIndex, pageSize);
            List<TaskType> tasks = result.getTask();
            int totalRecords = result.getTotalRecords();
            int returnRecords = result.getReturnedRecords();
            out.print("<taskDetails>");



            for (TaskType task : tasks) {
                out.print("<task>");
                out.print("<id>");
                out.print(task.getTaskId());
                out.print("</id>");

                out.print("<title>");
                out.print(task.getTitle() == null ? "" : task.getTitle());
                out.print("</title>");

                out.print("<createDate>");
                out.print(task.getSubmittedDate().toString());
                out.print("</createDate>");

                out.print("<priority>");
                out.print(task.getPriority());
                out.print("</priority>");

                out.print("<status>");
                out.print(task.getStatus().value());
                out.print("</status>");

                out.print("<assignedTo>");
                out.print(task.getAssignedTo() == null ? "" : task.getAssignedTo());
                out.print("</assignedTo>");

                out.print("<owner>");
                out.print(task.getClaimedBy() == null ? "" : task.getClaimedBy());
                out.print("</owner>");

                out.print("<deadline>");
                out.print(task.getDeadline() == null ? "" : task.getDeadline().toString ());
                out.print("</deadline>");

                /**  Use the following method to get keywords 
                    String keywords = task.getKeywords();
                    List<String> keywordsList = com.sun.workflow.xml.KeywordsHelper.getKeywords(keywords);
                    out.print("<OrderId>");
                    out.print(keywordsList.get(0));
                    out.print("</OrderId1>");
                     ...
                     //Also, need to add each keyword tag (e.g. OrderId) to wlmEntry.jsp's myColumnHeaders, myDataSource.responseSchema
                **/

                out.print("</task>");
            }
            out.print("<total>");
            out.print(totalRecords);
            out.print("</total>");
            out.print("<returned>");
            out.print(returnRecords);
            out.print("</returned>");
            out.print("</taskDetails>");
        } catch (Exception ex) {
            out.print(ex.getMessage());
        }
%>
<%-- end web service invocation --%>


