/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ManagementMessage.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.util;


/**
 *
 * @author Sun Microsystems
 */
public class ManagementMessage {
    public static final String STATUS_SUCCESS = "SUCCESS";
    public static final String STATUS_FAILED  = "FAILED";
    public static final String TYPE_ERROR     = "ERROR";
    public static final String TYPE_WARNING   = "WARNING";
    public static final String TYPE_INFO      = "INFO";

    /** Creates a new instance of ManagementMessage */
    private ManagementMessage() {
    }

    static public String createDeployMessage(String suName,
                                             String taskName,
                                             String result,
                                             String type,
                                             String token,
                                             String msg,
                                             String[] params,
                                             Throwable exp) {
        StringBuffer paramBuff = new StringBuffer();
        if (params != null) {
            for (int i = 0, I = params.length; i < I; i++) {
                paramBuff.append("          <loc-param>")
                    .append(params[i])
                    .append("</loc-param>\n");
            }
        }
        String msgStr = "";
        if (msg != null) {
            msgStr = "          <loc-message>" + msg + "</loc-message>\n";
        }
        String expStr = "";
        if (exp != null) {
            expStr = "        <stack-trace>" + exp + "</stack-trace>\n";
        }

        String expInfoStr = "";
        if (!result.equals(STATUS_SUCCESS) && (msg != null || exp != null)) {
            expInfoStr =    "      <exception-info>\n" +
                            "        <nesting-level>1</nesting-level>\n" +
                            "        <msg-loc-info>\n" +
                            "          <loc-token>" + token + "</loc-token>\n" +
                                       msgStr +
                                       paramBuff.toString()+
                            "        </msg-loc-info>\n" +
                                     expStr +
                            "      </exception-info>\n";
        }

        String retMsg =     "<component-task-result>\n" +
                            "  <component-name>" + suName + "</component-name>\n" +
                            "  <component-task-result-details xmlns=\"http://java.sun.com/xml/ns/jbi/management-message\">\n" +
                            "    <task-result-details>\n" +
                            "      <task-id>" + taskName + "</task-id>\n" +
                            "      <task-result>" + result + "</task-result>\n" +
                            "      <message-type>" + type + "</message-type>\n" +
                                   expInfoStr +
                            "    </task-result-details>\n" +
                            "  </component-task-result-details>\n" +
                            "</component-task-result>\n";
        return retMsg;
    }
}
