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
 * @(#)DefaultJBITaskMessageBuilder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import java.util.Iterator;

import com.sun.jbi.util.JBIUtils;

public class DefaultJBITaskMessageBuilder extends AbstractJBITaskMessageBuilder {
    protected String createComponentMessage(ComponentTaskResult componentTaskResult)
        throws JBIMessageException
    {
        StringBuffer buffer = new StringBuffer();
        populate(buffer, componentTaskResult);
        return buffer.toString();
    }

    private void populate(StringBuffer buffer, ComponentTaskResult ctr) throws JBIMessageException
    {
        ctr.validate();
        buffer.append("<");
        buffer.append(ComponentTaskResult.ELEMENT_NAME);

        buffer.append(">\n");

        buffer.append("  <");
        buffer.append(ComponentTaskResult.COMPONENT_NAME_NAME);
        buffer.append(">");
        buffer.append(JBIUtils.escape(ctr.getComponentName()));
        buffer.append("</");
        buffer.append(ComponentTaskResult.COMPONENT_NAME_NAME);
        buffer.append(">\n");

        populate(buffer, ctr.getDetails());

        buffer.append("</");
        buffer.append(ComponentTaskResult.ELEMENT_NAME);
        buffer.append(">");
    }

    private void populate(StringBuffer buffer, ComponentTaskResultDetails ctrd) {
        ctrd.validate();
        buffer.append("  <");
        buffer.append(ComponentTaskResultDetails.ELEMENT_NAME);
//buffer.append(" xmlns=\"http://java.sun.com/xml/ns/jbi/management-message\"");
        buffer.append(">\n");

        populate(buffer, ctrd.getDetails());

        buffer.append("  </");
        buffer.append(ComponentTaskResultDetails.ELEMENT_NAME);
        buffer.append(">\n");
    }

    private void populate(StringBuffer buffer, TaskResultDetails trd) {
        trd.validate();
        buffer.append("  <");
        buffer.append(TaskResultDetails.ELEMENT_NAME);
        buffer.append(">\n");

        buffer.append("    <");
        buffer.append(TaskResultDetails.TASK_ID_NAME);
        buffer.append(">");
        buffer.append(trd.getTaskId());
        buffer.append("</");
        buffer.append(TaskResultDetails.TASK_ID_NAME);
        buffer.append(">\n");

        buffer.append("    <");
        buffer.append(TaskResultDetails.TASK_RESULT_NAME);
        buffer.append(">");
        buffer.append(trd.isSuccessfulResult() ? "SUCCESS": "FAILED");
        buffer.append("</");
        buffer.append(TaskResultDetails.TASK_RESULT_NAME);
        buffer.append(">\n");

        if (trd.getMessageType() != JBITaskMessageBuilder.NONE) {
            buffer.append("    <");
            buffer.append(TaskResultDetails.MESSAGE_TYPE_NAME);
            buffer.append(">");
            buffer.append(trd.getMessageTypeString());
            buffer.append("</");
            buffer.append(TaskResultDetails.MESSAGE_TYPE_NAME);
            buffer.append(">\n");
        }

        if (trd.getMessages() != null && trd.getMessages().size() != 0) {
            buffer.append("    <");
            buffer.append(TaskResultDetails.TASK_STATUS_MESSAGE_NAME);
            buffer.append(">\n");
            Iterator i = trd.getMessages().iterator();
            while (i.hasNext()) {
                MessageLocalizationInfo messageLocalization = (MessageLocalizationInfo) i.next();
                populate(buffer, messageLocalization);
            }
            buffer.append("    </");
            buffer.append(TaskResultDetails.TASK_STATUS_MESSAGE_NAME);
            buffer.append(">\n");
        }

        if (trd.getExceptions() != null && trd.getExceptions().size() != 0) {
            Iterator i = trd.getExceptions().iterator();
            while (i.hasNext()) {
                ExceptionInfo exceptionInfo = (ExceptionInfo ) i.next();
                populate(buffer, exceptionInfo);
            }
        }

        buffer.append("  </");
        buffer.append(TaskResultDetails.ELEMENT_NAME);
        buffer.append(">\n");
    }

    private void populate(StringBuffer buffer, MessageLocalizationInfo mli) {
        mli.validate();
        buffer.append("      <");
        buffer.append(MessageLocalizationInfo.ELEMENT_NAME);
        buffer.append(">\n");

        buffer.append("        <");
        buffer.append(MessageLocalizationInfo.LOCALIZATION_TOKEN_NAME);
        buffer.append(">");
        buffer.append(JBIUtils.escape(mli.getToken()));
        buffer.append("</");
        buffer.append(MessageLocalizationInfo.LOCALIZATION_TOKEN_NAME);
        buffer.append(">\n");

        buffer.append("        <");
        buffer.append(MessageLocalizationInfo.LOCALIZATION_MESSAGE_NAME);
        buffer.append(">");
        buffer.append(JBIUtils.escape(mli.getMessage()));
        buffer.append("</");
        buffer.append(MessageLocalizationInfo.LOCALIZATION_MESSAGE_NAME);
        buffer.append(">\n");

        Object[] parameters = mli.getParameters();
        if (parameters != null) {
            for (int i = 0; i < parameters.length; i++) {
                String parameter = ""; // "null"
                if (parameters[i] != null) {
                    parameter = parameters[i].toString();
                }
                buffer.append("        <");
                buffer.append(MessageLocalizationInfo.LOCALIZATION_PARAMETER_NAME);
                buffer.append(">");
                buffer.append(JBIUtils.escape(parameter));
                buffer.append("</");
                buffer.append(MessageLocalizationInfo.LOCALIZATION_PARAMETER_NAME);
                buffer.append(">\n");
            }
        }

        buffer.append("      </");
        buffer.append(MessageLocalizationInfo.ELEMENT_NAME);
        buffer.append(">\n");
    }

    private void populate(StringBuffer buffer, ExceptionInfo ei) {
        ei.validate();
        buffer.append("    <");
        buffer.append(ExceptionInfo.ELEMENT_NAME);
        buffer.append(">\n");

        buffer.append("      <");
        buffer.append(ExceptionInfo.NESTING_LEVEL_NAME);
        buffer.append(">");
        buffer.append(ei.getNestingLevel());
        buffer.append("</");
        buffer.append(ExceptionInfo.NESTING_LEVEL_NAME);
        buffer.append(">\n");

        populate(buffer, ei.getMessageLocalization());

        buffer.append("      <");
        buffer.append(ExceptionInfo.STACK_TRACE_NAME);
        buffer.append(">\n");
        for (Throwable t = ei.getException(); t != null; t = t.getCause()) {
            buffer.append('\n');
            buffer.append(JBIUtils.escape(JBIUtils.stringStackTrace(t)));
        }
        buffer.append("      </");
        buffer.append(ExceptionInfo.STACK_TRACE_NAME);
        buffer.append(">\n");

        buffer.append("    </");
        buffer.append(ExceptionInfo.ELEMENT_NAME);
        buffer.append(">\n");
    }
}
