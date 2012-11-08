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
 * @(#)SchedulerExtensionSerializer.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain.wsdl4jext.impl;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.common.util.Util;
import java.io.PrintWriter;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;
import org.glassfish.openesb.schedulerbc.domain.SchedulerConstants;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.ActivePeriodEx;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.BindingEx;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.BindingOperationEx;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.TriggerEx;

/**
 * Serializes Scheduler extensibility elements.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerExtensionSerializer implements ExtensionSerializer,
        SchedulerConstants {

    private static final String indent = "    ";                        //NOI18N
    private static final String indentX2 = indent + indent;
    private static final String indentX3 = indentX2 + indent;
    private static final String indentX4 = indentX3 + indent;
    private static final String indentX5 = indentX4 + indent;
    private static final String BEGIN_TAG = "<";                        //NOI18N
    private static final String END_TAG = " />";                        //NOI18N
    
    public void marshall(Class parentType, QName elementType,
            ExtensibilityElement extension, PrintWriter pw, Definition def,
            ExtensionRegistry extReg) throws WSDLException {
        if (BindingEx.ELEM_TYPE.equals(elementType)) {
            BindingEx bindingEx = (BindingEx) extension;
            openTag(indentX2, BindingEx.ELEM_TYPE, def, pw);
            appendAttribute(GROUP_ATTR_NCNAME, bindingEx.getGroup(), pw);
            appendAttribute(DATEFORMAT_ATTR_NCNAME,
                    bindingEx.getDateFormat(), pw);
            closeTag(pw);
        } else if (BindingOperationEx.ELEM_TYPE.equals(elementType)) {
            BindingOperationEx operationEx = (BindingOperationEx) extension;
            openTag(indentX3, BindingOperationEx.ELEM_TYPE, def, pw);
            appendAttribute(MODE_ATTR_NCNAME, operationEx.getMode(), pw);
            closeTag(pw);
        } else if (TriggerEx.ELEM_TYPE.equals(elementType)) {
            TriggerEx triggerEx = (TriggerEx) extension;
            openTag(indentX4, TriggerEx.ELEM_TYPE, def, pw);
            appendAttribute(NAME_ATTR_NCNAME, triggerEx.getName(), pw);
            appendAttribute(TYPE_ATTR_NCNAME, triggerEx.getType(), pw);
            appendAttribute(ENABLED_ATTR_NCNAME,
                    Boolean.toString(triggerEx.isEnabled()), pw);
            newline(pw);
            if (triggerEx.getDescription() != null) {
                appendAttribute(indentX5, DESCRIPTION_ATTR_NCNAME,
                        triggerEx.getDescription(), pw);
                newline(pw);
            }
            if (!Util.isEmpty(triggerEx.getRepeat())) {
                appendAttribute(indentX5, REPEAT_ATTR_NCNAME,
                        triggerEx.getRepeat(), pw);
                if (!Util.isEmpty(triggerEx.getInterval())) {
                    appendAttribute(INTERVAL_ATTR_NCNAME,
                            triggerEx.getInterval(), pw);
                }
                newline(pw);
            }
            if (!Util.isEmpty(triggerEx.getCronExpr())) {
                appendAttribute(indentX5, CRON_EXPR_ATTR_NCNAME,
                        triggerEx.getCronExpr(), pw);
                newline(pw);
            }
            if (!Util.isEmpty(triggerEx.getDuration())) {
                appendAttribute(indentX5, DURATION_ATTR_NCNAME,
                        triggerEx.getDuration(), pw);
                newline(pw);
            }
            appendAttribute(indentX5, MESSAGE_ATTR_NCNAME,
                    triggerEx.getMessage(), pw);
            closeTag(pw);
        } else if (ActivePeriodEx.ELEM_TYPE.equals(elementType)) {
            ActivePeriodEx activePeriodEx = (ActivePeriodEx) extension;
            openTag(indentX3, ActivePeriodEx.ELEM_TYPE, def, pw);
            appendAttribute(STARTING_ATTR_NCNAME,
                    activePeriodEx.getStarting(), pw);
            appendAttribute(ENDING_ATTR_NCNAME,
                    activePeriodEx.getEnding(), pw);
            if (!Util.isEmpty(activePeriodEx.getTimezone())) {
                appendAttribute(TIMEZONE_ATTR_NCNAME,
                        activePeriodEx.getTimezone(), pw);
            }
            closeTag(pw);
        }
    }
    
    private void openTag(String indention, QName et, Definition def,
            PrintWriter pw) throws WSDLException {
        pw.print(indention + BEGIN_TAG + getTag(et, def));
    }
    
    private void newline(PrintWriter pw) {
        pw.println();
    }
    
    private void closeTag(PrintWriter pw) {
        pw.println(END_TAG);
    }
    
    private String getTag(QName et, Definition def) throws WSDLException {
        return DOMUtils.getQualifiedValue(et.getNamespaceURI(),
                    et.getLocalPart(), def);
    }

    private void appendAttribute(String indention, String name, String value,
            PrintWriter pw) {
        if (indention != null) {
            pw.print(indention);
        }
        appendAttribute(name, value, pw);
    }
    
    private void appendAttribute(String name, String value, PrintWriter pw) {
        DOMUtils.printAttribute(name, value, pw);
    }
}
