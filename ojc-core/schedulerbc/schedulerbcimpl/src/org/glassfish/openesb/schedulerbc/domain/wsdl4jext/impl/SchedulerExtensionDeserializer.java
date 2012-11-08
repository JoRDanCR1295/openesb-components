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
 * @(#)SchedulerExtensionDeserializer.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain.wsdl4jext.impl;

import com.ibm.wsdl.util.xml.DOMUtils;
import java.util.List;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.glassfish.openesb.schedulerbc.domain.SchedulerConstants;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.ActivePeriodEx;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.BindingEx;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.BindingOperationEx;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.TriggerEx;
import org.w3c.dom.Element;

/**
 * Deserializes Scheduler extensibility elements.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerExtensionDeserializer implements ExtensionDeserializer,
        SchedulerConstants {

    public ExtensibilityElement unmarshall(Class parentType, QName elementType,
            Element extensibility, Definition def, ExtensionRegistry extReg)
            throws WSDLException {
        if (BindingEx.ELEM_TYPE.equals(elementType)) {
            BindingEx bindingEx = new BindingExImpl();
            List attrs = DOMUtils.getAttributes(extensibility);
            String group = DOMUtils.getAttribute(extensibility,
                    GROUP_ATTR_NCNAME, attrs);
            if (group != null) {
                bindingEx.setGroup(group);
            }
            String dateFormat = DOMUtils.getAttribute(extensibility,
                    DATEFORMAT_ATTR_NCNAME, attrs);
            if (dateFormat != null) {
                bindingEx.setDateFormat(dateFormat);
            }
            return bindingEx;
        } else if (BindingOperationEx.ELEM_TYPE.equals(elementType)) {
            BindingOperationEx operationEx = new BindingOperationExImpl();
            List attrs = DOMUtils.getAttributes(extensibility);
            String mode = DOMUtils.getAttribute(extensibility,
                    MODE_ATTR_NCNAME, attrs);
            operationEx.setMode(mode);
            return operationEx;
        } else if (TriggerEx.ELEM_TYPE.equals(elementType)) {
            TriggerEx triggerEx = new TriggerExImpl();
            List attrs = DOMUtils.getAttributes(extensibility);
            String name = DOMUtils.getAttribute(extensibility,
                    NAME_ATTR_NCNAME, attrs);
            triggerEx.setName(name);
            String type = DOMUtils.getAttribute(extensibility,
                    TYPE_ATTR_NCNAME, attrs);
            triggerEx.setType(type);
            String enabled = DOMUtils.getAttribute(extensibility,
                    ENABLED_ATTR_NCNAME, attrs);
            if (enabled != null) {
                triggerEx.setEnabled(enabled);
            }
            String description = DOMUtils.getAttribute(extensibility,
                    DESCRIPTION_ATTR_NCNAME, attrs);
            if (description != null) {
                triggerEx.setDescription(description);
            }
            String repeat = DOMUtils.getAttribute(extensibility,
                    REPEAT_ATTR_NCNAME, attrs);
            triggerEx.setRepeat(repeat);
            String interval = DOMUtils.getAttribute(extensibility,
                    INTERVAL_ATTR_NCNAME, attrs);
            triggerEx.setInterval(interval);
            String cronExpr = DOMUtils.getAttribute(extensibility,
                    CRON_EXPR_ATTR_NCNAME, attrs);
            triggerEx.setCronExpr(cronExpr);
            String duration = DOMUtils.getAttribute(extensibility,
                    DURATION_ATTR_NCNAME, attrs);
            triggerEx.setDuration(duration);
            String message = DOMUtils.getAttribute(extensibility,
                    MESSAGE_ATTR_NCNAME, attrs);
            triggerEx.setMessage(message);
            return triggerEx;
        } else if (ActivePeriodEx.ELEM_TYPE.equals(elementType)) {
            ActivePeriodEx activePeriodEx = new ActivePeriodExImpl();
            List attrs = DOMUtils.getAttributes(extensibility);
            String starting = DOMUtils.getAttribute(extensibility,
                    STARTING_ATTR_NCNAME, attrs);
            activePeriodEx.setStarting(starting);
            String ending = DOMUtils.getAttribute(extensibility,
                    ENDING_ATTR_NCNAME, attrs);
            activePeriodEx.setEnding(ending);
            String timezone = DOMUtils.getAttribute(extensibility,
                    TIMEZONE_ATTR_NCNAME, attrs);
            activePeriodEx.setTimezone(timezone);
            return activePeriodEx;
        }
        return null;
    }
}
