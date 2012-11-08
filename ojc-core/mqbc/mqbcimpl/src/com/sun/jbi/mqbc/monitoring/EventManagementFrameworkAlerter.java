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
 */
package com.sun.jbi.mqbc.monitoring;

import com.sun.jbi.alerter.Alerter;
import com.sun.jbi.alerter.AlerterImpl;

/**
 * Utility class that provides an Alerter object whose outlet is the Event
 * Management Framework.
 *
 * @author Noel.Ang@sun.com
 */
public final class EventManagementFrameworkAlerter {
    private EventManagementFrameworkAlerter() {
    }

    /** Usable Alerter for dispatching alert notifications. */
    public static final Alerter alerter = new AlerterImpl();
}
