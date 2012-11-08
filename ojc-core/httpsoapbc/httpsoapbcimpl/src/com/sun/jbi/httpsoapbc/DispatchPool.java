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
 *
 * @author Alexander Lomov
 *
 * Copyright 2011 Open-ESB Community.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.httpsoapbc;

import com.sun.jbi.httpsoapbc.configuration.RuntimeConfiguration;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.concurrent.atomic.AtomicInteger;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.xml.namespace.QName;
import javax.xml.ws.Dispatch;

public class DispatchPool {

    private static DispatchPool me;
    private static AtomicInteger MAXNUM;
    private HashMap<QName, LinkedList<Dispatch>> dispatches;

    private NotificationListener notificationListener = new NotificationListener() {

        public void handleNotification(Notification notification, Object handback) {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                if (attrName.equals(RuntimeConfiguration.CONFIG_OUTBOUND_THREADS)) {
                    int newVal = (Integer) attrNotif.getNewValue();
                    MAXNUM.set(newVal);

                }

            }
        }
    };

    private DispatchPool(RuntimeConfiguration config) {
        config.addNotificationListener(notificationListener, null, null);
        dispatches = new HashMap<QName, LinkedList<Dispatch>>();
        MAXNUM = new AtomicInteger();
    }

    public static synchronized void intitalize(RuntimeConfiguration config) {
        if (me == null) {
            me = new DispatchPool(config);
        }
        MAXNUM.set(config.getOutboundThreads());
    }

    public static DispatchPool instance() {
        if (me == null) {
            throw new RuntimeException("Call DispatchPool.inititlize first");
        }
        return me;
    }

    public static synchronized void uninitialize() {
        instance().dispatches.clear();
    }

    public synchronized Dispatch retain(HttpSoapEndpoint endpoint) {
        QName qualif = new QName(endpoint.getServiceName().getNamespaceURI(), endpoint.getEndpointName());

        LinkedList<Dispatch> disps = dispatches.get(qualif);
        Dispatch d = null;

        if (disps == null || disps.size() == 0) {
            d = endpoint.createDispatch();
        } else {
            d = disps.poll();
        }
        return d;

    }

    public synchronized void release(HttpSoapEndpoint endpoint, Dispatch dispatch) {
        QName qualif = new QName(endpoint.getServiceName().getNamespaceURI(), endpoint.getEndpointName());

        LinkedList<Dispatch> disps = dispatches.get(qualif);
        if (disps == null) {
            disps = new LinkedList<Dispatch>();
            dispatches.put(qualif, disps);
        }

        if (disps.size() < MAXNUM.get()) {
            disps.add(dispatch);
        }

    }
}
