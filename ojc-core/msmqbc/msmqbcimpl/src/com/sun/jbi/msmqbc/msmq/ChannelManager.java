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
 * @(#)ChannelManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 *
 * @author Sun Microsystems
 */

package com.sun.jbi.msmqbc.msmq;

import javax.xml.namespace.QName;

import com.sun.jbi.msmqbc.Endpoint;
import com.sun.jbi.msmqbc.exception.ChannelManagerException;

/**
 * Channel management interface
 * 
 */
public interface ChannelManager {

    /**
     * Creates a Channel and returns it.
     * Retains the created Channel for management.
     *
     * @param endpoint The Endpoint instance containing the MSMQ binding operation
     * @param msmqBindingOpName The MSMQ binding operation name for which the Channel is to model
     * @return The channel added
     *
     * @throws ChannelManagerException if a Channel already exists for the given Endpoint
     *         and operation QName.
     */
    public Channel addChannel(Endpoint endpoint, QName msmqBindingOpName) throws ChannelManagerException;

    /**
     * Removes a previously created Channel.
     *
     * @param endpoint The Endpoint instance containing the MSMQ binding operation
     * @param msmqBindingOpName The MSMQ binding operation name for which the Channel is to model
     * @return The channel removed
     *
     * @throws ChannelManagerException if a Channel does not exist for the given Endpoint
     *         and operation QName.
     */
    public Channel removeChannel(Endpoint endpoint, QName msmqBindingOpName) throws ChannelManagerException;

    /**
     * Lookup a previously created Channel.
     *
     * @param endpoint The Endpoint instance containing the MSMQ binding operation
     * @param msmqBindingOpName The MSMQ binding operation name for which the Channel is to model
     *
     * @throws ChannelManagerException if a Channel does not exist for the given Endpoint
     *         and operation QName.
     */
    public Channel lookup(Endpoint endpoint, QName msmqBindingOpName) throws ChannelManagerException;

    /**
     * Lookup a previously created Channel.
     *
     * @param serviceName The Service QName.
     * @param endpointName The endpoint name.
     * @param endpointType The endpoint type; if provisioning endpoint then Endpoint.EndpointType.OUTBOUND
     *                     otherwise it is a consuming endpoint then Endpoint.EndpointType.INBOUND.
     * @param operation The MSMQ binding operation QName.
     *
     * @throws ChannelManagerException if a Channel can not be found.
     */
    public Channel lookup(QName serviceName, String endpointName, int endpointType, QName msmqBindingOpName)
            throws ChannelManagerException;

    /**
     * Removes all previously created Channel(s) if any.
     */
    public void removeChannels();

}
