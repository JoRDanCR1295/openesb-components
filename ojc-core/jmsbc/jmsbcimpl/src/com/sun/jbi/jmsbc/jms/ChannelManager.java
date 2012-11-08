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

package com.sun.jbi.jmsbc.jms;

import java.text.MessageFormat;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.Endpoint;
import com.sun.jbi.jmsbc.Endpoint.EndpointType;

/**
 * Channel management interface
 * 
 */
public interface ChannelManager {
    
    /**
     * Creates a Channel and returns it.
     * Retains the created Channel for management.
     *
     * @param endpoint The Endpoint instance containing the JMS binding operation
     * @param jmsBindingOpName The JMS binding operation name for which the Channel is to model
     * @return The channel added
     *
     * @throws ChannelManagerException if a Channel already exists for the given Endpoint
     *         and operation QName.
     */
    public Channel addChannel(Endpoint endpoint, 
                              QName jmsBindingOpName) throws ChannelManagerException;

    /**
     * Removes a previously created Channel.
     *
     * @param endpoint The Endpoint instance containing the JMS binding operation
     * @param jmsBindingOpName The JMS binding operation name for which the Channel is to model
     * @return The channel removed
     *
     * @throws ChannelManagerException if a Channel does not exist for the given Endpoint
     *         and operation QName.
     */
    public Channel removeChannel (Endpoint endpoint, 
                                  QName jmsBindingOpName) throws ChannelManagerException;
    
    /**
     * Lookup a previously created Channel.
     *
     * @param endpoint The Endpoint instance containing the JMS binding operation
     * @param jmsBindingOpName The JMS binding operation name for which the Channel is to model
     *
     * @throws ChannelManagerException if a Channel does not exist for the given Endpoint
     *         and operation QName.
     */
    public Channel lookup(Endpoint endpoint, 
                         QName jmsBindingOpName) throws ChannelManagerException;

    /**
     * Lookup a previously created Channel.
     *
     * @param serviceName The Service QName.
     * @param endpointName The endpoint name.
     * @param endpointType The endpoint type; if provisioning endpoint then Endpoint.EndpointType.OUTBOUND
     *                     otherwise it is a consuming endpoint then Endpoint.EndpointType.INBOUND.
     * @param operation The JMS binding operation QName.
     *
     * @throws ChannelManagerException if a Channel can not be found.
     */
    public Channel lookup(QName serviceName, 
                          String endpointName, 
                          int endpointType,
                          QName jmsBindingOpName) throws ChannelManagerException;
            
}
