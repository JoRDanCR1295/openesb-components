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
 * @(#)$Id: HL7ManagementClient.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7.mgmt.client;

import com.sun.esb.management.client.ManagementClient;
import com.sun.jbi.hl7.mgmt.api.HL7ManagementService;
import com.sun.jbi.hl7.mgmt.impl.HL7ManagementServiceImpl;
import java.io.Serializable;
import javax.management.MBeanServerConnection;

/**
 *
 * @author ylee
 */
public class HL7ManagementClient extends ManagementClient implements Serializable {

    /** remote MBeanServer connection */
    protected transient MBeanServerConnection remoteConnection;
    /** is this a local or remote connection */
    protected boolean isRemoteConnection;


    /** Constructor */
    public HL7ManagementClient() {
        this(null, false);
    }

    /**
     * Constructor
     *
     * @param serverConnection
     */
    public HL7ManagementClient(MBeanServerConnection serverConnection) {
        this(serverConnection, false);
    }

    /**
     * Constructor
     *
     * @param serverConnection
     * @param isRemoteConnection
     */
    public HL7ManagementClient(MBeanServerConnection serverConnection,
            boolean isRemoteConnection) {
        super(serverConnection,isRemoteConnection);
        // keep a copy
        this.remoteConnection = serverConnection;
        this.isRemoteConnection = isRemoteConnection;
    }

    //
    public HL7ManagementService getHL7ManagementService() {
        return new HL7ManagementServiceImpl(remoteConnection,isRemoteConnection);
    }
    

}
