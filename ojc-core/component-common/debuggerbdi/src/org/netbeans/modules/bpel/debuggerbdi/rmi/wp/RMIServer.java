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
 * @(#)RMIServer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.netbeans.modules.bpel.debuggerbdi.rmi.wp;

import java.io.IOException;

import java.net.InetAddress;


public interface RMIServer extends Runnable {

    public RMIService getService();

    public InetAddress getLocalAddress();

    public int getLocalPort();

    public RMIClient getClient() throws IOException;

    public ObjectAdapter createObjectAdapter(String adapterName)
        throws IOException;

    public void destroy();

    public void closeClients();

    public RMIClient createClient(String host, int port)
        throws IOException;

    public RMIClient createClient(InetAddress host, int port)
        throws IOException;

    public void setDefaultAdaptor(ObjectAdapter oa);
}
