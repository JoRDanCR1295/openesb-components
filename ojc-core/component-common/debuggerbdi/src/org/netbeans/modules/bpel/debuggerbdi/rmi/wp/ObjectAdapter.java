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
 * @(#)ObjectAdapter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.netbeans.modules.bpel.debuggerbdi.rmi.wp;


import java.io.IOException;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebugListener;


public interface ObjectAdapter extends Runnable {

    public RMIServer getServer();

    public String getAdapterName();

    public Object exportObject(Object servant) throws IOException;

    public Object exportObject(String objectKey, Object servant)
        throws IOException;

    public Object unexportObjectWithKey(String key) throws IOException;

    public String[] unexportObject(Object servant) throws IOException;

    public Object getObjectWithKey(String key) throws IOException;

    public String[] getObjectKeys(Object servant) throws IOException;

    public void setClient(RMIClient client);

    public void start();

    public void destroy();

    public void registerListerner(DebugListener listener);

    public void notifyClose(Object socket);
}
