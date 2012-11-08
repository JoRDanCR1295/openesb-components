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
 * @(#)IEPEngineRmiRegistry.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.rmi;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.ArrayList;


/**
 * IEPEngineRmiRegistry.java
 *
 * Created on May 25, 2005, 5:32 PM
 *
 * @author Bing Lu
 */
public interface IEPEngineRmiRegistry extends Remote {
    public static final int DEFAULT_IEP_RMI_REGISTRY_PORT = 6688;
    public static final String IEP_RMI_REGISTRY_NAME = "com.sun.jbi.engine.iep.core.runtime.rmi.IepRmiRegistry";

    /**
     * Create a IEP Engine 
     *
     * @param engineId  uniquely identifies the IEP session
     */
    public IEPEngineRmi createEngine(String engineId) throws RemoteException;
    
    /**
     * Callback method to get notified that the engine is created and registered
     *
     * @param engineId  uniquely identifies the IEP session
     */
    public void engineRegistered(String engineId) throws RemoteException;
    
    /**
     * Get a IEP session bound with: engineId
     *
     * @param engineId  uniquely identifies the IEP session
     */
    public IEPEngineRmi getEngine(String engineId) throws RemoteException;
    
    /**
     * @return true if engineId is bound.
     */
    public boolean hasEngine(String engineId) throws RemoteException;
    
    /**
     * @return the list of engineIds already bound
     */
    public ArrayList<String> listEngines() throws RemoteException;
}
