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
 * @(#)IEPEngineRmiRegistryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.rmi;

import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.logging.Level;


/**
 * IEPEngineRmiRegistryImpl.java
 *
 * Created on May 25, 2005, 5:32 PM
 *
 * @author Bing Lu
 */
public class IEPEngineRmiRegistryImpl extends UnicastRemoteObject implements IEPEngineRmiRegistry {
    private static final Messages mMessages = Messages.getMessages(IEPEngineRmiRegistryImpl.class);
    
    private String mStarter;
    private int mPort;
    private transient Registry mRegistry;
    private HashMap<String, RegistryRecord> mRegistryTable;
    
    public IEPEngineRmiRegistryImpl(String starter, int port) throws RemoteException {
        mStarter = starter;
        mPort = port;
        mRegistry = LocateRegistry.getRegistry(port);
        mRegistryTable = new HashMap<String, RegistryRecord>();
    }
    
    public IEPEngineRmi createEngine(String engineId) throws RemoteException {
        RegistryRecord record = new RegistryRecord(engineId);
        String[] args = new String[]{mStarter, engineId, "" + mPort};
        try {
            // launch: startMatlabWithIep.exe engineId registryPort
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "IEPEngineRmiRegistryImpl.Start_engine_with", new Object[]{args[0], args[1], args[2]});
            }    
            Runtime.getRuntime().exec(args);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "IEPEngineRmiRegistryImpl.CreateEngine_fails", e);
            return null;
        }
        mRegistryTable.put(engineId, record);
        while (!record.mRegistered)  {
            synchronized (record.mToken) {
                try {
                    if (!record.mRegistered) {
                        record.mToken.wait();
                    }
                } catch (InterruptedException e) {
                }
            }
        }
        return getEngine(engineId);
    }
    
    public void engineRegistered(String engineId) throws RemoteException {
        RegistryRecord record = mRegistryTable.get(engineId);
        if (record != null && record.mEngineId.equals(engineId)) {
            synchronized (record.mToken) {
                record.mRegistered = true;
                record.mToken.notifyAll();
            }
        }
    }
    
    public IEPEngineRmi getEngine(String engineId) throws RemoteException {
        try {
            return (IEPEngineRmi)mRegistry.lookup(engineId);
        } catch (NotBoundException e) {
            return null;
        }
    }
    
    public boolean hasEngine(String engineId) throws RemoteException {
        String[] list = mRegistry.list();
        for (int i = 0; i < list.length; i++) {
            if (list[i].equals(engineId)) {
                return true;
            }
        }
        return false;
    }
    
    public ArrayList<String> listEngines() throws RemoteException {
        ArrayList<String> ret = new ArrayList<String>();
        String[] list = mRegistry.list();
        for (int i = 0; i < list.length; i++) {
            if (list[i].equals(IEPEngineRmiRegistry.IEP_RMI_REGISTRY_NAME)) {
                continue;
            }
            ret.add(list[i]);
        }
        return ret;
    }
}

class RegistryRecord {
    public String mEngineId;
    public Object mToken = new Object();
    public boolean mRegistered = false;
    
    public RegistryRecord(String engineId) {
        mEngineId = engineId;
    }
}
