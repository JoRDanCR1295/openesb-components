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
 * @(#)IEPEngineRmiRegistrar.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.rmi;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.io.Serializable;
import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.util.logging.Level;

/**
 * IEPEngineRmiRegistrar.java
 *
 * Created on May 23, 2005, 6:10 PM
 *
 * @author Bing Lu
 */
public class IEPEngineRmiRegistrar implements Serializable {
    private static final Messages mMessages = Messages.getMessages(IEPEngineRmiRegistrar.class);

    private static IEPEngineRmiRegistrar mInstance;
    
    private IEPEngineRmiImpl mEngine;
    
    private Object mToken;
    
    private transient boolean mUnregistered = false;
    
    /**
     * Creates a new instance of IEPEngineRmiRegistrar
     */
    private IEPEngineRmiRegistrar() {
        try {
            mEngine = new IEPEngineRmiImpl(this);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "IEPEngineRmiRegistrar.Constructor_fails", e);
        }
    }
    
    public void register(String engineId) {
        register(engineId, IEPEngineRmiRegistry.DEFAULT_IEP_RMI_REGISTRY_PORT);
    }
    
    public void register(String engineId, int port) {
        try {
            Registry registry = LocateRegistry.getRegistry(port);
            registry.rebind(engineId, mEngine);
            IEPEngineRmiRegistry iepRegistry = (IEPEngineRmiRegistry)registry.lookup(IEPEngineRmiRegistry.IEP_RMI_REGISTRY_NAME);
            iepRegistry.engineRegistered(engineId);
            mToken = new Object();
            while (!mUnregistered) {
                synchronized (mToken) {
                    try {
                        if (!mUnregistered) {
                            mToken.wait();
                        }
                    } catch (InterruptedException ie) {
                    }
                }
            }
            registry.unbind(engineId);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "IEPEngineRmiRegistrar.register_fails", e);
        }
    }
    
    public void unregister() {
        synchronized (mToken) {
            mUnregistered = true;
            mToken.notifyAll();
        }
    }
    
    public synchronized static IEPEngineRmiRegistrar getInstance() {
        if (mInstance == null) {
            mInstance = new IEPEngineRmiRegistrar();
        }
        return mInstance;
    }
    
    public static void main(String[] args) {
        getInstance().register(args[0]);
    }
}
