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
 * @(#)IEPEngineRmiClientConsole.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.rmi;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlanInfo;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.util.List;
import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.Properties;
import java.io.BufferedReader;
import java.io.InputStreamReader;

import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.util.logging.Level;

/**
 * IEPEngineRmiClientConsole.java
 * 
 * Created on May 24, 2005, 11:27 AM
 * 
 * @author Bing Lu
 */
public class IEPEngineRmiClientConsole {
    private static final Messages mMessages = Messages.getMessages(IEPEngineRmiRegistryImpl.class);

    // -DdbHostname=architect1.stc.com 
    // -DdbPort=1521 
    // -DdbUsername=bing 
    // -DdbPassword=bing 
    // -DdbSid=orclar1
    private static Properties mConfigProp = System.getProperties();
    
    private static IEPEngineRmiRegistry mIepRegistry;
    
    private static String[] parseCommand(String command) {
        StringTokenizer st = new StringTokenizer(command, " ");
        List<String> list = new ArrayList<String>();
        while (st.hasMoreTokens()) {
            list.add(st.nextToken());
        }
        return (String[])list.toArray(new String[0]);
    }

    private static void createEngine(String engineId) {
        try {
            IEPEngineRmi engine = mIepRegistry.createEngine(engineId);
            if (engine != null) {
                engine.init(mConfigProp);
                if (mMessages.isLoggable(Level.FINE)) {
                    mMessages.log(Level.FINE, "IEPEngineRmiClientConsole.Engine_is_created_successfully", engineId);
                }    
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "IEPEngineRmiClientConsole.Failed_to_create_engine", engineId, e);
        }
    }

    private static void listEngines() {
        try {
            List<String> list = mIepRegistry.listEngines();
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "IEPEngineRmiClientConsole.Registered_engines", list);
            }    
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "IEPEngineRmiClientConsole.Failed_to_list_registered_engines", e);
        }
    }
    
    private static IEPEngineRmi getEngine(String engineId) {
        try {
            return mIepRegistry.getEngine(engineId);
        } catch (Throwable e) {
            mMessages.log(Level.SEVERE, "IEPEngineRmiClientConsole.Failed_to_get_information_for_engine", engineId, e);
            return null;
        }
    }
    
    private static void deploy(String engineId, String instanceId, String planFilePath) throws Exception {
        IEPEngineRmi engine = getEngine(engineId);
        if (engine != null) {
            engine.deploy(new QueryPlanInfo(instanceId, planFilePath));
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "IEPEngineRmiClientConsole.Event_processor_is_deployed_successfully", new Object[]{engineId, instanceId, planFilePath});
            }    
        }
    }
    
    private static void undeploy(String engineId, String instanceId) throws Exception {
        IEPEngineRmi engine = getEngine(engineId);
        if (engine != null) {
            engine.undeploy(instanceId);
            Util.removePlanFromDatabase(instanceId, mConfigProp);
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "IEPEngineRmiClientConsole.Event_processor_is_undeployed_successfully", new Object[]{engineId, instanceId});
            }    
        }
    }
    
    private static void start(String engineId, String instanceId) throws Exception {
        IEPEngineRmi engine = getEngine(engineId);
        if (engine != null) {
            engine.start(instanceId);
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "IEPEngineRmiClientConsole.Event_processor_is_started_successfully", new Object[]{engineId, instanceId});
            }    
        }
    }
    private static void stop(String engineId, String instanceId) throws Exception {
        IEPEngineRmi engine = getEngine(engineId);
        if (engine != null) {
            engine.stop(instanceId);
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "IEPEngineRmiClientConsole.Event_processor_is_stopped_successfully", new Object[]{engineId, instanceId});
            }    
        }
    }
    private static void stopAll(String engineId) throws Exception {
        IEPEngineRmi engine = getEngine(engineId);
        if (engine != null) {
            engine.stopAll();
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "IEPEngineRmiClientConsole.All_event_processors_of_engine_are_stopped_successfully", engineId);
            }    
        }
    }
    
    private static void unregister(String engineId) throws Exception {
        IEPEngineRmi engine = getEngine(engineId);
        if (engine != null) {
            engine.unregister();
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "IEPEngineRmiClientConsole.Engine_is_unregistered_successfully", engineId);
            }    
        }
    }

    public static void main(String[] args) {
        StringBuffer menu = new StringBuffer();
        menu.append("\n>action? ");
        menu.append("(createEngine engineId)");
        menu.append("|(listEngines)");
        menu.append("|(deploy engineId instance-id plan-name plan-file-path)");
        menu.append("(undeploy engineId instance-id)");
        menu.append("|(start engineId instance-id)");
        menu.append("|(stop engineId instance-id)");
        menu.append("|(stopAll engineId)");
        menu.append("|(unregister engineId)");
        menu.append("|(quit)");
        BufferedReader userIn = null;
        try {
            String registryHost = "localhost";    
            if (args.length > 0) {
                registryHost = args[0];
            }
            int registryPort = IEPEngineRmiRegistry.DEFAULT_IEP_RMI_REGISTRY_PORT;
            if (args.length > 1) {
                try {
                    registryPort = Integer.parseInt(args[1]);
                } catch (NumberFormatException nfe) {
                }
            }
            Registry registry = LocateRegistry.getRegistry(registryHost, registryPort);
            mIepRegistry = (IEPEngineRmiRegistry)registry.lookup(IEPEngineRmiRegistry.IEP_RMI_REGISTRY_NAME);
            
            userIn = new BufferedReader(new InputStreamReader(System.in));
            while (true) {
                try {
                    mMessages.logOriginal(Level.INFO, menu.toString());
                    mMessages.logOriginal(Level.INFO, ">>");
                    String ans = userIn.readLine();
                    if (ans == null) {
                        break;
                    }
                    ans = ans.trim();
                    if (ans.equals("")) {
                        continue;
                    }
                    String[] cmd = parseCommand(ans);
                    if ("createEngine".startsWith(cmd[0])) {
                        createEngine(cmd[1]);
                    } else if ("listEngines".startsWith(cmd[0])) {
                        listEngines();
                    } else if ("deploy".startsWith(cmd[0])) {
                        deploy(cmd[1], cmd[2], cmd[3]);
                    } else if ("undeploy".startsWith(cmd[0])) {
                        undeploy(cmd[1], cmd[2]);
                    } else if ("start".startsWith(cmd[0])) {
                        start(cmd[1], cmd[2]);
                    } else if ("stop".startsWith(cmd[0])) {
                        stop(cmd[1], cmd[2]);
                    } else if ("stopAll".startsWith(cmd[0])) {
                        stopAll(cmd[1]);
                    } else if ("unregister".startsWith(cmd[0])) {
                        unregister(cmd[1]);
                    } else if ("quit".startsWith(cmd[0])) {
                       System.exit(0);
                    }
                } catch (Throwable e) {
                    mMessages.log(Level.SEVERE, "IEPEngineRmiClientConsole.main_fails", e);
                }
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "IEPEngineRmiClientConsole.main_fails", e);
        } finally {
            try {
                if (userIn != null) {
                    userIn.close();
                }
            } catch (Exception e1) {
                mMessages.log(Level.SEVERE, "IEPEngineRmiClientConsole.Closing_userIn_fails", e1);
            }
        }
    }
    
}
