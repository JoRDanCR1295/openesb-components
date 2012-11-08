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
 * @(#)IEPEngineRmiRegistryConsole.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.rmi;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.util.List;
import java.util.ArrayList;
import java.util.StringTokenizer;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.logging.Level;

/**
 * IEPEngineRmiRegistryConsole.java
 *
 * Created on May 25, 2005, 6:19 PM
 *
 * @author Bing Lu
 */
public class IEPEngineRmiRegistryConsole {
    private static final Messages mMessages = Messages.getMessages(IEPEngineRmiRegistryConsole.class);

    private static Registry mRegistry = null;
    
    private static String[] parseCommand(String command) {
        StringTokenizer st = new StringTokenizer(command, " ");
        List<String> list = new ArrayList<String>();
        while (st.hasMoreTokens()) {
            list.add(st.nextToken());
        }
        return (String[])list.toArray(new String[0]);
    }
    
    private static void startRegistry(String starter, int port) {
        try {
            mRegistry = LocateRegistry.createRegistry(port);
            mRegistry.rebind(IEPEngineRmiRegistry.IEP_RMI_REGISTRY_NAME, new IEPEngineRmiRegistryImpl(starter, port));
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "IEPEngineRmiRegistryConsole.IEPEngineRmiRegistry_is_started_at_port", port);
            }    
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "IEPEngineRmiRegistryConsole.startRegistry_fails", e);
        }
    }
    
    private static void stopRegistry() {
        try {
            mRegistry.unbind(IEPEngineRmiRegistry.IEP_RMI_REGISTRY_NAME);
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "IEPEngineRmiRegistryConsole.IEPEngineRmiRegistry_is_stopped");
            }    
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "IEPEngineRmiRegistryConsole.stopRegistry_fails", e);
        }
    }
    
    private static void listRegistry() {
        try {
            IEPEngineRmiRegistry iepRegistry = 
                (IEPEngineRmiRegistry)mRegistry.lookup(IEPEngineRmiRegistry.IEP_RMI_REGISTRY_NAME);
            List<String> list = iepRegistry.listEngines();
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "IEPEngineRmiRegistryConsole.Registered_engines", list);
            }    
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "IEPEngineRmiRegistryConsole.listRegistry_fails", e);
        }
    }

    // args: startMatlabWithIep-path port
    public static void main(String[] args) {
        String starter = args[0];
        int port = IEPEngineRmiRegistry.DEFAULT_IEP_RMI_REGISTRY_PORT;
        if (args.length > 1) {
            try {
                port = Integer.parseInt(args[1]);
            } catch (NumberFormatException e) {
                
            }
        }
        StringBuffer menu = new StringBuffer();
        menu.append("\n>action? ");
        menu.append("(startRegistry)");
        menu.append("|(listRegistry)");
        menu.append("|(stopRegistry)");
        menu.append("|(quit)");
        BufferedReader userIn = null;
        try {
            userIn = new BufferedReader(new InputStreamReader(System.in));
            while (true) {
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
                if ("startRegistry".startsWith(cmd[0])) {
                    startRegistry(starter, port);
                } else if ("listRegistry".startsWith(cmd[0])) {
                    listRegistry();
                } else if ("stopRegistry".startsWith(cmd[0])) {
                    stopRegistry();
                } else if ("quit".startsWith(cmd[0])) {
                   System.exit(0);
                }
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "IEPEngineRmiRegistryConsole.main_fails", e);
        } finally {
            try {
                if (userIn != null) {
                    userIn.close();
                }
            } catch (Exception e1) {
                mMessages.log(Level.SEVERE, "IEPEngineRmiRegistryConsole.Closing_userIn_fails", e1);
            }
        }
    }
}
