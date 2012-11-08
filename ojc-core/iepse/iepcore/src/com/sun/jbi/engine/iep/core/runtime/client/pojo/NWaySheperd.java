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
 * @(#)NWaySheperd.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import com.sun.jbi.engine.iep.core.runtime.util.IOUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.logging.Level;

/**
 * NWaySheperd.java
 *
 * Created on September 7, 2005, 11:53 PM
 *
 * @author Bing Lu
 */
public class NWaySheperd {
    private static final Messages mMessages = Messages.getMessages(NWaySheperd.class);
    
    private List<Runnable> mTaskList;
    private Map<String, Command> mCommandTable;
    private boolean mStop = false;
    
    /* ================Example nwaysheper property file================
     * description=?
     * ######### Commands ########
     * command.count=?
     * command.0.name=?
     * command.0.type=?
     * command.0.props=?
     * ......
     * command..name=?
     * command.K.class=?
     * command.K.propertyFile=?
     *
     * ######### Inputs #########
     * input.count=?
     * input.0.dataFile=?
     * input.0.templateFile=?
     * input.0.batchSize=?
     * ....
     * input.$N.templateFile=?
     * input.$N.dataFile=?
     * input.$N.batchSize=?
     *
     * ######## Script ###########
     * scriptFile=?
     * ===============End of example property file===========
     * 
     * see Input.java for example data file and example template file
     *
     * ===============Example script file====================
     * send $inputName $numberOfBatches (for example: "send input.1 3")
     * wait $seconds (for example: "wait 2")
     * ===============End of example script file=============
     */
    public NWaySheperd(Properties prop) {
        // String description = prop.getProperty("description");
        try {
            // commands
            int commandCount = Integer.parseInt(prop.getProperty("command.count"));
            mCommandTable = new HashMap<String, Command>();
            for (int i = 0; i < commandCount; i++) {
                String name = "command." + i;
                String commandName = prop.getProperty(name + ".name");
                String type = prop.getProperty(name + ".type");
                String props = prop.getProperty(name + ".props");
                Command cmd = (Command)Class.forName(type).newInstance();
                cmd.init(this, props);
                mCommandTable.put(commandName, cmd);
            }
            
            // inputs
            int inputCount = Integer.parseInt(prop.getProperty("input.count"));
            Map<String, Input> inputTable = new HashMap<String, Input>();
            for (int i = 0; i < inputCount; i++) {
                String name = "input." + i;
                String dataFile = prop.getProperty(name + ".dataFile");
                String templateFile = prop.getProperty(name + ".templateFile");
                int batchSize = Integer.parseInt(prop.getProperty(name + ".batchSize"));
                inputTable.put(name, new Input(name, templateFile, dataFile, batchSize));
            }
            // script
            String scriptFile = prop.getProperty("scriptFile");
            mTaskList = loadScript(scriptFile, inputTable);
        } catch (Exception ex) {
            mMessages.log(Level.SEVERE, "NWaySheperd.Constructor_fails", ex);
        } catch (Error er) {
            mMessages.log(Level.SEVERE, "NWaySheperd.Constructor_fails", er);
        }
    }
    
    public void run() {
        mStop = false;
        for (Runnable r : mTaskList) {
            if (mStop) {
                break;
            }
            r.run();
        }
    }
    
    public void stop() {
        mStop = true;
    }
    
    public void destroy() {
        Command[] cmds = (Command[])mCommandTable.values().toArray(new Command[0]);
        for (Command cmd : cmds) {
            cmd.destroy();
        }
    }
    
    private static String[] parseCommand(String command) {
        StringTokenizer st = new StringTokenizer(command, " ");
        List<String> list = new ArrayList<String>();
        while (st.hasMoreTokens()) {
            list.add(st.nextToken());
        }
        return (String[])list.toArray(new String[0]);
    }

    private List<Runnable> loadScript(String scriptFile, Map<String, Input> inputTable) {
        List<Runnable> taskList = new ArrayList<Runnable>();
        BufferedReader fileIn = null;
        try {
            fileIn = new BufferedReader(new InputStreamReader(IOUtil.getResourceAsStream(scriptFile)));
            while (true) {
                String line = fileIn.readLine();
                if (line == null){
                    break;
                }
                if (line.trim().equals("")) {
                    continue;
                };
                String[] cmd = parseCommand(line);
                Command command = mCommandTable.get(cmd[0]);
                if (command != null) {
                    taskList.add(command.createRunnable(inputTable, cmd));
                }
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "NWaySheperd.loadScript_fails", e);
        } finally {
            try {
                if (fileIn != null) {
                    fileIn.close();
                }
            } catch (Exception e1) {
                mMessages.log(Level.SEVERE, "NWaySheperd.Closing_fileIn_fails", e1);
            }
        }
        return taskList;
    }
    
    public static void main(String[] args) {
        if (args.length < 1) {
            mMessages.logOriginal(Level.INFO, "java com.sun.jbi.engine.iep.core.runtime.pojo.NWaySheperd propFile");
            return;
        }
        try {
            String propFile = args[0];
            Properties p = new Properties();
            p.load(IOUtil.getResourceAsStream(propFile));
            NWaySheperd s = new NWaySheperd(p);
            s.run();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "NWaySheperd.main_fails", e);
        }
    }
    
}
