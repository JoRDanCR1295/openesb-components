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

package com.sun.jbi.engine.iep.util;

import java.io.File;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author rdwivedi
 */
public class Utility {
    
    private static Logger LOGGER = Logger.getLogger(Utility.class.getName());
    private static String derbyScriptFile= "C:/open-esb-jbi-component/open-jbi-components/ojc-core/iepse/iepjbiadapter/test/scripts/StartDatabaseServer.bat";
    private static String serverScriptFile= "C:/open-esb-jbi-component/open-jbi-components/ojc-core/iepse/iepjbiadapter/test/scripts/StartAppServer.bat";
    public static void startAppServer(Properties props) throws Exception {
        LOGGER.log(Level.INFO, "Starting AppsServer ...");
        File startAppServerScriptFile = new File(serverScriptFile);
        Runtime.getRuntime().exec(startAppServerScriptFile.getAbsolutePath());
        //+
        //" >> " + props.getProperty("StartAppServerStatus"));
    }
    
    public static void startDBServer(Properties props) throws Exception {
        LOGGER.log(Level.INFO, "Starting database server ...");
        File startDatabaseServerScriptFile = new File(derbyScriptFile);
        Runtime.getRuntime().exec(startDatabaseServerScriptFile.getAbsolutePath()) ;
        //+
       // " >> " + props.getProperty("StartDatabaseServerStatus"));        
    }

}
