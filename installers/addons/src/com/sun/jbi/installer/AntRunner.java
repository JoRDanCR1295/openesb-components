/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)AntRunner.java
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.installer;

import java.io.File;
import java.io.FileOutputStream;
import java.io.*;
import java.util.logging.Logger;
import java.util.Properties;

/**
 * AntRunner is a class that can be used to run
 * an ant task from a specified ant file in a specified directory.
 *
 * @author Sun Microsystems, Inc.
 */
public class AntRunner
{

    /**
     * Run a target in an ant file.
     *
     * @param basedir the ant basedir
     * @param antfn the name of the ant file.
     * @param targetname the target you want to run
     * @return true if ant task is successfully run
     */
    public static boolean
    runAnt(String appserverInstallDir, String antfn, String targetname, 
        Properties props, String logFileName) throws Exception
    {
        String antHome = null;
        String asant = null;
        String antLauncher = null;
        String space = " ";
        if (System.getProperty("os.name").indexOf("Windows") != -1) {

            antHome = appserverInstallDir + File.separator + 
                        "lib" + File.separator +
                        "ant";
            
            antLauncher = antHome + File.separator 
                            + "lib" + File.separator +
                            "ant-launcher.jar";
                    
            //if appserver installdir has spaces, deal with it
            //surround the entire command with quotes
            if(appserverInstallDir.indexOf(' ') != -1) {
                    antHome = "\"" + antHome + "\"";
            }  
            
            if(appserverInstallDir.indexOf(' ') != -1) {
                    antLauncher = "\"" + antLauncher + "\"";
            }                
            
            String asJdk = getAppserverJDK(appserverInstallDir, true);            
            String asJava = asJdk + File.separator
                     + "bin" + File.separator
                     + "java.exe";
            
             if(asJdk.indexOf(' ') != -1) {
                    asJava = "\"" + asJava + "\"";
             }    
            
            String javaHome = asJdk;
            if(asJdk.indexOf(' ') != -1) {
                javaHome = "\"" + asJdk + "\"";
            }   
                    
              asant = asJava + space 
                     + "-classpath" + space 
                     + antLauncher  + space
                     + "-Dant.home="  + antHome + space
                     + "-Djava.home=" + javaHome + space
                     + "org.apache.tools.ant.launch.Launcher" + space
                    ;            
         }//windows
         else {
            asant =  appserverInstallDir + File.separator + "bin" +
                    File.separator + "asant --noconfig";
        }
        File propertiesFile = null;
        FileOutputStream outStream = null;
        try {       
            propertiesFile = File.createTempFile("antPropertiesFile", null);
            propertiesFile.deleteOnExit();
            outStream = new FileOutputStream(propertiesFile);
            props.store(outStream, null);
            outStream.close();
            outStream = null;
        } catch (Exception e) {
        }
                
        String arguments =   
                "-f" + space +
                new File(antfn).getPath() + space +
                "-propertyfile" + space +
                propertiesFile.getPath() + space +
                targetname;

        int exitValue =  new ProcessExecutor().execute(
                asant + space + arguments, 
                new File(logFileName));
        return true;
    }
    
    /**
     * This method returns the JAVA_HOME for the JDK 
     * used by the appserver
     */
     public static String getAppserverJDK(String installDir, boolean isWindows) {
        BufferedReader bf = null;
        InputStream in = null;
        String javaHome = "";
        try {
            String asenv="";
            if(!isWindows) 
                asenv = installDir + File.separator + "config" + File.separator + 
                        "asenv.conf";
            else
             asenv = installDir + File.separator + "config" + File.separator + 
                     "asenv.bat";
        
            in = new FileInputStream(asenv);

            bf = new BufferedReader(new InputStreamReader(in));
            String line = bf.readLine();
            while(line != null) {
                if(line.indexOf("AS_JAVA") != -1) {
                    int pos = line.indexOf("=");
                    if (pos > 0) {
                        String lhs = (line.substring(0, pos)).trim();
                        String rhs = (line.substring(pos + 1)).trim();

                        if (isWindows) {    //trim off the "set "
                            lhs = (lhs.substring(3)).trim();
                        }

                        if (!isWindows) {      // take the quotes out
                            pos = rhs.indexOf("\"");
                            if(pos != -1) {
                                rhs = (rhs.substring(pos+1)).trim();
                                pos = rhs.indexOf("\"");
                                if(pos != -1)
                                rhs = (rhs.substring(0, pos)).trim();
                            }
                
                        }
                        javaHome = rhs;
                        break;    
                    }
                }
                line = bf.readLine();
             }
        } catch(Exception e) {
           //System.out.println("Default JDK of SDK");
           return installDir + File.separator + "java";
        } 
        return javaHome;
     }

}
