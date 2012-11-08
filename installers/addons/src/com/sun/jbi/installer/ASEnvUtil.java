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
 * @(#)ASEnvUtil.java
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.installer;
        
import java.io.File;
import java.io.IOException;
import java.io.FileOutputStream;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.StringReader;

/**
 * This class is used to parse the appServer environment variable values.
 */
public class ASEnvUtil {
    
    
    public static void main(String args[]) {
            if (args.length == 1) {
                String asJDKHome = getAppserverJDK(args[0]);
                System.out.println("The appserver JDK home is " + asJDKHome);
        }
    }

        /**
     * This method returns the JAVA_HOME for the JDK 
     * used by the appserver
     */
     public static String getAppserverJDK(String installDir) {
        BufferedReader bf = null;
        InputStream in = null;
        String javaHome = "";
        boolean isWindows = false;
        try {
                        if (System.getProperty("os.name").indexOf("Windows") != -1) {
                                isWindows = true;
                      }
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
