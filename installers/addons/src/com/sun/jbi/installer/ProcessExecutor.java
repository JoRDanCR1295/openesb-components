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
 * @(#)ProcessExecutor.java
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.installer;

import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.OutputStream;
import java.io.FileWriter;


/**
 *
 *This class is used to execute external commands and is used 
 * during installation/uninstallation
 */

    
    /**
     *  This is the inner class that consumes the error and output stream
     */
    class StreamConsumer extends Thread {
        /**
         * The inputstream from the process
         */
        InputStream is;

        /**
         * The boolean that denotes if this is installation
         * this is true if this is installation
         */
        
        /**
         * the logFile
         */
        File logFile = null;
        
        boolean isInstall;
        StreamConsumer (InputStream is,
                File logFile) 
        {
            this.is = is;
            this.logFile = logFile;
        }

    
        /**
         * the thread execution method. This method consumes the input from the
         * given stream and write it in the given resultStore. If no resultStore is given
         * then it is written into the debug file. Installer uses resultStore for commands
         * where the output has to be analysed and is predictably small. For example 
         * output of asadmin start-domain. But for cases like invoking ant commands to
         * install files into the domain/or to install sample components, installer does not
         * supply result store and thus the output goes to the debug files.
         */
        public void run() {
            try {
                InputStreamReader isr = new InputStreamReader(is);
                BufferedReader br = new BufferedReader(isr);
                String line=null;
                while ( (line = br.readLine()) != null) {
                          try{
                                writeToLog(line);

                        } catch(Exception e) {
                            //try writing into the installer debug file.
                            //if not possible simply ignore
                        }
                }
            } catch (Exception ioe) {
                   ioe.printStackTrace();
            }
        }
        
        /**
         *This method is used to dump the contents of the Stream
         *consumers to a log file
         */
        public void writeToLog(String line) {
            try
            { 
                StringBuffer strBuf = new StringBuffer(line);
                strBuf.append("\n");
                FileWriter fw = new FileWriter(logFile, true);
                fw.write(strBuf.toString());
                fw.flush();
                fw.close();
            } 
            catch (Exception e)
            {
                
            }
            
            
        }
    }
    
/**
 * This class executes the given external  command and returns the exitValue
 * output and error.
 */
 public class ProcessExecutor  {
     
     
    /**
     * This method is used to execute a given external command 
     * @return int the execute value
     * @param output after the command is executed 
     * @param error after the command is executed 
     * @param timeout the timeout for the executed process
     */
    public int execute(
                        String cmd, 
                        File logFile) 
    {
        try {
            Runtime rt = Runtime.getRuntime();
            Process proc = rt.exec(cmd);
            
            StreamConsumer errorConsumer = 
                    new StreamConsumer(proc.getErrorStream(), 
                    logFile);
            StreamConsumer outputConsumer = 
                    new StreamConsumer(proc.getInputStream(), 
                     logFile);
            errorConsumer.start();
            outputConsumer.start();
            int exitVal = proc.waitFor();
            return exitVal;
        } catch(Exception e) {
            return -1;
        }
    }
    
 }
