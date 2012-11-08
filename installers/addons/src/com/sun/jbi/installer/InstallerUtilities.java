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
 * @(#)InstallerUtilities.java
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.installer;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Properties;
import java.util.ResourceBundle;

/**
 * This class has utility methods
 */
public class InstallerUtilities {

    /**
     * This method is used to write to log file
     * @param logFile, the log File
     * @param append, the contents are appended to the file if true
     * @param inputString, contents to be written to the log file
     */
    public static void writeToLogFile(File logFile, boolean append, String inputString)
    {
        try{
            FileWriter logWriter = new FileWriter(logFile, append);
            logWriter.write(inputString);
            logWriter.write("\n");
            logWriter.flush();
            logWriter.close();
        } catch (Exception e) {

        }
    }


    /**
     * This method is used to copy a file
     */
    public static void copyFile(File sourceFile, File destinationFile) throws Exception{

        InputStream in = new FileInputStream(sourceFile);
        OutputStream out = new FileOutputStream(destinationFile);

        byte[] buf = new byte[1024];
        int len;
        while ((len = in.read(buf)) > 0) {
            out.write(buf, 0, len);
        }
        in.close();
        out.close();
    }

    /**
     * This method is used to read a property from a property file.
     */

    public static String  getProperty(File propertyFile, String property){

        try {
            InputStream inputStream = new FileInputStream(propertyFile);
            Properties props = new Properties();
            props.load(inputStream);
            return props.getProperty(property);
        } catch (Exception e) {
            return null;
        }

    }


    /**
     * This method is used to write a property to a property file
     */

    public static void setProperty(File propertyFile, String property, String value){

        try {

            Properties props = new Properties();
            props.setProperty(property, value);
            String comment = ResourceBundle.getBundle(InstallConstants.RESOURCE_BUNDLE).getString("comment-property-file");

            props.store(new FileOutputStream(propertyFile),comment );
        } catch (Exception e){
        }
    }
}
