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
 * @(#)Utility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.performance.jdbcjmsbd;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.logging.Level;
import java.util.logging.Logger;


public class Utility {

    private static Logger LOGGER = Logger.getLogger(Utility.class.getName());


    
    /**
     * @param message
     */
    public static void print(String message) {
        print(message, null);
    }
    
    /**
     * @param message
     * @param value
     */
    public static void print(String message, String value) {
        if (value == null) {
            LOGGER.log(Level.INFO, message);
            return;
        }
        LOGGER.log(Level.INFO, message + " : " + value);        
    }
    
    public static String readTextFromFile(String fileName, String srcEncoding)
        throws IOException {
        InputStream inputStream = null;
        String ret = null;
        try {
            inputStream = new FileInputStream(fileName);
            InputStreamReader in = new InputStreamReader(inputStream, srcEncoding);
            StringWriter out = new StringWriter();
            
            char[] buf = new char[512];
            int n = 0;
            while ((n = in.read(buf)) != -1) {
                out.write(buf, 0, n);
            }
            out.flush();
            ret = out.toString();
        } catch (IOException e) {
            throw e;
        } finally {
            if (inputStream != null) {
                inputStream.close();
            }
        }
        //mLog.debug("ret: " + ret);
        return ret;
    }    
    
    public static void writeTextToFile(String fileName, String text, String encoding) 
        throws Exception {
        OutputStream outputStream = null;
        try {
            outputStream = new FileOutputStream(fileName);
            OutputStreamWriter out = new OutputStreamWriter(outputStream, encoding);
            StringReader in = new StringReader(text);
            char[] buf = new char[512];
            int n = 0;
            while ((n = in.read(buf)) != -1) {
                out.write(buf, 0, n);
            }
            out.flush();
        }catch (IOException e) {
            throw e;
        } finally {
            if (outputStream != null) {
                outputStream.close();
            }
        }
        
    } 
    
        
    public static String replaceAll(String s, String match, String replacement) {
        StringBuffer sb = new StringBuffer();
        String temp = s;
        while (true) {
            int i = temp.indexOf(match);
            if (i < 0) {
                sb.append(temp);
                return sb.toString();
            }
            sb.append(temp.substring(0, i));
            sb.append(replacement);
            temp = temp.substring(i + match.length());
        }
    }
}
