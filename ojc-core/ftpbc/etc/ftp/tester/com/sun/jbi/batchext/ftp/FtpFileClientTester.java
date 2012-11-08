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
 * @(#)FtpFileClientTester.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.batchext.ftp;

import java.util.Properties;

/*
 *
 * 
 * 
 * 
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */
public class FtpFileClientTester {
    
    public FtpFileClientTester() {
    }
    
    public static void main(String args[])
    throws Exception {
        System.out.println("Start of FtpFileClientTester.");
        FtpFileClientImpl ftp = null;
        try {
            BatchFtp etd = new BatchFtp();
            etd.initialize(new Properties());
            boolean old = false;
            if(old) {
                ftp = new FtpFileClientImpl();
                ftp.initialize(etd);
            } else {
                ftp = (FtpFileClientImpl)etd.getClient();
            }
        } catch(Exception e) {
            System.out.println("Could not create ftp handle.");
            e.printStackTrace();
            return;
        }
        try {
            Properties props = new Properties();
            props.put("FTP/Directory Listing Style", "NT 4.0");
            props.put("FTP/Host Name", "localhost");
            props.put("FTP/User Name", "anonymous");
            props.put("FTP/Password", "");
            props.put("Target Location/Target Directory Name", "FtpFileClient");
            props.put("Target Location/Target File Name", "file_%#.txt");
            ftp.initialConfigValues(props);
            ftp.setPayload("this is a payload for a ftp file".getBytes());
            ftp.put();
            System.out.println("End of FtpFileClientTester successfully :-)");
            return;
        } catch(Exception e) {
            e.printStackTrace();
            System.out.println("End of FtpFileClientTester with exception :-(");
            return;
        }
    }
}
