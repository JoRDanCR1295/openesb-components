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
 * @(#)PrinterDriver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.logging.Level;

/**
 * PrinterDriver.java
 *
 * Created on September 7, 2005, 11:53 PM
 *
 * @author Bing Lu
 */
public class PrinterDriver {
    private static final Messages mMessages = Messages.getMessages(Printer.class);

    public static void main(String[] args) {
        if (args.length < 4) {
            mMessages.logOriginal(Level.INFO, "java com.sun.jbi.engine.iep.core.runtime.util.PrinterDriver printerName name instanceId outputId fileName|stdout interval");
            return;
        }
        BufferedReader userIn = null;
        try {
            String printerName = args[0];
            String name = args[1];
            String instanceId = args[2];
            String outputId = args[3];
            String fileName = args[4];
            long interval = Long.parseLong(args[5]);
            Printer printer = (Printer)Class.forName(printerName).newInstance();
            printer.initialize(name, instanceId, outputId, fileName, interval);
            Thread thread = new Thread(printer);
            thread.start();
            userIn = new BufferedReader(new InputStreamReader(System.in));
            while (true) {
                mMessages.log(Level.INFO, "PrintDriver.Stop", "(y)");
                String ans = userIn.readLine();
                if (ans == null) {
                    break;
                }
                if (ans.trim().equals("y")) {
                    printer.stop();
                    break;
                }
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "PrintDriver.main_fails", e);
        } finally {
            try {
                userIn.close();
            } catch (Exception e) {
                mMessages.log(Level.SEVERE, "PrintDriver.Closing_userIn_fails", e);
            }
        }
    }
    
}    
