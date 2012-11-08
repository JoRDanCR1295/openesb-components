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

package com.sun.encoder.cmdline;

import com.sun.encoder.custom.CustomEncoderProvider;

/**
 *
 * @author ltang
 */
public class MainCustom {

    /**
     * @param args the command line arguments
     * @throws Exception 
     */
    public static void main(String[] args) throws Exception {

        if (args.length < 4 || args.length > 7) {
            System.out.println("Usage: java com.sun.encoder.cmdline.MainCustom"
                    + " <decode|encode> xsdFilename inputDataFile outputDataFile [rootQName] [logFilename] [logLevel]");
            System.exit(0);
        }

        boolean toDecode = args[0].toUpperCase().startsWith("D");
        String xsdFilename = args[1];
        String inputFilename = args[2];
        String outputFilename = args[3];
        String encodingStyle = CustomEncoderProvider.STYLE_ID;
        String rootQName = args.length > 4 ? args[4]
                : "{http://xml.netbeans.org/schema/customenc}Root";
        String loggerPkgName = "com.sun.encoder.custom";
        String logFilename = toDecode ? "decode.log" : "encode.log";
        if (args.length > 5) {
            logFilename = args[5];
        }
        String logLevel = "FINE";
        if (args.length > 6) {
            logLevel = args[6].toUpperCase();
        }

        CmdLineMain.process(toDecode, xsdFilename, inputFilename, outputFilename,
                encodingStyle, rootQName, loggerPkgName, logFilename, logLevel);
    }
}
