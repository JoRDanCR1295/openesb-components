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
 * @(#)TestCocoLexer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.model;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;

/**
 * Test program to verify correctness of CocoLexer
 *
 * @author  Noel Ang
 *
 * @version $Revision: 1.2 $
 */
public class TestCocoLexer {

    /**
     * Provided for command-line testing for CocoLexer.
     *
     * @param args Command-line arguments
     */
    public static void main(String[] args) throws Exception {
        File inFile = null;
        File outFile = null;
        PrintStream outputStream = null;
    
        if (args.length < 1 || args.length > 2) {
            displayUsage();
            System.exit(-1);
        }
    
        switch (args.length) {
            case 2:
                outFile = new File(args[1]);
                outputStream = new PrintStream(new FileOutputStream(outFile));
            case 1:
                inFile = new File(args[0]);
                outputStream = System.out;
        }
    
        CocoLexer lexer = new CocoLexer(inFile);
        CocoToken token = null;
        do {
            token = lexer.getNextToken();
            if (token != null) {
                displayToken(token, outputStream);
            }
        } while (token != null);
    }
    
    private static void displayUsage() {
        System.err.println(TestCocoLexer.class.toString()
            + " infile [outfile]");
    }
    
    private static void displayToken(CocoToken token, PrintStream outStream) {
        String value = token.getStringValue();
        int col      = token.getColumn();
        int row      = token.getRow();
        int len      = token.getLength();
        CocoTokenTypes type  = token.getType();
        outStream.print("TOKEN/" + type + ":" + value);
        outStream.println(" [col=" + col + " row=" + row + " len=" + len + "]");
        outStream.flush();
    }
}
