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
 * @(#)TestCocoParser.java 
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
 * Test program to verify correctness of CocoParser
 *
 * @author  Noel Ang
 * 
 * @version $Revision: 1.2 $
 */
public class TestCocoParser {
    
    /**
     * Provided for command-line testing for CocoLexer.
     *
     * @param args Command-line arguments
     */
    public static void main(String[] args) throws Exception {
        Boolean enforce72 = Boolean.valueOf(true);
        Boolean checkNamesForReserved = Boolean.valueOf(true);
        File inFile = null;
        File outFile = null;
        PrintStream outputStream = null;
    
        if (args.length < 3 || args.length > 4) {
            displayUsage();
            System.exit(-1);
        }
    
        switch (args.length) {
            case 4:
                outFile = new File(args[3]);
                outputStream = new PrintStream(new FileOutputStream(outFile));
            case 3:
                inFile = new File(args[2]);
                outputStream =
                    (outputStream == null ? System.out : outputStream);
                checkNamesForReserved = Boolean.valueOf(args[1]);
                enforce72 = Boolean.valueOf(args[0]);
        }
    
        CocoLexer lexer = new CocoLexer(inFile);
        lexer.setDisable72ColumnLimit(!enforce72.booleanValue());
        CocoParser parser = new CocoParser(lexer);
        parser.disableItemNameReservedWordChecking(
                !checkNamesForReserved.booleanValue());
        CocoDataModel model = null;
        try {
            model = parser.parse();
            model.toStream(outputStream);
        } catch (CocoParseException cpe) {
            outputStream.println(cpe.getMessage());
            /*
            outputStream.println(cpe.getLocalizedMessage());
            outputStream.println("Row " + cpe.getRow());
            outputStream.println("Col " + cpe.getColumn());
            */
            cpe.printStackTrace(outputStream);
            outputStream.flush();
        }
    }
    
    private static void displayUsage() {
        System.err.println(TestCocoParser.class.toString()
            + " enforce72 checkName infile [outfile]\n"
            + "\n"
            + "     checkName  true or false; true to check item names vs " +
                    "reserved words\n"
            + "     enforce72  true or false; true to enforce column 72 limit\n"
            + "     infile     input file name\n"
            + "     outfile    output file name\n");
    
    }
}
