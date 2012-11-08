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
 * @(#)TestCocoXsdBuilder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.xsdbuilder;

import java.io.File;

/**
 * Test program to run CocoXsdBuilder
 *
 * @author  Noel Ang, Jun Xu
 * @version $Revision: 1.1 $
 */
public class TestCocoXsdBuilder {

    /**
     * Provided for command-line testing for CocoBuilder.
     *
     * @param args Command-line arguments
     */
    public static void main(String[] args) throws Exception {
    
        if (args.length != 4) {
            displayUsage();
            System.exit(-1);
        }
    
        Boolean enforce72;
        Boolean checkNamesForReserved;
        File inFile;
        File outFile;
        String otdName;
        int extPos;
    
        enforce72 = Boolean.valueOf(args[0]);
        checkNamesForReserved = Boolean.valueOf(args[1]);
        inFile = new File(args[2]);
        outFile = new File(args[3]);
        
        try {
            
            otdName = inFile.getName();
            extPos = otdName.indexOf(".");
            otdName =
                    otdName.substring(0,
                            (extPos == -1 ? otdName.length() : extPos));
    
            CocoXsdBuilderSpec spec = new CocoXsdBuilderSpec();
            spec.setCopybookLocation(inFile.getAbsolutePath());
            spec.setCopybookCharEncoding("ASCII");
            spec.setDisplayCharEncoding("Cp037"); //EBCDIC Latin-1 charset
            spec.setDisplay1CharEncoding("Cp834"); //DBCS EBCDIC Korean charset
            spec.setXsdLocation(outFile.getAbsolutePath());
            spec.setCheckNamesForReservedWords(
                    checkNamesForReserved.booleanValue());
            spec.setIgnoreContentBeyondCol72(enforce72.booleanValue());
    
            CocoXsdBuilder builder = new CocoXsdBuilder(spec);
            builder.buildXsd();
    
        } catch (com.sun.encoder.coco.model.CocoParseException cpe) {
            System.err.println(cpe.getLocalizedMessage());
            System.err.println("Row " + cpe.getRow());
            System.err.println("Col " + cpe.getColumn());
            cpe.printStackTrace();
            System.err.flush();
            System.exit(1);
        } catch (CocoXsdBuilderException obe) {
            System.err.println(obe.getLocalizedMessage());
            obe.printStackTrace();
            System.err.flush();
            System.exit(2);
        }
    }
    
    private static void displayUsage() {
        System.err.println(TestCocoXsdBuilder.class.toString()
                + " enforce72 checkname infile outdir\n"
                + "\n"
                + "     enforce72  true or false; true " +
                        "to enforce column 72 limit\n"
                + "     checkName  true or false; true to check item " +
                        "names vs reserved words\n"
                + "     infile     input file name\n"
                + "     outdir     output directory for generated OTDs\n");
    }
}
