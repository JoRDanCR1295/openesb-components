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
 * @(#)MainGenerator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.swift.xsdbuilder;

import java.io.File;
import java.sql.SQLException;

/**
 * The main generator that calls all other generators to generate all XSDs.
 *  
 * @author Jun Xu
 */
public class MainGenerator {

    public void generate(String dbFileLocation, String targetLocation,
            String version) throws GeneratorException {
        HL7DBConnection hl7DBConn = null;
        try {
            hl7DBConn = new HL7DBConnection(dbFileLocation);
            File fileLocation = new File(targetLocation);
            NameGenerator nameGen = new NameGenerator();
            fileLocation.mkdirs();
            XSDGenerator generator =
                new DataTypesGenerator(hl7DBConn.getConn(), version,
                        fileLocation, nameGen);
            generator.generate();
            generator =
                new FieldsGenerator(hl7DBConn.getConn(), version,
                        fileLocation, nameGen);
            generator.generate();
            generator =
                new SegmentsGenerator(hl7DBConn.getConn(), version,
                        fileLocation, nameGen);
            generator.generate();
            generator =
                new MessagesGenerator(hl7DBConn.getConn(), version,
                        fileLocation, nameGen);
            generator.generate();
        } catch (ClassNotFoundException e) {
            throw new GeneratorException(e);
        } catch (SQLException e) {
            throw new GeneratorException(e);
        } finally {
            if (hl7DBConn != null) {
                try {
                    hl7DBConn.close();
                } catch (SQLException e) {
                    throw new GeneratorException(e);
                }
            }
        }
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        
        String dbLocation = null;
        String outputLocation = null;
        String version = null;
        
        //Default values for testing only
        version = "2.5";
        dbLocation =
            System.getProperty("ENCODER_SHAREDLIBRARY") + "hl7/metadata/hl7_56.mdb";
        outputLocation =
            System.getProperty("ENCODER_SHAREDLIBRARY") + "hl7/dist/xsd/" + version;
        
        for (int i = 0; args != null && i < args.length; i++) {
            if ("-i".equals(args[i])) {
                dbLocation = args[i + 1];
            } else if ("-o".equals(args[i])) {
                outputLocation = args[i + 1];
            } else if ("-v".equals(args[i])) {
                version = args[i + 1];
            }
        }
        if (dbLocation == null || outputLocation == null
                || version == null) {
            System.out.println("Usage: java MainGenerator -i <DB location>"
                    + " -o <output location> -v <HL7 version>");
            System.exit(1);
        }
        
        MainGenerator generator = new MainGenerator();
        
        try {
            generator.generate(dbLocation, outputLocation, version);
        } catch (GeneratorException e) {
            e.printStackTrace();
            System.exit(2);
        }
    }
}
