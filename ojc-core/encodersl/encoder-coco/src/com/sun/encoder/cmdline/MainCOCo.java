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

import com.sun.encoder.coco.CocoEncoderProvider;
import com.sun.encoder.coco.xsdbuilder.CocoXsdBuilder;
import com.sun.encoder.coco.xsdbuilder.CocoXsdBuilderSpec;
import java.io.File;
import java.io.FileInputStream;
import java.util.Properties;

/**
 *
 * @author sun
 */
public class MainCOCo {

    /**
     * @param args the command line arguments
     * @throws Exception 
     */
    public static void main(String[] args) throws Exception  {

        if (args.length < 2
                || args.length > 7
                || (args.length < 4 && !args[0].toUpperCase().startsWith("B"))) {
            System.out.println("Usage: java com.sun.encoder.cmdline.MainCOCo"
                    + " <build> CobolCopybookDoc [outputXSD] [propertiesFile]");
            System.out.println("   OR: java com.sun.encoder.cmdline.MainCOCo"
                    + " <decode|encode> xsdFilename inputDataFile outputDataFile [rootQName] [logFilename] [logLevel]");
            System.exit(0);
        }

        if (args[0].toUpperCase().startsWith("B")) {
            String copybookFilename = args[1];
            File copybookFile = new File(copybookFilename);
            if (!copybookFile.exists()) {
                throw new Exception("Copybook File does not exist: " + copybookFilename);
            }
            System.out.println("CobolCopybookDoc: " + copybookFilename);
            File outputXSDFile = null;
            if (args.length > 2) {
                outputXSDFile = new File(args[2]);
            } else {
                String basename = copybookFile.getName();
                if (basename.toUpperCase().endsWith(".CPY")) {
                    basename = basename.substring(0, basename.toUpperCase().lastIndexOf(".CPY"));
                } else if (basename.toUpperCase().endsWith(".COBOL")) {
                    basename = basename.substring(0, basename.toUpperCase().lastIndexOf(".COBOL"));
                }
                outputXSDFile = new File(copybookFile.getParentFile(), basename + ".xsd");
            }
            System.out.println("outputXSDFile: " + outputXSDFile);
            String propertiesFilename = null;
            if (args.length > 3) {
                propertiesFilename = args[3];
            }
            System.out.println("propertiesFilename: " + propertiesFilename);
            buildEncoderXsd(propertiesFilename,
                copybookFilename, outputXSDFile.getAbsolutePath());
            return;
        }

        boolean toDecode = args[0].toUpperCase().startsWith("D");
        String xsdFilename = args[1];
        String inputFilename = args[2];
        String outputFilename = args[3];
        String encodingStyle = CocoEncoderProvider.STYLE_ID;
        String rootQName = args.length > 4 ? args[4]
                : "{}GDS-OUTPUT-BUFFER";
        String loggerPkgName = "com.sun.encoder.coco";
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

    /**
     * 
     * @param propertiesFilename
     * @param copybookFilename
     * @param outputXsdFilename
     * @throws Exception
     */
    public static void buildEncoderXsd(String propertiesFilename,
            String copybookFilename, String outputXsdFilename)
            throws Exception {
        Properties props = new Properties();
        if (propertiesFilename != null) {
            props.load(new FileInputStream(propertiesFilename));
        }
        CocoXsdBuilderSpec buildSpec = new CocoXsdBuilderSpec();
        buildSpec.setCheckNamesForReservedWords(
                Boolean.valueOf(
                    propertiesFilename == null ? "true" :
                        props.getProperty(
                                CocoXsdBuilderSpec
                                    .CHECK_NAMES_FOR_RESERVED_WORDS)));
        buildSpec.setCopybookCharEncoding(
            propertiesFilename == null ? "ASCII" :
                props.getProperty(
                        CocoXsdBuilderSpec.COPYBOOK_CHAR_ENCODING));
        buildSpec.setDisplay1CharEncoding(
            propertiesFilename == null ? "ASCII" :
                props.getProperty(
                        CocoXsdBuilderSpec.DISPLAY1_CHAR_ENCODING));
        buildSpec.setDisplayCharEncoding(
            propertiesFilename == null ? "ASCII" :
                props.getProperty(
                        CocoXsdBuilderSpec.DISPLAY_CHAR_ENCODING));
        buildSpec.setIgnoreContentBeyondCol72(
                Boolean.valueOf(
                    propertiesFilename == null ? "true" :
                        props.getProperty(
                                CocoXsdBuilderSpec
                                    .IGNORE_CONTENT_BEYOND_COLUMN72)));
        buildSpec.setTargetNamespace(
                props.getProperty(
                        CocoXsdBuilderSpec.TARGET_NAMESPACE));
        if (props.containsKey(
                CocoXsdBuilderSpec.PREDECODE_CHAR_CODING)) {
            buildSpec.setPreDecodeCharCoding(
                    props.getProperty(
                            CocoXsdBuilderSpec.PREDECODE_CHAR_CODING));
        }
        if (props.containsKey(
                CocoXsdBuilderSpec.POSTENCODE_CHAR_CODING)) {
            buildSpec.setPostEncodeCharCoding(
                    props.getProperty(
                            CocoXsdBuilderSpec.POSTENCODE_CHAR_CODING));
        }
        buildSpec.setCopybookLocation(copybookFilename);
        buildSpec.setXsdLocation(outputXsdFilename);
        System.out.println("buildSpec: " + buildSpec);
        CocoXsdBuilder builder = new CocoXsdBuilder();
        builder.setOtdBuilderSpec(buildSpec);
        builder.buildXsd();

    }
}
