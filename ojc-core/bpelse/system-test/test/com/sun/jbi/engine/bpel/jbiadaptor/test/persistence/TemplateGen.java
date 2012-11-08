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
 * @(#)TemplateGen.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.persistence;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;

public class TemplateGen {

    /**
     * @param args
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 2) {
            System.out.println("Usage - TemplateGen <testCaseFolderName> <system-test-Location> \n");
            System.out.println("For example - TemplateGen WhileTest ${jbicomps_home}/bpelse/system-test");
            System.exit(1);
        }
        String bpelsFolderName = args[1];
        String testCaseFolderName = args[0];

        String bpelFolderString = bpelsFolderName + "/test/com/sun/jbi/engine/bpel/jbiadaptor/test/persistence/bpels";

        String testCaseFolderString = bpelFolderString + File.separator + testCaseFolderName;
        File testCaseFolder = new File(testCaseFolderString);
        if (testCaseFolder.exists()) {
            System.out.println("Provide a Unique test case folder name within the bpels folder.");
            System.exit(2);
        }
        testCaseFolder.mkdirs();

        String testCaseInputFolderString = testCaseFolderString + File.separator + "input";
        String testCaseOutputFolderString = testCaseFolderString + File.separator + "output";
        String testCaseDeplFolderString = testCaseFolderString + File.separator + "deployedFolder";

        File testCaseInputFolder = new File(testCaseInputFolderString);
        File testCaseOutputFolder = new File(testCaseOutputFolderString);
        File testCaseDeplFolder = new File(testCaseDeplFolderString);

        testCaseInputFolder.mkdirs();
        testCaseOutputFolder.mkdirs();
        testCaseDeplFolder.mkdirs();

        URL inputFileSrcURL = TemplateGen.class.getResource("template/input.xml");
        File ipFileSrc = new File(inputFileSrcURL.toURI());
        String ipFileDestString = testCaseInputFolderString + "/input.xml";
        File ipFileDest = new File(ipFileDestString);

        copy(ipFileSrc, ipFileDest);

        URL propsFileSrcURL = TemplateGen.class.getResource("template/test.properties");
        File propsFileSrc = new File(propsFileSrcURL.toURI());
        int index = testCaseFolderName.lastIndexOf("/");
        String testCaseName;
        if (index == -1) {
            testCaseName = testCaseFolderName;
        } else {
            testCaseName = testCaseFolderName.substring(index);
        }

        String propsFileDestString = testCaseInputFolderString + "/" + testCaseName + ".properties";
        File propsFileDest = new File(propsFileDestString);

        copy(propsFileSrc, propsFileDest);

        URL ouputFileSrcURL = TemplateGen.class.getResource("template/output.xml");
        File opFileSrc = new File(ouputFileSrcURL.toURI());
        String opFileDestString = testCaseOutputFolderString + "/output.xml";
        File opFileDest = new File(opFileDestString);

        copy(opFileSrc, opFileDest);

        URL outFileSrcURL = TemplateGen.class.getResource("template/test.out");
        File outFileSrc = new File(outFileSrcURL.toURI());
        String outFileDestString = testCaseOutputFolderString + "/" + testCaseName + ".out";
        File outFileDest = new File(outFileDestString);

        copy(outFileSrc, outFileDest);

        if (args.length == 3) {

            File deployedFolderSrc = new File(args[2]);
            copyRecursively(deployedFolderSrc, testCaseDeplFolder);
        }
    }


    static void copy(File src, File dst) throws IOException {
        InputStream in = new FileInputStream(src);
        OutputStream out = new FileOutputStream(dst);

        // Transfer bytes from in to out
        byte[] buf = new byte[1024];
        int len;
        while ((len = in.read(buf)) > 0) {
            out.write(buf, 0, len);
        }
        in.close();
        out.close();
    }

    static void copyRecursively(File src, File dst) throws IOException {
        File[] listFiles = src.listFiles();
        for (int i = 0; i < listFiles.length; i++) {
            File file = listFiles[i];
            if (file.isDirectory()) {
                String dirName = file.getName();
                String destDirString = dst.getAbsolutePath() + File.separator + dirName;
                File destDir = new File(destDirString);
                destDir.mkdir();
                copyRecursively(file, destDir);
            } else {
                String fileName = file.getName();
                String destFileString = dst.getAbsolutePath() + File.separator + fileName;
                File destFile = new File(destFileString);
                copy(file, destFile);
            }
        }
    }
}
