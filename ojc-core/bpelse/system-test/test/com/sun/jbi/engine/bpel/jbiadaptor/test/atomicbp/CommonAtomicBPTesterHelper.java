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
 * @(#)CommonPersistenceTesterHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.atomicbp;

import java.io.File;
import java.net.URI;
import java.net.URL;
import java.util.Properties;

import junit.framework.TestCase;

/**
 *
 * @author Sun Microsystems
 */
public class CommonAtomicBPTesterHelper {

    public static void commonCode(String folder, String propertiesFilename) throws Exception {
        URL url = CommonAtomicBPTesterHelper.class.getResource(folder);
        String deployedFolderPath = folder + "/deployedFolder";
        //File testinputdir = new File(new URL(url.toExternalForm() + File.separator + "input").toURI());
        File testinputdir = new File(url.toURI().getPath() + File.separator + "input");


        File testfile = new File(testinputdir, propertiesFilename);
        if (!testfile.exists()) {
            TestCase.fail("Cannot execute test, file doesn't exist: "+ propertiesFilename);
        }

        URI propFilePath = testfile.toURI();
        cleanup(propFilePath);
        int status = execCommand(propFilePath, deployedFolderPath);
        TestCase.assertTrue(status == AtomicBPEngineDriver.SUCCESSFUL_EXECUTION);

    }

    private static void cleanup(URI propFilePath) {
        File propFile = new File(propFilePath);
        File parentDir = propFile.getParentFile().getParentFile();
        File tmpDir = new File(parentDir.getPath() + File.separator + "temp");

        if (tmpDir.isDirectory() && tmpDir.exists()) {
            File[] children = tmpDir.listFiles();

            for (int i = 0; i < children.length; i++) {
                children[i].delete();
            }

            tmpDir.delete();
        }
    }

    private static int execCommand(URI propFilePath, String deployedFolderPath)
        throws Exception {

        Properties props = new Properties();
        props.load(propFilePath.toURL().openStream());

        String funcParam1 = propFilePath.getPath();
        String funcParam2 = deployedFolderPath;
        return AtomicBPEngineDriver.main(new String[] {funcParam1, funcParam2, "8888", "bpelse.properties"});
    }
}
