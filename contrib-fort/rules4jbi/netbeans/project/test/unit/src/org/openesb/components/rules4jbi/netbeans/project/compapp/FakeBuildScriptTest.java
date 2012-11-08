/*
 * @(#)FakeBuildScriptTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:21 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.project.compapp;

import org.openesb.components.rules4jbi.shared.util.XOMUtils;
import nu.xom.Element;
import org.junit.Test;
//import static org.junit.Assert.*;
import static nu.xom.tests.XOMTestCase.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:21 $
 * 
 * @since 0.1
 */
public class FakeBuildScriptTest {

    @Test
    public void testCreateFakeBuildScript() {
        String expected = "<project name='foo' default='" + FakeBuildScript.DIST_TARGET_NAME + "' basedir='.'>"
                + "<target name='" + FakeBuildScript.CLEAN_TARGET_NAME + "'>" 
                + "<echo message='" + FakeBuildScript.CLEAN_TARGET_MESSAGE + "'/></target>"
                + "<target name='" + FakeBuildScript.DIST_TARGET_NAME + "'>" 
                + "<echo message='" + FakeBuildScript.DIST_TARGET_MESSAGE + "'/></target>"
                + "</project>";

        Element result = FakeBuildScript.createFakeBuildScript("foo");

        assertEquals(XOMUtils.toElement(expected), result);
    }

    @Test
    public void testCreateTarget() {
        String expected = "<target name='clean'><echo message='Cleaning project'/></target>";

        Element result = FakeBuildScript.createTarget("clean", "Cleaning project");

        assertEquals(XOMUtils.toElement(expected), result);


        expected = "<target name='dist'><echo message='Creating distribution'/></target>";

        result = FakeBuildScript.createTarget("dist", "Creating distribution");

        assertEquals(XOMUtils.toElement(expected), result);
    }
}
