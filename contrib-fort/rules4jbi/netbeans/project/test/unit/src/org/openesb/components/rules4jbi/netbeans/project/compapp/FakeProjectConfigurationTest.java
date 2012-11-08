/*
 * @(#)FakeProjectConfigurationTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:21 $
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
public class FakeProjectConfigurationTest {

    @Test
    public void testCreateFakeProjectConfiguration() {
        String expected = "<project><name>aProjectName</name></project>";
        
        Element result = FakeProjectConfiguration.createFakeProjectConfiguration("aProjectName");
        
        assertEquals(XOMUtils.toElement(expected), result);
        
        
        expected = "<project><name>FooBar</name></project>";
        
        result = FakeProjectConfiguration.createFakeProjectConfiguration("FooBar");
        
        assertEquals(XOMUtils.toElement(expected), result);
    }
}
