/*
 * @(#)NameAndLocationWizardPanelTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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

package org.openesb.components.rules4jbi.netbeans.wsdl.wizard;

import java.io.File;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public class NameAndLocationWizardPanelTest {

    @Test
    public void updateWSDLFileLocation() {
        NameAndLocationWizardPanel panel = new NameAndLocationWizardPanel();
        
        panel.initWSDLFileLocation("/tmp/test", "abc_rules");
        assertEquals("/tmp/test" + File.separator + "abc_rules.wsdl", panel.getWSDLFileLocation());
        
        panel.updateWSDLFileLocation("whatnot");
        assertEquals("/tmp/test" + File.separator + "whatnot.wsdl", panel.getWSDLFileLocation());

        panel.updateWSDLFileLocation("_");
        assertEquals("/tmp/test" + File.separator + "_.wsdl", panel.getWSDLFileLocation());
        
        panel.updateWSDLFileLocation("");
        assertEquals("/tmp/test" + File.separator + ".wsdl", panel.getWSDLFileLocation());
        
    }
}
