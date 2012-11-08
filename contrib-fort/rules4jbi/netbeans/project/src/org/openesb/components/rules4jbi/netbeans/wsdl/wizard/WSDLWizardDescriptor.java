/*
 * @(#)WSDLWizardDescriptor.java        $Revision: 1.2 $ $Date: 2008/11/24 12:47:18 $
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

import org.openesb.components.rules4jbi.netbeans.wsdl.wizard.table.BusinessObjectsTableModel;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.netbeans.api.project.Project;
import org.openide.WizardDescriptor;
import org.openide.util.NbBundle;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/11/24 12:47:18 $
 * 
 * @since 0.1
 */
public class WSDLWizardDescriptor extends WizardDescriptor {
    
    private static final Logger logger = Logger.getLogger(WSDLWizardDescriptor.class.getName());
    
    private final BusinessObjectsWizardPanel panel1;
    
    private final ConfigurationWizardPanel panel2 = new ConfigurationWizardPanel();
    
    private final NameAndLocationWizardPanel panel3 = new NameAndLocationWizardPanel();

    public WSDLWizardDescriptor(final Project project) {
        panel1 = new BusinessObjectsWizardPanel(project);
        
        initProperties();
        
        List<Panel<WizardDescriptor>> panels = new ArrayList<Panel<WizardDescriptor>>();
        panels.add(panel1);
        panels.add(panel2);
        panels.add(panel3);
        
        /* At this point, the readSettings() method of the first wizard panel is called */
        setPanelsAndSettings(new ArrayIterator<WizardDescriptor>(panels), this);
        
        /* {0} will be replaced by WizardDesriptor.Panel.getComponent().getName() */
        setTitleFormat(new MessageFormat("{0}"));
        setTitle(NbBundle.getMessage(WSDLWizardDescriptor.class, "WSDLWizardDescriptor.title"));
    }
    
    private void initProperties() {
        logger.fine("Initializing properties");
        
        putProperty("WizardPanel_autoWizardStyle", Boolean.TRUE);
        putProperty("WizardPanel_contentDisplayed", Boolean.TRUE);
        putProperty("WizardPanel_contentNumbered", Boolean.TRUE);
        putProperty("WizardPanel_contentData", new String[] {panel1.getName(), panel2.getName(), panel3.getName()});
        
        putProperty(Constants.PROPERTY_DEFINITIONS_NAME, Constants.DEFAULT_DEFINITIONS_NAME);
        putProperty(Constants.PROPERTY_TARGET_NAMESPACE, Constants.DEFAULT_TARGET_NAMESPACE);
        putProperty(Constants.PROPERTY_PORT_TYPE_NAME, Constants.DEFAULT_PORT_TYPE_NAME);
        putProperty(Constants.PROPERTY_SERVICE_NAME, Constants.DEFAULT_SERVICE_NAME);
        putProperty(Constants.PROPERTY_PORT_NAME, Constants.DEFAULT_PORT_NAME);
        putProperty(Constants.PROPERTY_PARTNER_LINK_TYPE_NAME, Constants.DEFAULT_PARTNER_LINK_TYPE_NAME);
        putProperty(Constants.PROPERTY_PARTNER_LINK_ROLE_NAME, Constants.DEFAULT_PARTNER_LINK_ROLE_NAME);
        putProperty(Constants.PROPERTY_WSDL_FILE_NAME, Constants.DEFAULT_WSDL_FILE_NAME);
        
        putProperty(Constants.PROPERTY_BUSINESS_OBJECTS, new BusinessObjectsTableModel());
        
        logger.fine("Properties: " + getProperties().toString());
    }
}
