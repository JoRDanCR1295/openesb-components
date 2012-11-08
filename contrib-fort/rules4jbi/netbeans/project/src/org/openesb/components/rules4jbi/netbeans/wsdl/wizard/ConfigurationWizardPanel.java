/*
 * @(#)ConfigurationWizardPanel.java        $Revision: 1.2 $ $Date: 2008/11/24 12:47:18 $
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

import org.openesb.components.rules4jbi.netbeans.util.wizard.AbstractWizardPanel;
import org.openesb.components.rules4jbi.netbeans.util.Validator;
import java.util.ResourceBundle;
import org.openide.WizardDescriptor;
import org.openide.util.NbBundle;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/11/24 12:47:18 $
 * 
 * @since 0.1
 */
public class ConfigurationWizardPanel extends AbstractWizardPanel {

    private ResourceBundle resourceBundle = NbBundle.getBundle(ConfigurationWizardPanel.class);
    
    private ConfigurationVisualPanel component = null;
    
    @Override
    public ConfigurationVisualPanel getComponent() {
        if (component == null) {
            component = new ConfigurationVisualPanel();
            
            component.putClientProperty("WizardPanel_contentSelectedIndex", 1);
            
            component.addDocumentListener(this);
        }
        
        return component;
    }

    @Override
    public String getName() {
        return resourceBundle.getString("ConfigurationPanel.name");
    }

    @Override
    protected boolean checkValidity() {
        if (!Validator.isValidNCName(getComponent().getDefinitionsName())) {
            showErrorMessage(resourceBundle.getString(
                    "ConfigurationWizardPanel.invalid.definitions.name.error.message"));
            
            return false;
        }
        
        if (!Validator.isValidURI(getComponent().getTargetNamespace())) {
            showErrorMessage(resourceBundle.getString(
                    "ConfigurationWizardPanel.invalid.target.namespace.error.message"));
            
            return false;
        }

        if (!Validator.isValidNCName(getComponent().getPortTypeName())) {
            showErrorMessage(resourceBundle.getString(
                    "ConfigurationWizardPanel.invalid.port.type.name.error.message"));
            
            return false;
        }
        
        if (!Validator.isValidNCName(getComponent().getServiceName())) {
            showErrorMessage(resourceBundle.getString(
                    "ConfigurationWizardPanel.invalid.service.name.error.message"));
            
            return false;
        }
        
        if (!Validator.isValidNCName(getComponent().getPortName())) {
            showErrorMessage(resourceBundle.getString(
                    "ConfigurationWizardPanel.invalid.port.name.error.message"));
            
            return false;
        }
        
        if (!Validator.isValidNCName(getComponent().getPartnerLinkTypeName())) {
            showErrorMessage(resourceBundle.getString(
                    "ConfigurationWizardPanel.invalid.partner.link.type.name.error.message"));
            
            return false;
        }
        
        if (!Validator.isValidNCName(getComponent().getPartnerLinkRoleName())) {
            showErrorMessage(resourceBundle.getString(
                    "ConfigurationWizardPanel.invalid.partner.link.role.name.error.message"));
            
            return false;
        }
        
        showErrorMessage(null);
        return true;
    }
    
    @Override
    public void readSettings(WizardDescriptor settings) {
        getComponent().setDefinitionsName((String) settings.getProperty(Constants.PROPERTY_DEFINITIONS_NAME));
        getComponent().setTargetNamespace((String) settings.getProperty(Constants.PROPERTY_TARGET_NAMESPACE));
        getComponent().setPortTypeName((String) settings.getProperty(Constants.PROPERTY_PORT_TYPE_NAME));
        getComponent().setServiceName((String) settings.getProperty(Constants.PROPERTY_SERVICE_NAME));
        getComponent().setPortName((String) settings.getProperty(Constants.PROPERTY_PORT_NAME));
        
        getComponent().setPartnerLinkTypeName(
                (String) settings.getProperty(Constants.PROPERTY_PARTNER_LINK_TYPE_NAME));
        getComponent().setPartnerLinkRoleName(
                (String) settings.getProperty(Constants.PROPERTY_PARTNER_LINK_ROLE_NAME));
        
        super.readSettings(settings);
    }

    public void storeSettings(WizardDescriptor settings) {
        settings.putProperty(Constants.PROPERTY_DEFINITIONS_NAME, getComponent().getDefinitionsName());
        settings.putProperty(Constants.PROPERTY_TARGET_NAMESPACE, getComponent().getTargetNamespace());
        settings.putProperty(Constants.PROPERTY_PORT_TYPE_NAME, getComponent().getPortTypeName());
        settings.putProperty(Constants.PROPERTY_SERVICE_NAME, getComponent().getServiceName());
        settings.putProperty(Constants.PROPERTY_PORT_NAME, getComponent().getPortName());
        
        settings.putProperty(Constants.PROPERTY_PARTNER_LINK_TYPE_NAME, getComponent().getPartnerLinkTypeName());
        settings.putProperty(Constants.PROPERTY_PARTNER_LINK_ROLE_NAME, getComponent().getPartnerLinkRoleName());
    }
}
