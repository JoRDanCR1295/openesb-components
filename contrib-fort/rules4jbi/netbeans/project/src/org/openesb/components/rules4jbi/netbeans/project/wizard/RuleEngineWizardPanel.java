/*
 * @(#)RuleEngineWizardPanel.java        $Revision: 1.3 $ $Date: 2008/12/17 23:21:35 $
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

package org.openesb.components.rules4jbi.netbeans.project.wizard;

import org.openesb.components.rules4jbi.netbeans.util.RulesEngineProvidersFinder;
import org.openesb.components.rules4jbi.netbeans.util.wizard.AbstractWizardPanel;
import java.util.ResourceBundle;
import org.openide.WizardDescriptor;
import org.openide.util.NbBundle;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/12/17 23:21:35 $
 * 
 * @since 0.1
 */
public class RuleEngineWizardPanel extends AbstractWizardPanel {
    
    private ResourceBundle resourceBundle = NbBundle.getBundle(RuleEngineWizardPanel.class);
    
    private RuleEngineVisualPanel component = null;
    
    @Override
    public RuleEngineVisualPanel getComponent() {
        if (component == null) {
            component = new RuleEngineVisualPanel();
            
            component.setProviderURIs(
                    RulesEngineProvidersFinder.getInstance().getRuleServiceProviderURIs());
            
            component.setProviderClassNames(
                    RulesEngineProvidersFinder.getInstance().getRuleServiceProviderClassNames());
            
            component.clearSelectedItems();
            
//            component.addDocumentListener(this);
        }
        
        return component;
    }

    @Override
    public String getName() {
        return resourceBundle.getString("RuleEnginePanel.name");
    }

    @Override
    public boolean isFinishPanel() {
        return true;
    }
    
    @Override
    protected boolean checkValidity() {
        
        /*
         * It's hard to add correct validation for this panel,
         * as the provider URI doesn't really need to be a valid URI
         * according to JSR 94. Maybe we could validate the class name,
         * whether it conforms to JLS, but that is an overkill.
         * It is certainly valid to leave the fields blank.
         */

        showErrorMessage(null);
        return true;
    }
    
    @Override
    public void readSettings(WizardDescriptor settings) {
        getComponent().selectProvider((String) settings.getProperty(Constants.PROPERTY_PROVIDER_URI));
        getComponent().selectProviderClass((String) settings.getProperty(Constants.PROPERTY_PROVIDER_CLASS));
        
        super.readSettings(settings);
    }

    public void storeSettings(WizardDescriptor settings) {
        settings.putProperty(Constants.PROPERTY_PROVIDER_URI, getComponent().getSelectedProvider());
        settings.putProperty(Constants.PROPERTY_PROVIDER_CLASS, getComponent().getSelectedProviderClass());
    }
}
