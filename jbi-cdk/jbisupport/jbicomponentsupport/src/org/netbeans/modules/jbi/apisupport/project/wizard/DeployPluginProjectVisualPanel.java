/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 *
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */


package org.netbeans.modules.jbi.apisupport.project.wizard;

import java.io.File;
import javax.swing.JPanel;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.Document;
import org.netbeans.modules.jbi.apisupport.common.wizard.ProjectLocationVisualPanel;
import org.netbeans.modules.jbi.apisupport.common.wizard.AbstractWizardModel;
import org.netbeans.modules.jbi.apisupport.common.wizard.AbstractWizardVisualPanel;
import org.openide.WizardDescriptor;
import org.openide.WizardValidationException;
import org.openide.util.NbBundle;

/** First panel in the NewProject wizard. Used for filling in
 * name, and directory of the project.
 *
 * @author chikkala
 */
public class DeployPluginProjectVisualPanel extends AbstractWizardVisualPanel {
    
    private boolean ignoreProjectDirChanges;
    
    private boolean ignoreAntProjectNameChanges;
    
    private boolean noDir = true;
    
    private ProjectLocationVisualPanel projectLocationPanel;
    private AbstractWizardVisualPanel optionsPanel;
    
    /** Creates new form PanelInitProject */
    public DeployPluginProjectVisualPanel(DeployPluginProjectPanel panel, String compType) {
        super(panel);
        initComponents();
        
        setName(NbBundle.getMessage(DeployPluginProjectVisualPanel.class,"TXT_NameAndLoc")); // NOI18N
        
        projectLocationPanel = new ProjectLocationVisualPanel(panel);
        // putClientProperty("NewProjectWizard_Title", NbBundle.getMessage(BasicJbiComponentProjectVisualPanel.class,"TXT_NewASA")); // NOI18N
        jSeparator1.setVisible(true);
        //getAccessibleContext().setAccessibleName(NbBundle.getMessage(BasicJbiComponentProjectVisualPanel.class,"TXT_NewASA")); // NOI18N
        //getAccessibleContext().setAccessibleDescription(NbBundle.getMessage(BasicJbiComponentProjectVisualPanel.class,"ACSD_NewASA")); // NOI18N
        
        locationContainer.add( projectLocationPanel, java.awt.BorderLayout.CENTER );
        
        if ( DeployPluginWizardModel.BINDING_COMPONENT_COMP_TYPE.equals(compType)) {
            BCDPCompDescriptionVisualPanel bcVisualPanel = new BCDPCompDescriptionVisualPanel(panel);
            projectLocationPanel.addPropertyChangeListener(bcVisualPanel);
            optionsPanel = bcVisualPanel;
        } else if (DeployPluginWizardModel.SERVICE_ENGINE_COMP_TYPE.equals(compType)) {
            SEDPCompDescriptionVisualPanel seVisualPanel = new SEDPCompDescriptionVisualPanel(panel);
            projectLocationPanel.addPropertyChangeListener(seVisualPanel);
            optionsPanel = seVisualPanel;
        }        
        
        optionsContainer.add( optionsPanel, java.awt.BorderLayout.CENTER );
    }
    
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents()//GEN-BEGIN:initComponents
    {
        java.awt.GridBagConstraints gridBagConstraints;

        locationContainer = new javax.swing.JPanel();
        jSeparator1 = new javax.swing.JSeparator();
        optionsContainer = new javax.swing.JPanel();

        setLayout(new java.awt.GridBagLayout());

        locationContainer.setLayout(new java.awt.BorderLayout());

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        add(locationContainer, gridBagConstraints);
        locationContainer.getAccessibleContext().setAccessibleName(null);
        locationContainer.getAccessibleContext().setAccessibleDescription(null);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(12, 0, 12, 0);
        add(jSeparator1, gridBagConstraints);

        optionsContainer.setLayout(new java.awt.BorderLayout());

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.gridheight = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(optionsContainer, gridBagConstraints);
        optionsContainer.getAccessibleContext().setAccessibleName(null);
        optionsContainer.getAccessibleContext().setAccessibleDescription(null);

    }//GEN-END:initComponents
    
    /** Currently only handles the "Browse..." button
     */
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JPanel locationContainer;
    private javax.swing.JPanel optionsContainer;
    // End of variables declaration//GEN-END:variables
    
    
    public void loadFromWizardModel(AbstractWizardModel wizModel) {
        optionsPanel.loadFromWizardModel( wizModel );
        projectLocationPanel.loadFromWizardModel( wizModel );
        
    }
    
    public void saveToWizardModel(AbstractWizardModel wizModel) {
        projectLocationPanel.saveToWizardModel( wizModel );
        optionsPanel.saveToWizardModel( wizModel );
    }
    
    public void validateVisualPanel(AbstractWizardModel wizModel) throws WizardValidationException {
        projectLocationPanel.validateVisualPanel(wizModel);
    }
    
    public boolean isValidVisualPanel(AbstractWizardModel wizModel) {
        
        WizardDescriptor wiz = wizModel.getWizardDescriptor();
        wiz.putProperty( "WizardPanel_errorMessage", "" ); //NOI18N
        
        return projectLocationPanel.isValidVisualPanel( wizModel )
        && optionsPanel.isValidVisualPanel(wizModel);
    }
    
    
}
