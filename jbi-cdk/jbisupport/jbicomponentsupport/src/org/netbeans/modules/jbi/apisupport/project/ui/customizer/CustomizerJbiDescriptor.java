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

package org.netbeans.modules.jbi.apisupport.project.ui.customizer;

import java.awt.Dialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.netbeans.api.project.Project;
import org.netbeans.modules.j2ee.deployment.devmodules.api.ServerManager;
import org.netbeans.modules.jbi.apisupport.JbiAdminSettings;
import org.netbeans.modules.jbi.apisupport.common.JbiDescriptor;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProject;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProjectUtil;
import org.netbeans.modules.jbi.apisupport.project.SourceRoots;
import org.openide.DialogDescriptor;
import org.openide.DialogDisplayer;
import org.openide.awt.MouseUtils;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;


/**
 *
 * @author  chikkala
 */
public class CustomizerJbiDescriptor extends JPanel implements HelpCtx.Provider {
    
    private JbiCompProject project;
    CustomizerUIModel mUIModel;
    
    public CustomizerJbiDescriptor( CustomizerUIModel uiProperties ) {
        this.mUIModel = uiProperties;
        this.project = uiProperties.getProject();
        
        initComponents();
        
        initComponentType(uiProperties.JBI_COMPONENT_TYPE_MODEL);
        initCLDelegation();
        
        this.mNameTF.setDocument(uiProperties.JBI_COMPONENT_NAME_MODEL);
        this.mDescriptionTA.setDocument(uiProperties.JBI_COMPONENT_DESC_MODEL);
        
        this.mBootstrapClassNameTF.setDocument(uiProperties.JBI_COMPONENT_BT_CLASS_MODEL);
        this.mCompClassNameTF.setDocument(uiProperties.JBI_COMPONENT_CLASS_MODEL);
        
////        this.mCompClassBrowseBTN.addActionListener( 
////                new ComponentClassBrowserListener( project.getSourceRoots(), this.mCompClassNameTF, "javax.jbi.component.Component" ) );
////
////        this.mBootstrapClassBrowseBTN.addActionListener( 
////                new BootstrapClassBrowserListener( project.getSourceRoots(), this.mBootstrapClassNameTF, "javax.jbi.component.Bootstrap" ) );
////        
        this.mCompClassBrowseBTN.addActionListener( 
                new ComponentClassBrowserListener( project, this.mCompClassNameTF, "javax.jbi.component.Component" ) );
        
        this.mBootstrapClassBrowseBTN.addActionListener( 
                new BootstrapClassBrowserListener( project, this.mBootstrapClassNameTF, "javax.jbi.component.Bootstrap" ) );
        
    }
    
    public HelpCtx getHelpCtx() {
        return new HelpCtx( CustomizerRun.class );
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        mComponentTypeLBL = new javax.swing.JLabel();
        mComponentTypeValueLBL = new javax.swing.JLabel();
        mNameLBL = new javax.swing.JLabel();
        mNameTF = new javax.swing.JTextField();
        mDescriptionLBL = new javax.swing.JLabel();
        mDescriptionScrollPane = new javax.swing.JScrollPane();
        mDescriptionTA = new javax.swing.JTextArea();
        mCompClassNameLBL = new javax.swing.JLabel();
        mCompClassNameTF = new javax.swing.JTextField();
        mCompClassBrowseBTN = new javax.swing.JButton();
        mBootstrapClassNameLBL = new javax.swing.JLabel();
        mBootstrapClassNameTF = new javax.swing.JTextField();
        mBootstrapClassBrowseBTN = new javax.swing.JButton();
        mClassLoaderDelegationPanel = new javax.swing.JPanel();
        mCompClassLoaderLBL = new javax.swing.JLabel();
        mCompClassLoaderComboBox = new javax.swing.JComboBox();
        mBootstrapClassLoaderLBL = new javax.swing.JLabel();
        mBootstrapClassloaderComboBox = new javax.swing.JComboBox();
        mFillLBL = new javax.swing.JLabel();

        setLayout(new java.awt.GridBagLayout());

        mComponentTypeLBL.setText(org.openide.util.NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.componentTypeLBL")); // NOI18N
        mComponentTypeLBL.setToolTipText("");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 5;
        gridBagConstraints.ipady = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 2, 2, 4);
        add(mComponentTypeLBL, gridBagConstraints);

        mComponentTypeValueLBL.setText(org.openide.util.NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.componentType.Engine")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 5;
        gridBagConstraints.ipady = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(12, 2, 2, 4);
        add(mComponentTypeValueLBL, gridBagConstraints);

        mNameLBL.setHorizontalAlignment(javax.swing.SwingConstants.LEFT);
        mNameLBL.setText(org.openide.util.NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.nameLBL")); // NOI18N
        mNameLBL.setToolTipText("");
        mNameLBL.setHorizontalTextPosition(javax.swing.SwingConstants.LEFT);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 5;
        gridBagConstraints.ipady = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 4);
        add(mNameLBL, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 5;
        gridBagConstraints.ipady = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 4);
        add(mNameTF, gridBagConstraints);

        mDescriptionLBL.setHorizontalAlignment(javax.swing.SwingConstants.LEFT);
        mDescriptionLBL.setText(org.openide.util.NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.descriptionLBL")); // NOI18N
        mDescriptionLBL.setToolTipText("");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 5;
        gridBagConstraints.ipady = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 4);
        add(mDescriptionLBL, gridBagConstraints);

        mDescriptionScrollPane.setPreferredSize(new java.awt.Dimension(4, 40));

        mDescriptionTA.setFont(new java.awt.Font("SansSerif", 0, 12));
        mDescriptionTA.setMargin(new java.awt.Insets(1, 1, 1, 1));
        mDescriptionScrollPane.setViewportView(mDescriptionTA);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 5;
        gridBagConstraints.ipady = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 4);
        add(mDescriptionScrollPane, gridBagConstraints);

        mCompClassNameLBL.setHorizontalAlignment(javax.swing.SwingConstants.LEFT);
        mCompClassNameLBL.setText(org.openide.util.NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.component.class.name.LBL")); // NOI18N
        mCompClassNameLBL.setToolTipText("");
        mCompClassNameLBL.setHorizontalTextPosition(javax.swing.SwingConstants.LEFT);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 5;
        gridBagConstraints.ipady = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 2, 2, 4);
        add(mCompClassNameLBL, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 5;
        gridBagConstraints.ipady = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(12, 2, 2, 4);
        add(mCompClassNameTF, gridBagConstraints);

        mCompClassBrowseBTN.setText(org.openide.util.NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.bootstrap.class.browse.BTN")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.insets = new java.awt.Insets(12, 2, 2, 4);
        add(mCompClassBrowseBTN, gridBagConstraints);

        mBootstrapClassNameLBL.setHorizontalAlignment(javax.swing.SwingConstants.LEFT);
        mBootstrapClassNameLBL.setText(org.openide.util.NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.bootstrap.class.name.LBL")); // NOI18N
        mBootstrapClassNameLBL.setToolTipText("");
        mBootstrapClassNameLBL.setHorizontalTextPosition(javax.swing.SwingConstants.LEFT);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 5;
        gridBagConstraints.ipady = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 2, 2, 4);
        add(mBootstrapClassNameLBL, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 5;
        gridBagConstraints.ipady = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 2, 2, 4);
        add(mBootstrapClassNameTF, gridBagConstraints);

        mBootstrapClassBrowseBTN.setText(org.openide.util.NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.component.class.browse.BTN")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.insets = new java.awt.Insets(8, 2, 2, 4);
        add(mBootstrapClassBrowseBTN, gridBagConstraints);

        mClassLoaderDelegationPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(org.openide.util.NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.class.loader.LBL"))); // NOI18N
        mClassLoaderDelegationPanel.setLayout(new java.awt.GridBagLayout());

        mCompClassLoaderLBL.setText(org.openide.util.NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.comp.class.loader.LBL")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 2, 4, 4);
        mClassLoaderDelegationPanel.add(mCompClassLoaderLBL, gridBagConstraints);

        mCompClassLoaderComboBox.setModel(createCLDelegationComboBoxModel());
        mCompClassLoaderComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                mCompClassLoaderComboBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        mClassLoaderDelegationPanel.add(mCompClassLoaderComboBox, gridBagConstraints);

        mBootstrapClassLoaderLBL.setText(org.openide.util.NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.bootstrap.class.loader.LBL")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 2, 4, 4);
        mClassLoaderDelegationPanel.add(mBootstrapClassLoaderLBL, gridBagConstraints);

        mBootstrapClassloaderComboBox.setModel(createCLDelegationComboBoxModel());
        mBootstrapClassloaderComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                mBootstrapClassloaderComboBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        mClassLoaderDelegationPanel.add(mBootstrapClassloaderComboBox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        add(mClassLoaderDelegationPanel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(mFillLBL, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void mCompClassLoaderComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_mCompClassLoaderComboBoxActionPerformed
        // TODO add your handling code here:
        int idx = this.mCompClassLoaderComboBox.getSelectedIndex();
        updateComponentCLDelegation(idx);
    }//GEN-LAST:event_mCompClassLoaderComboBoxActionPerformed

    private void mBootstrapClassloaderComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_mBootstrapClassloaderComboBoxActionPerformed
        // TODO add your handling code here:
        int idx = this.mBootstrapClassloaderComboBox.getSelectedIndex();
        updateBootstrapCLDelegation(idx);        
    }//GEN-LAST:event_mBootstrapClassloaderComboBoxActionPerformed
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton mBootstrapClassBrowseBTN;
    private javax.swing.JLabel mBootstrapClassLoaderLBL;
    private javax.swing.JLabel mBootstrapClassNameLBL;
    private javax.swing.JTextField mBootstrapClassNameTF;
    private javax.swing.JComboBox mBootstrapClassloaderComboBox;
    private javax.swing.JPanel mClassLoaderDelegationPanel;
    private javax.swing.JButton mCompClassBrowseBTN;
    private javax.swing.JComboBox mCompClassLoaderComboBox;
    private javax.swing.JLabel mCompClassLoaderLBL;
    private javax.swing.JLabel mCompClassNameLBL;
    private javax.swing.JTextField mCompClassNameTF;
    private javax.swing.JLabel mComponentTypeLBL;
    private javax.swing.JLabel mComponentTypeValueLBL;
    private javax.swing.JLabel mDescriptionLBL;
    private javax.swing.JScrollPane mDescriptionScrollPane;
    private javax.swing.JTextArea mDescriptionTA;
    private javax.swing.JLabel mFillLBL;
    private javax.swing.JLabel mNameLBL;
    private javax.swing.JTextField mNameTF;
    // End of variables declaration//GEN-END:variables
        
    private void setDocumentText(Document doc, String text) {
        try {
            doc.remove(0, doc.getLength());
            doc.insertString(0, text, null);
        } catch (BadLocationException ex) {
            ex.printStackTrace();
            // ingore
        }
    }
    
    private String getDocumentText(Document doc) {
        try {
            return doc.getText(0, doc.getLength());
        } catch (BadLocationException ex) {
            ex.printStackTrace();
            return "";
        }
    }
    
    private void initComponentType(Document doc) {
        String type = getDocumentText(doc);
        String typeValue = type;
        if ( CustomizerUIModel.SERVICE_ENGINE_COMP_TYPE.equals(type)) {
            typeValue = org.openide.util.NbBundle.getMessage(
                    CustomizerJbiDescriptor.class, "CustomizerCompDesc.componentType.Engine");
        } else if (  CustomizerUIModel.BINDING_COMPONENT_COMP_TYPE.equals(type) ) {
            typeValue = org.openide.util.NbBundle.getMessage(
                    CustomizerJbiDescriptor.class, "CustomizerCompDesc.componentType.Binding");
        } else {
            typeValue = type;
        }
        this.mComponentTypeValueLBL.setText(typeValue);
    }
    
    private ComboBoxModel createCLDelegationComboBoxModel() {
        String defaultValue = "Default";
        String parentFirstValue = "Parent First";
        String selfFirstValue = "Self First";
        String[] items = {defaultValue, parentFirstValue, selfFirstValue};
        return new DefaultComboBoxModel(items);
    }
    
    private int getCLDelegationIndex(String clDelegation) {
        int i = 0;
        if ( JbiDescriptor.PARENT_FIRST_CL_DELEGATION.equals(clDelegation)) {
            i = 1;
        } else if ( JbiDescriptor.SELF_FIRST_CL_DELEGATION.equals(clDelegation)) {
            i = 2;
        }
        return i;
    }
    private void initCLDelegation() {
        int compClDelegationIdx = getCLDelegationIndex(this.mUIModel.mCompCLDelegate);
        int bootstrapCLDelegationIdx = getCLDelegationIndex(this.mUIModel.mBootstrapCLDelegate);
        
        this.mCompClassLoaderComboBox.setSelectedIndex(compClDelegationIdx);
        this.mBootstrapClassloaderComboBox.setSelectedIndex(bootstrapCLDelegationIdx);
    }    
    private String getCLDelegationValue(int idx) {
        String clDelegation = null;
        if ( idx == 1) {
                clDelegation = JbiDescriptor.PARENT_FIRST_CL_DELEGATION;    
        } else if ( idx == 2) {
            clDelegation = JbiDescriptor.SELF_FIRST_CL_DELEGATION;
        }
        return clDelegation;
    }
    private void updateComponentCLDelegation(int idx) {
        String clDelegation = getCLDelegationValue(idx);
        this.mUIModel.mCompCLDelegate = clDelegation;
        
    }
    private void updateBootstrapCLDelegation(int idx) {
        String clDelegation = getCLDelegationValue(idx);
        this.mUIModel.mBootstrapCLDelegate = clDelegation;
    }
    
    /**
     * selects the class that implements the interface.
     */
    private static class ClassBrowserListener implements ActionListener /*, DocumentListener */ {
        
        private final JButton okButton;
        private SourceRoots sourceRoots;
        private JTextField mClassNameTextField;
        private String mInterfaceName;
        protected JbiCompProject mPrj = null;
        
        public ClassBrowserListener( JbiCompProject project, JTextField classNameTextField, String interfaceName ) {
            this(project.getSourceRoots(), classNameTextField, interfaceName);
            this.mPrj = project;            
        }
        
        public ClassBrowserListener( SourceRoots sourceRoots, JTextField classNameTextField, String interfaceName ) {
            this.sourceRoots = sourceRoots;
            this.mClassNameTextField = classNameTextField;
            this.mInterfaceName = interfaceName;
            this.okButton  = new JButton("Select"); //NOI18N
            this.initComponents();
        }
        protected void initClassChooserOKButton(JButton okButton) {
            okButton.setText(NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.select.class.BTN"));
            // okButton.getAccessibleContext().setAccessibleDescription(NbBundle.getMessage (CustomizerRun.class, "AD.CustomizerCompDesc.select.class.BTN"));
        }
        protected String getClassChooserTitle() {
            return NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.classChooser.title" );
        }
        protected String getClassChooserSubTitle() {
            return "classes";
        }
        
        protected void initComponents() {
            initClassChooserOKButton(this.okButton);
        }
        // Implementation of ActionListener ------------------------------------
        
        /** Handles button events
         */
        public void actionPerformed( ActionEvent e ) {
            
            // only chooseMainClassButton can be performed
            
//            if ( this.mPrj != null ) {
//                JbiCompProjectUtil.listImplementedClasses(this.mPrj, this.mInterfaceName);
//            }
            
            final CompClassChooser panel = new CompClassChooser(
                    sourceRoots.getRoots(), this.getClassChooserSubTitle(), mInterfaceName);
            Object[] options = new Object[] {
                okButton,
                DialogDescriptor.CANCEL_OPTION
            };
            panel.addChangeListener(new ChangeListener() {
                public void stateChanged(ChangeEvent e) {
                    if (e.getSource() instanceof MouseEvent && MouseUtils.isDoubleClick(((MouseEvent)e.getSource()))) {
                        // click button and finish the dialog with selected class
                        okButton.doClick();
                    } else {
                        okButton.setEnabled(panel.getSelectedClass() != null);
                    }
                }
            });
            okButton.setEnabled(false);
            DialogDescriptor desc = new DialogDescriptor(
                    panel,
                    getClassChooserTitle(),
                    true,
                    options,
                    options[0],
                    DialogDescriptor.BOTTOM_ALIGN,
                    null,
                    null);
            //desc.setMessageType (DialogDescriptor.INFORMATION_MESSAGE);
            Dialog dlg = DialogDisplayer.getDefault().createDialog(desc);
            dlg.setVisible(true);
            if (desc.getValue() == options[0]) {
                mClassNameTextField.setText(panel.getSelectedClass());
            }
            dlg.dispose();
        }
    }
    /**
     * selects the class that implements the interface.
     */
    private static class ComponentClassBrowserListener extends ClassBrowserListener /*, DocumentListener */ {

        public ComponentClassBrowserListener( JbiCompProject prj, JTextField classNameTextField, String interfaceName ) {
            super(prj, classNameTextField, interfaceName);
        }
        
        public ComponentClassBrowserListener( SourceRoots sourceRoots, JTextField classNameTextField, String interfaceName ) {
            super(sourceRoots, classNameTextField, interfaceName);
        }
        
        protected void initClassChooserOKButton(JButton okButton) {
            okButton.setText(NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.component.class.select.BTN"));
        }
        
        protected String getClassChooserTitle() {
            return NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.component.class.chooser.title" );
        }
        
        protected String getClassChooserSubTitle() {
            return "Component implementation classes:";
        }
        
    }

    /**
     * selects the class that implements the interface.
     */
    private static class BootstrapClassBrowserListener extends ClassBrowserListener /*, DocumentListener */ {
        
        public BootstrapClassBrowserListener( JbiCompProject prj, JTextField classNameTextField, String interfaceName ) {
            super(prj, classNameTextField, interfaceName);
        }
        
        public BootstrapClassBrowserListener( SourceRoots sourceRoots, JTextField classNameTextField, String interfaceName ) {
            super(sourceRoots, classNameTextField, interfaceName);
        }
        
        protected void initClassChooserOKButton(JButton okButton) {
            okButton.setText(NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.bootstrap.class.select.BTN"));
        }
        
        protected String getClassChooserTitle() {
            return NbBundle.getMessage(CustomizerJbiDescriptor.class, "CustomizerCompDesc.bootstrap.class.chooser.title" );
        }
        protected String getClassChooserSubTitle() {
            return "Bootstrap implementation classes:";
        }
        
    }
            
}
