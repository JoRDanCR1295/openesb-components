/*
 * @(#)NameAndLocationVisualPanel.java        $Revision: 1.2 $ $Date: 2008/11/12 08:26:21 $
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

package org.openesb.components.rules4jbi.netbeans.project.fileimport;

import java.awt.Dimension;
import java.util.ResourceBundle;

import javax.swing.event.DocumentListener;

import org.openide.awt.Mnemonics;
import org.openide.util.NbBundle;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/11/12 08:26:21 $
 * 
 * @since 0.3
 */
public class NameAndLocationVisualPanel extends javax.swing.JPanel {

    private static final long serialVersionUID = -2449366228226639442L;

    private ResourceBundle resourceBundle = NbBundle.getBundle(NameAndLocationVisualPanel.class);

    private NameAndLocationWizardPanel controller;
    
    /** Creates new form NameAndLocationVisualPanel */
    public NameAndLocationVisualPanel(NameAndLocationWizardPanel controller) {
        this.controller = controller;
        
        initComponents();
        
        Mnemonics.setLocalizedText(
                browseButton, resourceBundle.getString("NameAndLocationVisualPanel.browseButton.text"));
        
        setPreferredSize(new Dimension(
                Constants.PANEL_PREFERRED_SIZE_WIDTH, Constants.PANEL_PREFERRED_SIZE_HEIGHT));
    }
    
    @Override
    public String getName() {

        /* Using resourceBundle instance variable here causes NPE */
        return NbBundle.getMessage(FileTypeVisualPanel.class, "NameAndLocationPanel.name");
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        filePathLabel = new javax.swing.JLabel();
        filePathTextField = new javax.swing.JTextField();
        browseButton = new javax.swing.JButton();

        filePathLabel.setText(org.openide.util.NbBundle.getMessage(NameAndLocationVisualPanel.class, "NameAndLocationVisualPanel.filePathLabel.text")); // NOI18N

        filePathTextField.setEditable(false);
        filePathTextField.setText(org.openide.util.NbBundle.getMessage(NameAndLocationVisualPanel.class, "NameAndLocationVisualPanel.filePathTextField.text")); // NOI18N

        browseButton.setText(org.openide.util.NbBundle.getMessage(NameAndLocationVisualPanel.class, "NameAndLocationVisualPanel.browseButton.text")); // NOI18N
        browseButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                browseButtonActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(filePathLabel)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(filePathTextField, javax.swing.GroupLayout.DEFAULT_SIZE, 230, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(browseButton)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(filePathLabel)
                    .addComponent(browseButton)
                    .addComponent(filePathTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

private void browseButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_browseButtonActionPerformed
    
    controller.handleBrowseButtonPressed();
    
}//GEN-LAST:event_browseButtonActionPerformed
    
    void addDocumentListener(DocumentListener listener) {
        filePathTextField.getDocument().addDocumentListener(listener);
    }
    
    String getFilePath() {
        return filePathTextField.getText();
    }
    
    void setFilePath(String filePath) {
        filePathTextField.setText(filePath);
    }
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton browseButton;
    private javax.swing.JLabel filePathLabel;
    private javax.swing.JTextField filePathTextField;
    // End of variables declaration//GEN-END:variables
}
