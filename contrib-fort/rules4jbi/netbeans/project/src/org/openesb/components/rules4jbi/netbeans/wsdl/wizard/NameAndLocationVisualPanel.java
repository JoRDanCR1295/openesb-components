/*
 * @(#)NameAndLocationVisualPanel.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
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

import java.awt.Dimension;
import java.util.ResourceBundle;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import org.openide.awt.Mnemonics;
import org.openide.util.NbBundle;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
public class NameAndLocationVisualPanel extends javax.swing.JPanel {
    
    private static final long serialVersionUID = -2449366228226639442L;
    
    /**
     * The '\n' character that is used internally by <code>JTextArea</code> to represent newlines.
     */ 
    private static final String NEWLINE = "\n";
    
    private ResourceBundle resourceBundle = NbBundle.getBundle(NameAndLocationVisualPanel.class);
    
    /** Creates new form NameAndLocationVisualPanel */
    public NameAndLocationVisualPanel() {
        initComponents();
        
        Mnemonics.setLocalizedText(fileNameLabel,
                resourceBundle.getString("NameAndLocationVisualPanel.fileNameLabel.text"));
        
        Mnemonics.setLocalizedText(createdFilesLabel,
                resourceBundle.getString("NameAndLocationVisualPanel.createdFilesLabel.text"));
        
        Mnemonics.setLocalizedText(modifiedFilesLabel,
                resourceBundle.getString("NameAndLocationVisualPanel.modifiedFilesLabel.text"));
        
        setPreferredSize(new Dimension(
                Constants.PANEL_PREFERRED_SIZE_WIDTH, Constants.PANEL_PREFERRED_SIZE_HEIGHT));
    }
    
    @Override
    public String getName() {
        
        /* Using resourceBundle instance variable here causes NPE */
        return NbBundle.getMessage(NameAndLocationVisualPanel.class, "NameAndLocationPanel.name");
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        fileNameLabel = new javax.swing.JLabel();
        fileNameTextField = new javax.swing.JTextField();
        createdFilesLabel = new javax.swing.JLabel();
        createdFilesScrollPane = new javax.swing.JScrollPane();
        createdFilesTextArea = new javax.swing.JTextArea();
        modifiedFilesScrollPane = new javax.swing.JScrollPane();
        modifiedFilesTextArea = new javax.swing.JTextArea();
        modifiedFilesLabel = new javax.swing.JLabel();

        fileNameLabel.setLabelFor(fileNameTextField);
        fileNameLabel.setText(org.openide.util.NbBundle.getMessage(NameAndLocationVisualPanel.class, "NameAndLocationVisualPanel.fileNameLabel.text")); // NOI18N

        fileNameTextField.setText(org.openide.util.NbBundle.getMessage(NameAndLocationVisualPanel.class, "NameAndLocationVisualPanel.fileNameTextField.text")); // NOI18N

        createdFilesLabel.setLabelFor(createdFilesTextArea);
        createdFilesLabel.setText(org.openide.util.NbBundle.getMessage(NameAndLocationVisualPanel.class, "NameAndLocationVisualPanel.createdFilesLabel.text")); // NOI18N

        createdFilesTextArea.setColumns(20);
        createdFilesTextArea.setEditable(false);
        createdFilesTextArea.setRows(5);
        createdFilesScrollPane.setViewportView(createdFilesTextArea);

        modifiedFilesTextArea.setColumns(20);
        modifiedFilesTextArea.setEditable(false);
        modifiedFilesTextArea.setRows(5);
        modifiedFilesScrollPane.setViewportView(modifiedFilesTextArea);

        modifiedFilesLabel.setLabelFor(modifiedFilesTextArea);
        modifiedFilesLabel.setText(org.openide.util.NbBundle.getMessage(NameAndLocationVisualPanel.class, "NameAndLocationVisualPanel.modifiedFilesLabel.text")); // NOI18N

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(createdFilesLabel)
                    .addComponent(fileNameLabel)
                    .addComponent(modifiedFilesLabel))
                .addGap(14, 14, 14)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(modifiedFilesScrollPane, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 274, Short.MAX_VALUE)
                    .addComponent(createdFilesScrollPane, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 274, Short.MAX_VALUE)
                    .addComponent(fileNameTextField, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 274, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(fileNameLabel)
                    .addComponent(fileNameTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(createdFilesLabel)
                    .addComponent(createdFilesScrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(9, 9, 9)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(modifiedFilesScrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(modifiedFilesLabel))
                .addContainerGap(80, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents
    
    void addDocumentListener(DocumentListener listener) {
        fileNameTextField.getDocument().addDocumentListener(listener);
    }    

    String getFileName() {
        return fileNameTextField.getText();
    }
    
    void setFileName(String fileName) {
        fileNameTextField.setText(fileName);
    }
    
    public void clearCreatedFiles() {
        createdFilesTextArea.setText("");
    }
    
    public void clearModifiedFiles() {
        modifiedFilesTextArea.setText("");
    }
    
    public void addCreatedFile(String fileName) {
        createdFilesTextArea.append(fileName + NEWLINE);
        
        /* make sure the first element is visible */
        createdFilesTextArea.setCaretPosition(0);
    }
    
    public void addModifiedFile(String fileName) {
        modifiedFilesTextArea.append(fileName + NEWLINE);
        
        /* make sure the first element is visible */
        modifiedFilesTextArea.setCaretPosition(0);
    }
    
    public void setFirstCreatedFile(String fileName) {
        
        if (createdFilesTextArea.getLineCount() <= 1) {
            clearCreatedFiles();
            addCreatedFile(fileName);
            
        } else {
            try {
                createdFilesTextArea.replaceRange(fileName + NEWLINE,
                        createdFilesTextArea.getLineStartOffset(0),
                        createdFilesTextArea.getLineEndOffset(0));
                
            } catch (BadLocationException e) {
                throw new AssertionError("This exception should never occur for line number zero: " + e.getMessage());
            }
        }
    }
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel createdFilesLabel;
    private javax.swing.JScrollPane createdFilesScrollPane;
    private javax.swing.JTextArea createdFilesTextArea;
    private javax.swing.JLabel fileNameLabel;
    private javax.swing.JTextField fileNameTextField;
    private javax.swing.JLabel modifiedFilesLabel;
    private javax.swing.JScrollPane modifiedFilesScrollPane;
    private javax.swing.JTextArea modifiedFilesTextArea;
    // End of variables declaration//GEN-END:variables
    
}
