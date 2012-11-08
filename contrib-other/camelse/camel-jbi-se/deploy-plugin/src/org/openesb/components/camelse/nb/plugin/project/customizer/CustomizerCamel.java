/*
 * CustomizerCamel.java
 *
 * Created on July 17, 2008, 1:37 AM
 */
package org.openesb.components.camelse.nb.plugin.project.customizer;

import java.io.File;
import java.util.List;
import javax.swing.JFileChooser;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.openide.DialogDescriptor;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.filesystems.FileUtil;
import org.openide.util.HelpCtx;

/**
 *
 * @author  chikkala
 */
public class CustomizerCamel extends javax.swing.JPanel implements HelpCtx.Provider {

    private SEPluginProjectCustomizerModel uiModel;

    /** Creates new form CustomizerCamel */
    public CustomizerCamel(SEPluginProjectCustomizerModel uiModel) {
        this.uiModel = uiModel;
        initComponents();
        this.mCamelHomeTF.setDocument(uiModel.getCamelHomeModel());
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        mCamelHomeLBL = new javax.swing.JLabel();
        mCamelHomeTF = new javax.swing.JTextField();
        mCamelHomeBrowseBTN = new javax.swing.JButton();
        mCamelCPSP = new javax.swing.JScrollPane();
        mCamelCPTable = new javax.swing.JTable();
        addBTN = new javax.swing.JButton();
        removeBTN = new javax.swing.JButton();
        moveUpBTN = new javax.swing.JButton();
        moveDownBTN = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();

        mCamelHomeLBL.setText(org.openide.util.NbBundle.getMessage(CustomizerCamel.class, "CustomizerCamel.mCamelHomeLBL.text")); // NOI18N

        mCamelHomeTF.setText(org.openide.util.NbBundle.getMessage(CustomizerCamel.class, "CustomizerCamel.mCamelHomeTF.text")); // NOI18N
        mCamelHomeTF.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                mCamelHomeTFActionPerformed(evt);
            }
        });

        mCamelHomeBrowseBTN.setText(org.openide.util.NbBundle.getMessage(CustomizerCamel.class, "CustomizerCamel.mCamelHomeBrowseBTN.text")); // NOI18N
        mCamelHomeBrowseBTN.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                mCamelHomeBrowseBTNActionPerformed(evt);
            }
        });

        mCamelCPTable.setModel(getCamelClassPathModel());
        mCamelCPSP.setViewportView(mCamelCPTable);

        addBTN.setText(org.openide.util.NbBundle.getMessage(CustomizerCamel.class, "CustomizerCamel.addBTN.text")); // NOI18N
        addBTN.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addBTNActionPerformed(evt);
            }
        });

        removeBTN.setText(org.openide.util.NbBundle.getMessage(CustomizerCamel.class, "CustomizerCamel.removeBTN.text")); // NOI18N
        removeBTN.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                removeBTNActionPerformed(evt);
            }
        });

        moveUpBTN.setText(org.openide.util.NbBundle.getMessage(CustomizerCamel.class, "CustomizerCamel.moveUpBTN.text")); // NOI18N
        moveUpBTN.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                moveUpBTNActionPerformed(evt);
            }
        });

        moveDownBTN.setText(org.openide.util.NbBundle.getMessage(CustomizerCamel.class, "CustomizerCamel.moveDownBTN.text")); // NOI18N
        moveDownBTN.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                moveDownBTNActionPerformed(evt);
            }
        });

        jLabel1.setText(org.openide.util.NbBundle.getMessage(CustomizerCamel.class, "CustomizerCamel.jLabel1.text")); // NOI18N

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(org.jdesktop.layout.GroupLayout.TRAILING, layout.createSequentialGroup()
                        .add(mCamelCPSP, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 308, Short.MAX_VALUE)
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                            .add(addBTN)
                            .add(removeBTN)
                            .add(moveUpBTN)
                            .add(moveDownBTN))
                        .addContainerGap())
                    .add(layout.createSequentialGroup()
                        .add(mCamelHomeLBL, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 63, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(mCamelHomeTF, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 241, Short.MAX_VALUE)
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(mCamelHomeBrowseBTN)
                        .add(20, 20, 20))
                    .add(layout.createSequentialGroup()
                        .add(jLabel1)
                        .addContainerGap(331, Short.MAX_VALUE))))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                        .add(mCamelHomeLBL)
                        .add(mCamelHomeTF, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                    .add(mCamelHomeBrowseBTN))
                .add(18, 18, 18)
                .add(jLabel1)
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.UNRELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(layout.createSequentialGroup()
                        .add(addBTN)
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(removeBTN)
                        .add(18, 18, 18)
                        .add(moveUpBTN)
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(moveDownBTN))
                    .add(mCamelCPSP, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 257, Short.MAX_VALUE))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

private void mCamelHomeTFActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_mCamelHomeTFActionPerformed
// TODO add your handling code here:
}//GEN-LAST:event_mCamelHomeTFActionPerformed

private void mCamelHomeBrowseBTNActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_mCamelHomeBrowseBTNActionPerformed
// TODO add your handling code here:
    JFileChooser chooser = new JFileChooser();
    FileUtil.preventFileChooserSymlinkTraversal(chooser, null);
    chooser.setDialogTitle("Select Apache Camel Home");
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    String path = this.mCamelHomeTF.getText();
    if (path.length() > 0) {
        File f = new File(path);
        if (f.exists()) {
            chooser.setSelectedFile(f);
        }
    }
    if (JFileChooser.APPROVE_OPTION == chooser.showOpenDialog(this)) {
        File camelHomeDir = chooser.getSelectedFile();
        // this.mCamelHomeTF.setText(FileUtil.normalizeFile(camelHomeDir).getAbsolutePath());
        String camelHomePath = FileUtil.normalizeFile(camelHomeDir).getAbsolutePath();
        camelHomePath = camelHomePath.replace("\\", "/");
        setDocumentText(this.mCamelHomeTF.getDocument(), camelHomePath);
    }

}//GEN-LAST:event_mCamelHomeBrowseBTNActionPerformed

private void addBTNActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addBTNActionPerformed
// TODO add your handling code here:
   String camelLibsBrowserTitle = "Select Camel Libraries";
   String camelHome = this.mCamelHomeTF.getText();
   CamelLibsPanel panel = new CamelLibsPanel();
   panel.setCamelHome(camelHome);
   DialogDescriptor dialog = new DialogDescriptor(panel, camelLibsBrowserTitle);
   DialogDisplayer.getDefault().notify(dialog);
   if (NotifyDescriptor.OK_OPTION.equals(dialog.getValue()) ) {
       List<String> relPaths = panel.getCamelLibsModel().getSelectedPaths();
       this.getCamelClassPathModel().addPaths(relPaths);
   }
}//GEN-LAST:event_addBTNActionPerformed

private void removeBTNActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_removeBTNActionPerformed
// TODO add your handling code here:
    int idx = this.mCamelCPTable.getSelectedRow();
    idx = this.getCamelClassPathModel().removePath(idx);
    if ( idx >= 0 ) {
        this.mCamelCPTable.getSelectionModel().setSelectionInterval(idx, idx);
    }    
}//GEN-LAST:event_removeBTNActionPerformed

private void moveUpBTNActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_moveUpBTNActionPerformed
// TODO add your handling code here:
    int idx = this.mCamelCPTable.getSelectedRow();
    idx = this.getCamelClassPathModel().moveUp(idx);
    if ( idx >= 0 ) {
        this.mCamelCPTable.getSelectionModel().setSelectionInterval(idx, idx);
    }
}//GEN-LAST:event_moveUpBTNActionPerformed

private void moveDownBTNActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_moveDownBTNActionPerformed
// TODO add your handling code here:
    int idx = this.mCamelCPTable.getSelectedRow();
    idx = this.getCamelClassPathModel().moveDown(idx);
    if ( idx >= 0 ) {
        this.mCamelCPTable.getSelectionModel().setSelectionInterval(idx, idx);
    }    
}//GEN-LAST:event_moveDownBTNActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton addBTN;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JScrollPane mCamelCPSP;
    private javax.swing.JTable mCamelCPTable;
    private javax.swing.JButton mCamelHomeBrowseBTN;
    private javax.swing.JLabel mCamelHomeLBL;
    private javax.swing.JTextField mCamelHomeTF;
    private javax.swing.JButton moveDownBTN;
    private javax.swing.JButton moveUpBTN;
    private javax.swing.JButton removeBTN;
    // End of variables declaration//GEN-END:variables

    public CamelClassPathModel getCamelClassPathModel() {
        return this.uiModel.getCamelClassPathModel();
    }

    public HelpCtx getHelpCtx() {
        return new HelpCtx(CustomizerPackage.class);
    }

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
}