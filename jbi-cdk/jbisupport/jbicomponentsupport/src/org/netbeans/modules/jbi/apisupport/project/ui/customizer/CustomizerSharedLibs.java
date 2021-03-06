/*
 * CustomizerSharedLibs.java
 *
 * Created on September 26, 2007, 12:24 PM
 */

package org.netbeans.modules.jbi.apisupport.project.ui.customizer;

import org.openide.util.HelpCtx;

/**
 *
 * @author  chikkala
 */
public class CustomizerSharedLibs extends javax.swing.JPanel implements HelpCtx.Provider  {
    CustomizerUIModel mUIModel;
    /** Creates new form CustomizerSharedLibs */
    public CustomizerSharedLibs( CustomizerUIModel uiProperties ) {
        mUIModel = uiProperties;
        initComponents();
        initSharedLibEditor();
    }
    public HelpCtx getHelpCtx() {
        return new HelpCtx( CustomizerSharedLibs.class  );
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        mSLibEditor = new javax.swing.JPanel();

        setLayout(new java.awt.GridBagLayout());

        mSLibEditor.setLayout(null);
        mSLibEditor = createSharedLibraryEditor();
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(mSLibEditor, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel mSLibEditor;
    // End of variables declaration//GEN-END:variables
    
    private SharedLibrariesEditor createSharedLibraryEditor() {
        return new SharedLibrariesEditor();
    }
    
    private SharedLibrariesEditor getSharedLibraryEditor() {
        return (SharedLibrariesEditor) mSLibEditor;
    }
    private void initSharedLibEditor() {
        getSharedLibraryEditor().setSharedLibTableModel(this.mUIModel.SLIBS_TABLE_MODEL);
        getSharedLibraryEditor().loadSharedLibraries(this.mUIModel.mSLibs);
    }
}
