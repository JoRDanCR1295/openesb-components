/*
 * CamelLibsPanel.java
 *
 * Created on August 26, 2008, 12:58 AM
 */

package org.openesb.components.camelse.nb.plugin.project.customizer;

/**
 *
 * @author  chikkala
 */
public class CamelLibsPanel extends javax.swing.JPanel {

    /** Creates new form CamelLibsPanel */
    public CamelLibsPanel() {
        initComponents();
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
        mCamelHomeValueLBL = new javax.swing.JLabel();
        mCamelLibsLBL = new javax.swing.JLabel();
        mCamelLibsSP = new javax.swing.JScrollPane();
        mCamelLibsTBL = new javax.swing.JTable();

        mCamelHomeLBL.setText(org.openide.util.NbBundle.getMessage(CamelLibsPanel.class, "CamelLibsPanel.mCamelHomeLBL.text")); // NOI18N

        mCamelHomeValueLBL.setText(org.openide.util.NbBundle.getMessage(CamelLibsPanel.class, "CamelLibsPanel.mCamelHomeValueLBL.text")); // NOI18N

        mCamelLibsLBL.setText(org.openide.util.NbBundle.getMessage(CamelLibsPanel.class, "CamelLibsPanel.mCamelLibsLBL.text")); // NOI18N

        mCamelLibsTBL.setModel(getCamelLibsModel());
        mCamelLibsSP.setViewportView(mCamelLibsTBL);

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(layout.createSequentialGroup()
                        .add(mCamelHomeLBL)
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(mCamelHomeValueLBL, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 311, Short.MAX_VALUE))
                    .add(mCamelLibsLBL)
                    .add(mCamelLibsSP, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 375, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(mCamelHomeLBL)
                    .add(mCamelHomeValueLBL))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.UNRELATED)
                .add(mCamelLibsLBL)
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(mCamelLibsSP, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 275, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel mCamelHomeLBL;
    private javax.swing.JLabel mCamelHomeValueLBL;
    private javax.swing.JLabel mCamelLibsLBL;
    private javax.swing.JScrollPane mCamelLibsSP;
    private javax.swing.JTable mCamelLibsTBL;
    // End of variables declaration//GEN-END:variables

    private CamelLibsModel mCamelLibsModel;
    
    public CamelLibsModel getCamelLibsModel() {
        if ( this.mCamelLibsModel == null ) {
            this.mCamelLibsModel = new CamelLibsModel();
        }
        return this.mCamelLibsModel;
    }
    
    public void setCamelHome(String camelHome) {
        this.mCamelHomeValueLBL.setText(camelHome);
        this.getCamelLibsModel().load(camelHome);
    }
}
