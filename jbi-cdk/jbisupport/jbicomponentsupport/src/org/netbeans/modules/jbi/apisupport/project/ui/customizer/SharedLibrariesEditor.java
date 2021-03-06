/*
 * SharedLibrariesEditor.java
 *
 * Created on September 26, 2007, 10:50 AM
 */

package org.netbeans.modules.jbi.apisupport.project.ui.customizer;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.swing.table.AbstractTableModel;
import org.netbeans.modules.jbi.apisupport.common.JbiComponentDescriptor;
import org.netbeans.modules.jbi.apisupport.common.JbiSharedLibrary;

/**
 *
 * @author  chikkala
 */
public class SharedLibrariesEditor extends javax.swing.JPanel {
    
    /** Creates new form SharedLibrariesEditor */
    public SharedLibrariesEditor() {
        initComponents();
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        mSLibTBLScrollPane = new javax.swing.JScrollPane();
        mSLibTBL = new javax.swing.JTable();
        mAddSLibBTN = new javax.swing.JButton();
        mRemoveSLibBTN = new javax.swing.JButton();
        mUpSLibBTN = new javax.swing.JButton();
        mDownSLibBTN = new javax.swing.JButton();
        mEmptyVFillLBL = new javax.swing.JLabel();

        setLayout(new java.awt.GridBagLayout());

        mSLibTBL.setModel(getSharedLibTableModel());
        mSLibTBLScrollPane.setViewportView(mSLibTBL);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.gridheight = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 4, 4);
        add(mSLibTBLScrollPane, gridBagConstraints);

        mAddSLibBTN.setText(org.openide.util.NbBundle.getMessage(SharedLibrariesEditor.class, "SharedLibrariesEditor.mAddSLibBTN.text")); // NOI18N
        mAddSLibBTN.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                mAddSLibBTNActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 2, 2, 2);
        add(mAddSLibBTN, gridBagConstraints);

        mRemoveSLibBTN.setText(org.openide.util.NbBundle.getMessage(SharedLibrariesEditor.class, "SharedLibrariesEditor.mRemoveSLibBTN.text")); // NOI18N
        mRemoveSLibBTN.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                mRemoveSLibBTNActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 6, 2);
        add(mRemoveSLibBTN, gridBagConstraints);

        mUpSLibBTN.setText(org.openide.util.NbBundle.getMessage(SharedLibrariesEditor.class, "SharedLibrariesEditor.mUpSLibBTN.text")); // NOI18N
        mUpSLibBTN.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                mUpSLibBTNActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 2, 2, 2);
        add(mUpSLibBTN, gridBagConstraints);

        mDownSLibBTN.setText(org.openide.util.NbBundle.getMessage(SharedLibrariesEditor.class, "SharedLibrariesEditor.mDownSLibBTN.text")); // NOI18N
        mDownSLibBTN.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                mDownSLibBTNActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 6, 2);
        add(mDownSLibBTN, gridBagConstraints);

        mEmptyVFillLBL.setText(org.openide.util.NbBundle.getMessage(SharedLibrariesEditor.class, "SharedLibrariesEditor.mEmptyVFillLBL.text")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        add(mEmptyVFillLBL, gridBagConstraints);
        mEmptyVFillLBL.getAccessibleContext().setAccessibleName(org.openide.util.NbBundle.getMessage(SharedLibrariesEditor.class, "SharedLibrariesEditor.mEmptyVFillLBL.AccessibleContext.accessibleName")); // NOI18N
    }// </editor-fold>//GEN-END:initComponents

    private void mAddSLibBTNActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_mAddSLibBTNActionPerformed
        this.getSharedLibTableModel().addSharedLibInfo();
}//GEN-LAST:event_mAddSLibBTNActionPerformed

    private void mRemoveSLibBTNActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_mRemoveSLibBTNActionPerformed
        int idx = this.mSLibTBL.getSelectedRow();
        if ( idx >= 0 ) {
            this.getSharedLibTableModel().removeSharedLibInfo(idx);
        }
}//GEN-LAST:event_mRemoveSLibBTNActionPerformed

    private void mUpSLibBTNActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_mUpSLibBTNActionPerformed
        // TODO add your handling code here:
        int idx = this.mSLibTBL.getSelectedRow();
        if (idx >= 0) {
            this.getSharedLibTableModel().moveUpSharedLibInfo(idx);
        }
}//GEN-LAST:event_mUpSLibBTNActionPerformed

    private void mDownSLibBTNActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_mDownSLibBTNActionPerformed
        // TODO add your handling code here:
        int idx = this.mSLibTBL.getSelectedRow();
        if ( idx >= 0 ) {
            // move down
            this.getSharedLibTableModel().moveDownSharedLibInfo(idx);
        }
        
}//GEN-LAST:event_mDownSLibBTNActionPerformed
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton mAddSLibBTN;
    private javax.swing.JButton mDownSLibBTN;
    private javax.swing.JLabel mEmptyVFillLBL;
    private javax.swing.JButton mRemoveSLibBTN;
    private javax.swing.JTable mSLibTBL;
    private javax.swing.JScrollPane mSLibTBLScrollPane;
    private javax.swing.JButton mUpSLibBTN;
    // End of variables declaration//GEN-END:variables
    
    private SharedLibTableModel mSharedLibTableModel;
    
    public void setSharedLibTableModel(SharedLibTableModel model) {
        this.mSharedLibTableModel = model;
        this.mSLibTBL.setModel(this.mSharedLibTableModel);
    }
    
    public SharedLibTableModel getSharedLibTableModel() {
        if ( this.mSharedLibTableModel == null ) {
            this.mSharedLibTableModel = new SharedLibTableModel();
        }
        return mSharedLibTableModel;
    }
    /**
     * for now slib from the descriptor
     */
    public void loadSharedLibraries(List<JbiSharedLibrary> slibList) {
        getSharedLibTableModel().load(slibList);
    }
    
    /**
     * for now slib from the descriptor
     */
    public void loadSharedLibraries(String[] slibNames) {
        List<JbiSharedLibrary> list = new ArrayList<JbiSharedLibrary>();
        for ( String slibName : slibNames) {
            list.add(new JbiSharedLibrary.JbiSharedLibraryImpl(slibName, ""));
        }
        getSharedLibTableModel().load(list);
    }
    
    
    public static class SharedLibTableModel extends AbstractTableModel {
        
        public static final int NAME_COL_IDX = 0;
        public static final int VALUE_COL_IDX = 1;
        
        private List<JbiSharedLibrary> mSLibs;
        
        private boolean mEdited = false;
        
        public SharedLibTableModel() {
            
            this.mSLibs = new ArrayList();
            this.mEdited = false;
        }
        
        public boolean isEdited() {
            return this.mEdited;
        }
        
        protected void setEdited(boolean edited) {
            this.mEdited = edited;
        }
        
        public int getRowCount() {
            return this.mSLibs.size();
        }
        
        public int getColumnCount() {
            return 2;
        }
        
        @Override
        public String getColumnName(int column) {
            
            if ( column == NAME_COL_IDX ) {
                return "Shared Library";
            } else if ( column == VALUE_COL_IDX ) {
                return "Class Path";
            } else {
                return null;
            }
        }
        
        @Override
        public boolean isCellEditable(int rowIndex, int columnIndex) {
            // return ( columnIndex == VALUE_COL_IDX );
            return true;
        }
        
        public Object getValueAt(int rowIndex, int columnIndex) {
            
            JbiSharedLibrary slib = this.mSLibs.get(rowIndex);
            
            if ( columnIndex == NAME_COL_IDX ) {
                return slib.getName();
            } else if ( columnIndex == VALUE_COL_IDX ) {
                return slib.getClassPath();
            } else {
                return null;
            }
        }
        
        @Override
        public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
            
            JbiSharedLibrary slib = this.mSLibs.get(rowIndex);
            if ( slib == null ) {
                return;
            }
            
            if ( columnIndex == NAME_COL_IDX ) {
                slib.setName((String)aValue);
                this.setEdited(true);
            } else if ( columnIndex == VALUE_COL_IDX ) {
                slib.setClassPath((String)aValue);
                this.setEdited(true);
            }
            
        }
        
        public void clear() {
            this.mSLibs = new ArrayList();
            this.setEdited(true);
            this.fireTableDataChanged();
        }
        
        public void load(List<JbiSharedLibrary> slibs) {
            this.mSLibs.clear();
            this.mSLibs.addAll(slibs);
            this.setEdited(false);
            this.fireTableDataChanged();
        }
        
        public JbiSharedLibrary createNewSharedLibInfo() {
            List names = new ArrayList();
            JbiSharedLibrary newSLib = new JbiSharedLibrary.JbiSharedLibraryImpl("shared-lib", "");
            
            for ( JbiSharedLibrary slib : this.mSLibs) {
                names.add(slib.getName());
            }
            
            for ( int i=0; i < Integer.MAX_VALUE; ++i) {
                String newName = "shared-lib-" + i;
                if (!names.contains(newName)) {
                    newSLib = new JbiSharedLibrary.JbiSharedLibraryImpl(newName, "");
                    break;
                }
            }
            return newSLib;
        }
        
        public void addSharedLibInfo() {
            int i = this.mSLibs.size();
            this.mSLibs.add(createNewSharedLibInfo());
            this.setEdited(true);
            this.fireTableRowsInserted(i,i);
        }
        
        public void removeSharedLibInfo(int idx) {
            try {
                this.mSLibs.remove(idx);
                this.setEdited(true);
                this.fireTableRowsDeleted(idx, idx);
            } catch (IndexOutOfBoundsException ex) {
                ex.printStackTrace();
            }
        }
        
        public void moveUpSharedLibInfo(int idx) {
            try {
                this.setEdited(true);
                this.fireTableRowsUpdated(idx-1, idx);
            } catch (IndexOutOfBoundsException ex) {
                ex.printStackTrace();
            }
        }

        public void moveDownSharedLibInfo(int idx) {
            try {
                this.setEdited(true);
                this.fireTableRowsUpdated(idx, idx+1);
            } catch (IndexOutOfBoundsException ex) {
                ex.printStackTrace();
            }
        }
        
        
        public List<JbiSharedLibrary> getSharedLibs() {
            List<JbiSharedLibrary> list = new ArrayList<JbiSharedLibrary>();
            list.addAll(this.mSLibs);
            return list;
        }
    }
    
}
