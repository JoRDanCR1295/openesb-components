/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ProjectTab.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.border.EtchedBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import com.sun.jbi.component.toolkit.project.model.Project;
import com.sun.jbi.component.toolkit.project.model.Expr.ProjectExpr;
import com.sun.jbi.component.toolkit.project.model.Pom.Module;
import com.sun.jbi.component.toolkit.project.util.AsAdmin;
import com.sun.jbi.component.toolkit.project.view.DescriptionField.Text;
import com.sun.jbi.component.toolkit.project.view.event.AppEvent;
import com.sun.jbi.component.toolkit.project.view.event.AppEvent.Type;

/**
 * 
 * @author Kevan Simpson
 */
public class ProjectTab extends BasePanel {
    private DescriptionField mLocationLbl, mJbiPathLbl;
    private DescriptionField mScriptFld;
    private JTable mPomTable;
    private PomTableModel mTableModel;
    
    /**
     * 
     */
    public ProjectTab(App app) {
        super(app);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#updateValues() */
    @Override
    public boolean updateValues() {
        if (super.updateValues()) {
            Project proj = getApp().getProject();
            updateFields(proj);
//            mLocationLbl.setXmlObject(proj);
//            mJbiPathLbl.setXmlObject(proj);
            // poms
            mTableModel.update(proj);
            // as script
//            mScriptFld.setXmlObject(proj);
            return true;
        }
        
        return false;
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#init(java.lang.Object[]) */
    protected void init(Object... params) {
        setLayout(new GridBagLayout());
        GridBagConstraints c = new GridBagConstraints();
        c.gridx = 0;
        c.weightx = 1.0;
        
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weighty = 0;
        add(createInfoPanel(), c);
        
        c.fill = GridBagConstraints.BOTH;
        c.weighty = 0;
        add(createPomPanel(), c);
        
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weighty = 0;
        add(createActionPanel(), c);
        
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weighty = 1.0;
        add(new JPanel(), c);
    }
    
    protected JPanel createInfoPanel() {
        JPanel pnlInfo = new JPanel(new GridBagLayout());
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.anchor = GridBagConstraints.EAST;
        c.gridx = 0;
        c.weightx = 1.0;
        pnlInfo.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createEtchedBorder(EtchedBorder.LOWERED), "Project Info"));
        mLocationLbl = new DescriptionField(getApp(), 
                ProjectExpr.component_root, "Location:", 
                "The directory in which component project resides", Text.label);
        addField(mLocationLbl);
        pnlInfo.add(mLocationLbl, c);
        mJbiPathLbl = new DescriptionField(getApp(), 
                ProjectExpr.component_descriptor, "JBI Component Descriptor:", 
                "The JBI component descriptor path", Text.label);
        addField(mJbiPathLbl);
        pnlInfo.add(mJbiPathLbl, c);
        return pnlInfo;
    }
    
    protected JPanel createPomPanel() {
        JPanel pnlPoms = new JPanel(new GridBagLayout());
        GridBagConstraints c = new GridBagConstraints();
        pnlPoms.setBorder(
                BorderFactory.createTitledBorder(
                        BorderFactory.createEtchedBorder(EtchedBorder.LOWERED),
                        "POMs"
                )
        );
        mTableModel = new PomTableModel(getApp().getProject());
        mPomTable = new JTable(mTableModel);
        mPomTable.setDefaultRenderer(String.class, new DefaultTableCellRenderer() {
            /** @see javax.swing.table.DefaultTableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object, boolean, boolean, int, int) */
            @Override
            public Component getTableCellRendererComponent(JTable table,
                    Object value, boolean isSelected, boolean hasFocus,
                    int row, int column) {
                // indent cell values on left-hand side
                Component comp = super.getTableCellRendererComponent(
                        table, value, isSelected, hasFocus, row, column);
                if (comp instanceof JLabel) {
                    ((JLabel) comp).setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 0));
                }
                return comp;
            }
        });
        // resize 0 column, otherwise column 1 values get cut off
        TableColumn col1 = mPomTable.getColumnModel().getColumn(0);
        Component comp = mPomTable.getDefaultRenderer(String.class).
                getTableCellRendererComponent(
                        mPomTable, 
                        Module.jbiadapter.toString() + Module.packaging +
                                Module.top_level.toString(),
                        false, false, 0, 0);
        int cellWidth = comp.getPreferredSize().width;
        int cellHeight = comp.getPreferredSize().height;
        col1.setMaxWidth(cellWidth * 2);
        col1.setPreferredWidth(cellWidth);
        Dimension tableDim = mPomTable.getPreferredSize();
        mPomTable.setPreferredScrollableViewportSize(new Dimension(tableDim.width, cellHeight * 7));
        
        JScrollPane scrollPane =
                new JScrollPane(mPomTable, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                        JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED
                );
        c.fill = GridBagConstraints.HORIZONTAL;
        c.anchor = GridBagConstraints.CENTER;
        c.weightx = 1.0;
        pnlPoms.add(scrollPane, c);
        return pnlPoms;
    }
    
    protected JPanel createActionPanel() {
        GridBagConstraints c = new GridBagConstraints();
        c.insets = new Insets(2, 4, 2, 4);
        c.anchor = GridBagConstraints.CENTER;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 0;
        
        /*
         * appserver script,
         * btns: install, uninstall, start, stop, shut-down 
         *        startAS, stopAS
         */
        JPanel pnlBtns = new JPanel(new GridBagLayout());
        pnlBtns.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createEtchedBorder(EtchedBorder.LOWERED), 
                "Actions"));
        JPanel pnlAS = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 2));
        mScriptFld = new DescriptionField(getApp(), 
                ProjectExpr.as_script, "AppServer Script", 
                "The asadmin file of the application server in which the component is installed");
        addField(mScriptFld);
        pnlAS.add(mScriptFld);
        pnlAS.add(new JButton(new AbstractAction("Browse...") {
            /** @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent) */
            public void actionPerformed(ActionEvent e) {
                Project proj = getApp().getProject();
                File dir = (proj.getAsAdmin() == null)
                        ? proj.getRoot() : proj.getAsAdmin();
                // ensure source is displayed
                ProjectTab.this.getListenerSupport().fireAppEvent(
                        new AppEvent(Type.display_source, mScriptFld));
//                ProjectTab.this.firePropertyChange(
//                        App.Event.display_source.toString(), dir, mScriptFld);
                JFileChooser jfc = new JFileChooser(dir);
                jfc.setDialogTitle("Select the AppServer asadmin script...");
                int opt = jfc.showOpenDialog(null);
                if (opt == JFileChooser.APPROVE_OPTION) {
                    File file = jfc.getSelectedFile();
                    if (file != null) {
                        proj.setAsAdmin(file);
                        mScriptFld.modifyXPathElement(proj);
                    }
                }
            }
        }));
        
        JPanel pnlBtnAS = new JPanel(new GridBagLayout());
        GridBagConstraints asc = new GridBagConstraints();
        pnlBtnAS.setBorder(BorderFactory.createTitledBorder("AppServer"));
        asc.anchor = GridBagConstraints.NORTH;
        asc.fill = GridBagConstraints.HORIZONTAL;
        asc.insets = new Insets(2, 2, 2, 2);
        asc.gridy = 0;
        pnlBtnAS.add(createButton("Start Server", (int) 'r', AsAdmin.start_domain), asc);
        pnlBtnAS.add(createButton("Stop Server", (int) 'p', AsAdmin.stop_domain), asc);
        pnlBtnAS.add(createButton("Start Server (Debug)", (int) 'g', AsAdmin.debug_domain), asc);
        
        JPanel pnlGrid = new JPanel(new GridBagLayout());
        GridBagConstraints gridc = new GridBagConstraints();
        pnlGrid.setBorder(BorderFactory.createTitledBorder("Component"));
        // Build Instal Uninstall  |  Start Stop Shutdown  |  Status List Manage
        gridc.anchor = GridBagConstraints.CENTER;
        gridc.fill = GridBagConstraints.HORIZONTAL;
        gridc.insets = new Insets(2, 2, 2, 2);
        gridc.gridy = 0;
        pnlGrid.add(createButton("Build the Component", (int) 'b', AsAdmin.build_installer), gridc);
        pnlGrid.add(createButton("Install the Component", (int) 'i', AsAdmin.install_jbi_component), gridc);
        pnlGrid.add(createButton("Uninstall the Component", (int) 'u', AsAdmin.uninstall_jbi_component), gridc);
        pnlGrid.add(new JSeparator(JSeparator.VERTICAL), gridc);
        pnlGrid.add(createButton("Start the Component", (int) 's', AsAdmin.start_jbi_component), gridc);
        pnlGrid.add(createButton("Stop the Component", (int) 't', AsAdmin.stop_jbi_component), gridc);
        pnlGrid.add(createButton("Shut down the Component", (int) 'd', AsAdmin.shut_down_jbi_component), gridc);
        pnlGrid.add(new JSeparator(JSeparator.VERTICAL), gridc);
        pnlGrid.add(createButton("Component Status", (int) 'c', AsAdmin.show_jbi_service_engine), gridc);
        pnlGrid.add(createButton("List Assemblies", (int) 'l', AsAdmin.list_jbi_service_assemblies), gridc);
        pnlGrid.add(createButton("Manage Assemblies", (int) ',', AsAdmin.list_jbi_service_assemblies), gridc);
        
        c.gridx = 0;
        c.gridy = 0;
        c.gridwidth = 2;
        pnlBtns.add(pnlAS, c);

        c.gridx = 0;
        c.gridy = 1;
        c.gridwidth = 1;
        pnlBtns.add(pnlBtnAS, c);
        
        c.gridx = 1;
        c.gridy = 1;
        c.gridwidth = 2;
        pnlBtns.add(pnlGrid, c);
        
        return pnlBtns;
    }

    private JButton createButton(String description,
                                 int mnemonicKey,
                                 AsAdmin type) {
        JButton button = new JButton(new AsAdmin.AsAdminAction(
                description, getApp(), type, mnemonicKey));
        button.setToolTipText(description);
        return button;
    }

    private static class PomTableModel extends DefaultTableModel {
        private Project mProject;
        private String[] mNames;

        private PomTableModel(Project project) {
            super(0, 2);
            assert project != null;
            mProject = project;
            mNames = project.getModuleNames();
        }

        public synchronized void update(Project proj) {
            if (proj != null) {
                mProject = proj;
                mNames = proj.getModuleNames();
                fireTableDataChanged();
            }
        }
        
        /** @see javax.swing.table.TableModel#getColumnClass(int) */
        public Class<?> getColumnClass(int columnIndex) {
            return String.class;
        }

        /** @see javax.swing.table.TableModel#getColumnName(int) */
        public String getColumnName(int columnIndex) {
            return (columnIndex == 0) ? "Pom Name" : "Location";
        }

        /** @see javax.swing.table.TableModel#getRowCount() */
        public int getRowCount() {
            return (mNames == null) ? 0 : mNames.length;
        }

        /** @see javax.swing.table.TableModel#getValueAt(int, int) */
        public Object getValueAt(int rowIndex, int columnIndex) {
            if (mNames == null) return "";
            
            String name = mNames[rowIndex];
            if (columnIndex == 0) {
                return name;
            }
            else {
                return mProject.getModule(name).getFile().getAbsolutePath();
            }
        }

        /** @see javax.swing.table.TableModel#isCellEditable(int, int) */
        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return false;
        }
    }
}
