/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.openesb.components.camelse.nb.plugin.project.customizer;

import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import javax.swing.table.AbstractTableModel;

/**
 *
 * @author chikkala
 */
public class CamelLibsModel extends AbstractTableModel {

    private static final Logger LOG = Logger.getLogger(CamelLibsModel.class.getName());
    public static final int SELECTED_COL_IDX = 0;
    public static final int REL_PATH_COL_IDX = 1;
    private List<PathItem> mPathItems;

    public CamelLibsModel() {
        this.mPathItems = new ArrayList<PathItem>();
    }

    public int getRowCount() {
        return this.mPathItems.size();
    }

    public int getColumnCount() {
        return 2;
    }

    @Override
    public String getColumnName(int column) {

        if (column == SELECTED_COL_IDX) {
            return "Select";
        } else if (column == REL_PATH_COL_IDX) {
            return "Camel Library Path";
        } else {
            return null;
        }
    }

    @Override
    public Class<?> getColumnClass(int columnIndex) {
        if (columnIndex == SELECTED_COL_IDX) {
            return Boolean.class;
        } else if (columnIndex == REL_PATH_COL_IDX) {
            return String.class;
        } else {
            return super.getColumnClass(columnIndex);
        }
    }

    @Override
    public boolean isCellEditable(int rowIndex, int columnIndex) {
        if (columnIndex == SELECTED_COL_IDX) {
            return true;
        } else {
            return false;
        }
    }

    public Object getValueAt(int rowIndex, int columnIndex) {
        PathItem item = (PathItem) this.mPathItems.get(rowIndex);
        if (item == null) {
            //TODO: log at fine
            return null;
        }
        if (columnIndex == SELECTED_COL_IDX) {
            return item.isSelected();
        } else if (columnIndex == REL_PATH_COL_IDX) {
            return item.getRelativePath();
        } else {
            return null;
        }
    }

    @Override
    public void setValueAt(Object aValue, int rowIndex, int columnIndex) {

        PathItem item = (PathItem) this.mPathItems.get(rowIndex);
        if (item == null) {
            //TODO: log at fine
            LOG.fine("No PathItem at rowindex " + rowIndex);
            return;
        }
        if (columnIndex == SELECTED_COL_IDX) {
            item.setSelected((Boolean) aValue);
        }

    }

    public void clear() {
        this.mPathItems = new ArrayList<PathItem>();
        this.fireTableDataChanged();
    }

    public void load(List<String> relPaths) {

        this.mPathItems = new ArrayList<PathItem>();
        for (String relPath : relPaths) {
            PathItem item = new PathItem(relPath, false);
            this.mPathItems.add(item);
        }
        this.fireTableDataChanged();
    }

    /**
     * 
     * @param dir
     * @param relDir  relative dir without the / suffix or prefix. e.g. lib lib/extra 
     * can be null to list jars in the dir
     * @return
     */
    private List<String> listJars(File dir, String relDir) {
        File jarDir = dir;
        if (relDir != null && relDir.trim().length() > 0) {
            jarDir = new File(dir, relDir);
        }
        if (!jarDir.exists() || !jarDir.isDirectory()) {
            LOG.info("Directory not exist for locating jar files" + jarDir.getPath());
            return new ArrayList<String>();
        }
        File[] jars = jarDir.listFiles(new FileFilter() {

            public boolean accept(File pathname) {
                return pathname.getName().endsWith(".jar");
            }
        });
        List<String> list = new ArrayList<String>();
        for (File jar : jars) {
            String jarRelPath = jar.getName();
            if (relDir != null && relDir.trim().length() > 0) {
                jarRelPath = relDir + "/" + jarRelPath;
            }
            list.add(jarRelPath);
        }
        return list;
    }

    public void load(String camelHome) {
        List<String> relPaths = new ArrayList<String>();
        File camelHomeDir = new File(camelHome);
        if (camelHomeDir.exists() && camelHomeDir.isDirectory()) {
            relPaths.addAll(listJars(camelHomeDir, null));
            relPaths.addAll(listJars(camelHomeDir, "lib"));
            relPaths.addAll(listJars(camelHomeDir, "lib/optional"));
        }
        load(relPaths);
    }
    
    public List<String> getRelativePaths() {
        List<String> relPaths = new ArrayList<String>();
        for (PathItem item : this.mPathItems) {
                relPaths.add(item.getRelativePath());
        }
        return relPaths;
    }
    
    public void selectItems(List<String> relPaths) {
        for (String relPath : relPaths) {
            for (PathItem item : this.mPathItems) {
                if (relPath.equals(item.getRelativePath())) {
                    item.setSelected(true);
                    break;
                }
            }
        }
        this.fireTableDataChanged();
    }

    
    public List<String> getSelectedPaths() {
        List<String> relPaths = new ArrayList<String>();
        for (PathItem item : this.mPathItems) {
            if ( item.isSelected()) {
                relPaths.add(item.getRelativePath());
            }
        }
        return relPaths;
    }

    public static class PathItem {

        private Boolean mSelected;
        private String mRelativePath;

        public PathItem(String relativePath, Boolean selected) {
            this.mSelected = selected;
            this.mRelativePath = relativePath;
        }

        public Boolean isSelected() {
            return mSelected;
        }

        public void setSelected(Boolean selected) {
            this.mSelected = selected;
        }

        public String getRelativePath() {
            return mRelativePath;
        }

        public void setRelativePath(String relativePath) {
            this.mRelativePath = relativePath;
        }

        @Override
        public String toString() {
            return getRelativePath();
        }
    }
}
