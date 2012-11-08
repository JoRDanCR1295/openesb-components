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
public class CamelClassPathModel extends AbstractTableModel {

    private static final Logger LOG = Logger.getLogger(CamelClassPathModel.class.getName());
    public static final int REL_PATH_COL_IDX = 0;
    private List<String> mPathItems;

    public CamelClassPathModel() {
        this.mPathItems = new ArrayList<String>();
    }

    public int getRowCount() {
        return this.mPathItems.size();
    }

    public int getColumnCount() {
        return 1;
    }

    @Override
    public String getColumnName(int column) {

        if (column == REL_PATH_COL_IDX) {
            return "Camel Classpath items";
        } else {
            return null;
        }
    }

    @Override
    public Class<?> getColumnClass(int columnIndex) {
        if (columnIndex == REL_PATH_COL_IDX) {
            return String.class;
        } else {
            return super.getColumnClass(columnIndex);
        }
    }

    @Override
    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return false;
    }

    public Object getValueAt(int rowIndex, int columnIndex) {
        String item = this.mPathItems.get(rowIndex);
        if (item == null) {
            //TODO: log at fine
            return null;
        }
        if (columnIndex == REL_PATH_COL_IDX) {
            return item;
        } else {
            return null;
        }
    }

    @Override
    public void setValueAt(Object aValue, int rowIndex, int columnIndex) {

        String item = this.mPathItems.get(rowIndex);
        if (item == null) {
            //TODO: log at fine
            LOG.fine("No PathItem at rowindex " + rowIndex);
            return;
        }

    }

    public void clear() {
        this.mPathItems = new ArrayList<String>();
        this.fireTableDataChanged();
    }

    public void load(List<String> relPaths) {

        this.mPathItems = new ArrayList<String>();
        for (String relPath : relPaths) {
            this.mPathItems.add(relPath);
        }
        this.fireTableDataChanged();
    }

    public List<String> getRelativePaths() {
        List<String> relPaths = new ArrayList<String>();
        for (String item : this.mPathItems) {
            relPaths.add(item);
        }
        return relPaths;
    }

    public int addPath(String path) {
        this.mPathItems.add(path);
        this.fireTableDataChanged();
        return this.mPathItems.size();
    }
    public int addPaths(List<String> paths) {
        this.mPathItems.addAll(paths);
        this.fireTableDataChanged();
        return this.mPathItems.size();
    }
    public void removePath(String path) {
        for (String item : this.mPathItems) {
            if (item.equals(path)) {
                this.mPathItems.remove(path);
                break;
            }
        }
        this.fireTableDataChanged();
    }

    public int removePath(int idx) {
        int size = this.mPathItems.size();
        if (idx >= 0 && idx < size) {
            this.mPathItems.remove(idx);
            this.fireTableDataChanged();
        }        
        return idx-1;
    }

    public int moveUp(int idx) {
        int size = this.mPathItems.size();
        int mvIdx = idx - 1;
        if (mvIdx >= 0 && idx < size) {
            String idxItem = this.mPathItems.get(idx);
            String mvItem = this.mPathItems.get(mvIdx);
            this.mPathItems.set(idx, mvItem);
            this.mPathItems.set(mvIdx, idxItem);
            this.fireTableDataChanged();
        }       
        return mvIdx;
    }

    public int moveDown(int idx) {
        int size = this.mPathItems.size();
        int mvIdx = idx + 1;
        if (mvIdx < size && idx >= 0) {
            String idxItem = this.mPathItems.get(idx);
            String mvItem = this.mPathItems.get(mvIdx);
            this.mPathItems.set(idx, mvItem);
            this.mPathItems.set(mvIdx, idxItem);
            this.fireTableDataChanged();
        }                
        return mvIdx;
    }
}
