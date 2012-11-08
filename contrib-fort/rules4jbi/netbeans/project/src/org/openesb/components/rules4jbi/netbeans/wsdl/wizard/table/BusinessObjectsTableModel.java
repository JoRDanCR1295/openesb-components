/*
 * @(#)BusinessObjectsTableModel.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
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

package org.openesb.components.rules4jbi.netbeans.wsdl.wizard.table;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;
import javax.swing.table.AbstractTableModel;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
public class BusinessObjectsTableModel extends AbstractTableModel {
    
    private static final long serialVersionUID = -2449366228226639442L;
    
    private static final Logger logger = Logger.getLogger(BusinessObjectsTableModel.class.getName());
    
    private final String[] columnNames = {"Type", "Direction", "Cardinality"};
    
    private final Class<?>[] columnClasses = {String.class, Direction.class, Cardinality.class};

    private final List<BusinessObjectTableRecord> tableData = new ArrayList<BusinessObjectTableRecord>();
    
    public List<BusinessObjectTableRecord> getTableData() {
        return Collections.unmodifiableList(tableData);
    }
    
    @Override
    public Class<?> getColumnClass(int columnIndex) {
        return columnClasses[columnIndex];
    }

    @Override
    public String getColumnName(int columnIndex) {
        return columnNames[columnIndex];
    }

    @Override
    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return true;
    }
    
    public int getRowCount() {
        return tableData.size();
    }

    public int getColumnCount() {
        return columnNames.length;
    }

    public Object getValueAt(int rowIndex, int columnIndex) {
        if (rowIndex < 0 || rowIndex >= tableData.size()) {
            return null;
        }
        
        BusinessObjectTableRecord record = tableData.get(rowIndex);
        
        switch (columnIndex) {
        case 0:
            return record.getType();
            
        case 1:
            return record.getDirection();
            
        case 2:
            return record.getCardinality();
        
        default:
            throw new AssertionError("Invalid column index");
        }
    }

    @Override
    public void setValueAt(Object value, int rowIndex, int columnIndex) {
        if (rowIndex < 0 || rowIndex >= tableData.size()) {
            return;
        }
        
        BusinessObjectTableRecord record = tableData.get(rowIndex);
        
        switch (columnIndex) {
        case 0:
            record.setType((String) value);
            break;
            
        case 1:
            record.setDirection((Direction) value);
            break;
            
        case 2:
            record.setCardinality((Cardinality) value);
            break;
            
        default:
            throw new AssertionError("Invalid column index");
        }
        
        fireTableCellUpdated(rowIndex, columnIndex);
    }
    
    void addNewRecord(int rowIndex) {
        logger.finest("Adding row " + rowIndex);
        
        tableData.add(rowIndex, new BusinessObjectTableRecord());
        
        fireTableRowsInserted(rowIndex, rowIndex);
    }

    void deleteRecord(int rowIndex) {
        logger.finest("Deleting row " + rowIndex);
        
        tableData.remove(rowIndex);
        
        fireTableRowsDeleted(rowIndex, rowIndex);
    }

    void moveRecordUp(int rowIndex) {
        logger.finest("Moving row " + rowIndex + " up");

        BusinessObjectTableRecord currentRecord = tableData.remove(rowIndex);
        tableData.add(rowIndex - 1, currentRecord);
        
        fireTableDataChanged();
    }
    
    void moveRecordDown(int rowIndex) {
        logger.finest("Moving row " + rowIndex + " down");

        BusinessObjectTableRecord currentRecord = tableData.remove(rowIndex);
        tableData.add(rowIndex + 1, currentRecord);
        
        fireTableDataChanged();
    }
    
    @Override
    public String toString() {
        return tableData.toString();
    }
}
