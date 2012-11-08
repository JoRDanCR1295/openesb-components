/*
 * @(#)CardinalityEditor.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
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

import java.awt.Component;
import javax.swing.DefaultCellEditor;
import javax.swing.JComboBox;
import javax.swing.JTable;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
public class DirectionEditor extends DefaultCellEditor {
    
    private static final long serialVersionUID = -2449366228226639442L;
    
    private Direction value = null;

    public DirectionEditor() {
        super(new JComboBox());
        setClickCountToStart(2);
        
        JComboBox comboBox = (JComboBox) getComponent();
        
        comboBox.addItem(Direction.INPUT_VALUE);
        comboBox.addItem(Direction.OUTPUT_VALUE);

        value = new Direction();
        
        comboBox.setSelectedItem(value.toString());
    }

    @Override
    public boolean stopCellEditing() {
        String editorValue = (String) super.getCellEditorValue();
        
        value = new Direction(editorValue);
            
        return super.stopCellEditing();
    }

    @Override
    public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected,
            int row, int column)
    {
        this.value = (Direction) value;
        
        JComboBox comboBox =
                (JComboBox) super.getTableCellEditorComponent(table, value, isSelected, row, column);

        if (value != null && comboBox != null) {
            comboBox.setSelectedItem(this.value.toString());
        }

        return comboBox;
    }

    @Override
    public Object getCellEditorValue() {
        return value;
    }
}
