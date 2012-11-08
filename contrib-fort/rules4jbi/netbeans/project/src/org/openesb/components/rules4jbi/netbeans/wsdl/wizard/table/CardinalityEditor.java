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

import java.awt.Color;
import java.awt.Component;
import javax.swing.DefaultCellEditor;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.border.LineBorder;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
public class CardinalityEditor extends DefaultCellEditor {
    
    private static final long serialVersionUID = -2449366228226639442L;
    
    private Cardinality value = null;

    public CardinalityEditor() {
        super(new JTextField());
        
        JTextField textField = (JTextField) getComponent();
        
        textField.setHorizontalAlignment(SwingConstants.TRAILING);
        
        value = new Cardinality();
        textField.setText(value.toString());
    }

    @Override
    public boolean stopCellEditing() {
        String editorValue = (String) super.getCellEditorValue();
        
        if (!Cardinality.isValidCardinalityValue(editorValue)) {
            ((JComponent) getComponent()).setBorder(new LineBorder(Color.RED));
            return false;
        }

        value = new Cardinality(editorValue);
            
        return super.stopCellEditing();
    }

    @Override
    public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected,
            int row, int column)
    {
        this.value = (Cardinality) value;
        
        ((JComponent) getComponent()).setBorder(new LineBorder(Color.BLACK));

        return super.getTableCellEditorComponent(table, value, isSelected, row, column);
    }

    @Override
    public Object getCellEditorValue() {
        return value;
    }
}
