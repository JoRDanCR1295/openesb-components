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
 * @(#)Select.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.table.util;

import com.sun.data.provider.FieldKey;
import com.sun.data.provider.RowKey;
import com.sun.data.provider.TableDataProvider;
import com.sun.webui.jsf.event.TableSelectPhaseListener;

import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

// This class provides functionality for select tables.
//
// Note: UI guidelines recomend that rows should be unselected when no longer in
// view. For example, when a user selects rows of the table and navigates to
// another page. Or, when a user applies a filter or sort that may hide
// previously selected rows from view. If a user invokes an action to delete
// the currently selected rows, they may inadvertently remove rows not
// displayed on the current page. Using TableSelectPhaseListener ensures
// that invalid row selections are not rendered by clearing selected state
// after the render response phase.
public class Select {
    private TableSelectPhaseListener tspl = null; // Phase listener.
    private Group group = null; // Group util.

    // Default constructor.
    public Select(Group group) {
        this.group = group;
        tspl = new TableSelectPhaseListener();
    }

    // Construct an instance. The given flag indicates that selected
    // objects should not be cleared after the render response phase.
    public Select(Group group, boolean keepSelected) {
        this.group = group;
        tspl = new TableSelectPhaseListener(keepSelected);          
    }

    // Clear selected state from phase listener (e.g., when deleting rows).
    public void clear() {
        tspl.clear();
    }

    // Test flag indicating that selected objects should not be cleared.
    public boolean isKeepSelected() {
        return tspl.isKeepSelected();
    }

    // Set flag indicating that selected objects should not be cleared.
    public void keepSelected(boolean keepSelected) {
        tspl.keepSelected(keepSelected);
    }

    // Get selected property.
    public Object getSelected() {
	return tspl.getSelected(getTableRow());
    }

    // Set selected property.
    public void setSelected(Object object) {
        RowKey rowKey = getTableRow();
        if (rowKey != null) {
            tspl.setSelected(rowKey, object);
        }
    }

    // Get selected value property.
    public Object getSelectedValue() {
        RowKey rowKey = getTableRow();
        return (rowKey != null) ? rowKey.getRowId() : null;
    }

    // Get the selected state -- Sort on checked state only.
    public boolean getSelectedState() {
        // Typically, selected state is tested by comparing the selected and 
        // selectedValue properties. In this example, however, the phase 
        // listener value is not null when selected.
        return getSelectedState(getTableRow());
    }

    // Get the selected state.
    public boolean getSelectedState(RowKey rowKey) {
        return tspl.isSelected(rowKey);
    }
    
    public boolean getRunning() {
       RowKey rowKey = getTableRow();
       TableDataProvider provider = group.getInstances();
       FieldKey fieldKey = getFieldKeyForName(provider,"status");
       String state = (String) provider.getValue(fieldKey,rowKey);
       boolean isRunning = state.equalsIgnoreCase("RUNNING");
       return isRunning;
    }

    public FieldKey getFieldKeyForName(TableDataProvider provider,
            String fieldKeyName) {
       FieldKey[] fieldKeyArray = provider.getFieldKeys();
       FieldKey fieldKey = null;
       for (int index = 0; index < fieldKeyArray.length; index++) {
           if(fieldKeyArray[index].getDisplayName().equals(fieldKeyName)) {
               fieldKey = fieldKeyArray[index];
               break;
           }
        }
       return  fieldKey;       
    }
    
    // Get current table row.
    //
    // Note: To obtain a RowKey for the current table row, the use the same 
    // sourceVar property given to the TableRowGroup component. For example, if 
    // sourceVar="name", use "#{name.tableRow}" as the expression string.
    /*
    private RowKey getTableRow() {
        FacesContext context = FacesContext.getCurrentInstance();
        ValueBinding vb = context.getApplication().createValueBinding(
            "#{name.tableRow}");
        return (RowKey) vb.getValue(context);
    }
     */
    
        private RowKey getTableRow() {
        FacesContext context = FacesContext.getCurrentInstance();
        ValueBinding vb = context.getApplication().createValueBinding(
            "#{instance.tableRow}");
        return (RowKey) vb.getValue(context);
    }
}
