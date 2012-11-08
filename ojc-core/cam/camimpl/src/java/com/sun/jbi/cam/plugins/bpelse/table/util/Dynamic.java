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
 * @(#)Dynamic.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.table.util;

import com.sun.webui.jsf.component.Checkbox;
import com.sun.webui.jsf.component.Hyperlink;
import com.sun.webui.jsf.component.StaticText;
import com.sun.webui.jsf.component.Table;
import com.sun.webui.jsf.component.TableColumn;
import com.sun.webui.jsf.component.TableRowGroup;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.component.UIParameter;
import javax.faces.el.ValueBinding;

// This class provides functionality for dynamic tables.
@SuppressWarnings("unchecked")
public class Dynamic {
    public static final String CHECKBOX_ID = "select";
    public static final String HYPERLINK_ID = "link";

    // Default constructor.
    public Dynamic() {
    }

    // Note: When using tags in a JSP page, UIComponentTag automatically creates
    // a unique id for the component. However, when dynamically creating 
    // components, via a backing bean, the id has not been set. In this 
    // scenario, allowing JSF to create unique Ids may cause problems with
    // Javascript and components may not be able to maintain state properly. 
    // For example, if a component was assigned "_id6" as an id, that means 
    // there were 5 other components that also have auto-generated ids. Let us 
    // assume one of those components was a complex component that, as part of 
    // its processing, adds an additional non-id'd child before redisplaying the
    // view. Now, the id of this component will be "_id7" instead of "_id6". 
    // Assigning your own id ensures that conflicts do not occur.

    // Get Table component. 
    //
    // @param id The component id.
    // @param title The table title text.
    public Table getTable(String id, String title) {
        // Get table.
        Table table = new Table();
        //table.setDeselectMultipleButton(true); // Show deselect multiple button.
        //table.setSelectMultipleButton(true); // Show select multiple button.
        table.setTitle(title); // Set title text.

        return table;
    }

    // Get TableRowGroup component with header.
    //
    // @param id The component id.
    // @param sourceData Value binding expression for model data.
    // @param selected Value binding expression for selected property.
    // @param header Value binding expression for row group header text.
    public TableRowGroup getTableRowGroup(String id, String sourceData,
            String selected, String header) {
        // Get table row group.
        TableRowGroup rowGroup = new TableRowGroup();
        rowGroup.setId(id); // Set id.
        //rowGroup.setSourceVar("name"); // Set source var.
        rowGroup.setSourceVar("bpinstance"); // Set source var.
        rowGroup.setHeaderText(header); // Set header text.
        setValueBinding(rowGroup, "selected", selected); // Set row highlight.
        setValueBinding(rowGroup, "sourceData", sourceData); // Set source data.

        return rowGroup;
    }

    public TableRowGroup getTableRowGroup(String id, String sourceData,
            String header) {
        // Get table row group.
        TableRowGroup rowGroup = new TableRowGroup();
        rowGroup.setId(id); // Set id.
        rowGroup.setSourceVar("bpinstance"); // Set source var.
        rowGroup.setHeaderText(header); // Set header text.
        setValueBinding(rowGroup, "sourceData", sourceData); // Set source data.

        return rowGroup;
    }
    
    // Get TableColumn component.
    //
    // @param id The component id.
    // @param sort Value binding expression for column sort.
    // @param align The field key for column alignment.
    // @param header The column header text.
    // @param selectId The component id used to select table rows.
    public TableColumn getTableColumn(String id, String sort, String align,
            String header, String selectId) {
        // Get table column.
        TableColumn col = new TableColumn();
        col.setId(id); // Set id.
        col.setSelectId(selectId); // Set id used to select table rows.
        col.setHeaderText(header); // Set header text.
        col.setAlignKey(align); // Set align key.
        
        if (sort != null ){
            setValueBinding(col, "sort", sort); // Set sort.
        }

        return col;
    }
        
    public TableColumn getTableColumn(String id, String header, String selectId, String align) {
        // Get table column.
        TableColumn col = new TableColumn();
        col.setId(id); // Set id.
        col.setSelectId(selectId); // Set id used to select table rows.
        col.setHeaderText(header); // Set header text.
        col.setAlignKey(align); // Set align key.
        return col;
    }

    // Get Checkbox component used for select column.
    //
    // @param id The component id.
    // @param selected Value binding expression for selected property.
    // @param selectedValue Value binding expression for selectedValue property.
    public Checkbox getCheckbox(String id, String selected, 
            String selectedValue) {
        // Get checkbox.
        Checkbox cb = new Checkbox();
        cb.setId(id); // Set id here and set row highlighting below.
        cb.setOnClick("setTimeout('initAllRows()', 0)");
        setValueBinding(cb, "selected", selected); // Set selected.
        setValueBinding(cb, "selectedValue", selectedValue); // Set selected value.

        return cb;
    }

    // Get Hyperlink component.
    //
    // @param id The component id.
    // @param text Value binding expression for text.
    // @param action Method binding expression for action.
    // @param parameter Value binding expression for parameter.
    public Hyperlink getHyperlink(String id, String text, String action,
            String parameter) {
        // Get hyperlink.
        Hyperlink hyperlink = new Hyperlink();
        hyperlink.setId(id); // Set id.
        setValueBinding(hyperlink, "text", text); // Set text.
        setMethodBinding(hyperlink, "action", action); // Set action.

        // Create paramerter.
        UIParameter param = new UIParameter();
        param.setId(id + "_param");
        param.setName("param");
        setValueBinding(param, "value", parameter); // Set parameter.
        hyperlink.getChildren().add(param);

        return hyperlink;
    }

    // Get StaticText component.
    //
    // @param text Value binding expression for text.
    public StaticText getText(String text) {
        // Get static text.
        StaticText staticText = new StaticText();
        setValueBinding(staticText, "text", text); // Set text.

        return staticText;
    }

    // Set TableRowGroup children.
    //
    // @param rowGroup The TableRowGroup component.
    // @param cbSort Value binding expression for cb sort.
    // @param cbSelected Value binding expression for cb selected property.
    // @param cbSelectedValue Value binding expression for cb selectedValue property.
    // @param action The Method binding expression for hyperlink action.
    // @param showHeader Flag indicating to display column header text.
    
    public void setTableRowGroupChildren(TableRowGroup rowGroup, String cbSort,
        String cbSelected, String cbSelectedValue, String action,
            boolean showHeader) {
        // UI guidelines recomend no headers for second row group.
        String header1 = showHeader ? "Last Name" : null;
        String header2 = showHeader ? "First Name" : null;

        // Get columns.
        TableColumn col1 = getTableColumn(
            "col0", cbSort, null, null, CHECKBOX_ID);
        TableColumn col2 = getTableColumn(
            "col1", "#{name.value.last}", "last", header1, null);            
        TableColumn col3 = getTableColumn(
            "col2", "#{name.value.first}", "first", header2, null);
        
        // Get column components.
        Checkbox cb = getCheckbox(CHECKBOX_ID, cbSelected, cbSelectedValue);
        StaticText firstName = getText("#{name.value.first}");
       
        // If action was provided, add a hyperlink; otherwise, use static text.
        if (action != null) {
            Hyperlink lastName = getHyperlink(HYPERLINK_ID, 
                "#{name.value.last}", action,
                "#{name.value.last}");
            col2.getChildren().add(lastName);
        } else {
            StaticText lastName = getText("#{name.value.last}");
            col2.getChildren().add(lastName);
        }

        // Add Children.
        col1.getChildren().add(cb);
        col3.getChildren().add(firstName);
        rowGroup.getChildren().add(col1);
        rowGroup.getChildren().add(col2);
        rowGroup.getChildren().add(col3);
        
    }

    
    public void setTableRowGroupChildren(TableRowGroup rowGroup, String cbSelected, String cbSelectedValue,
            List<String> colNames, List<String> colValExprs){
                // Get column components.
        Checkbox cb = getCheckbox(CHECKBOX_ID, cbSelected, cbSelectedValue);
        TableColumn colSelect = getTableColumn(
            "col0", null, null, null, CHECKBOX_ID);
        colSelect.getChildren().add(cb);
        rowGroup.getChildren().add(colSelect);
        
        Iterator colNameIt = colNames.iterator();
        Iterator colValExprIt = colValExprs.iterator();
        while( colNameIt.hasNext() ){
            String colName = (String) colNameIt.next();
            String beanPropExpr = (String) colValExprIt.next();
            String alignKey = beanPropExpr.substring(beanPropExpr.lastIndexOf(".") + 1);
            System.out.println(">>>>>>>>>>>>> alignKey " + alignKey);
            TableColumn col = getTableColumn(colName, colName, beanPropExpr, alignKey);
            StaticText colValExpr = getText(beanPropExpr);
            col.getChildren().add(colValExpr);
            rowGroup.getChildren().add(col);
        }
    }


    // Helper method to set value bindings.
    //
    // @param component The UIComponent to set a value binding for.
    // @param name The name of the value binding.
    // @param value The value of the value binding.
    public void setValueBinding(UIComponent component, String name, 
            String value) {
        if (value == null) {
            return;
        }
        FacesContext context = FacesContext.getCurrentInstance();
        component.setValueBinding(name, context.getApplication().
            createValueBinding(value));
    }

    // Helper method to set method bindings.
    //
    // @param component The UIComponent to set a value binding for.
    // @param name The name of the method binding.
    // @param action The action of the method binding.
    public void setMethodBinding(UIComponent component, String name,
            String action) {
        if (action == null) {
            return;
        }
        FacesContext context = FacesContext.getCurrentInstance();
        component.getAttributes().put(name, context.getApplication().
            createMethodBinding(action, new Class[0]));
    }
}
