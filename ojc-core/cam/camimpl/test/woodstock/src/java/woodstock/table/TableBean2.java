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
 * @(#)TableBean2.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package woodstock.table;

import com.sun.data.provider.RowKey;
import com.sun.webui.jsf.component.TableRowGroup;
import java.io.Serializable;
import java.util.ArrayList;
import com.sun.data.provider.TableDataProvider;
import com.sun.data.provider.impl.ObjectArrayDataProvider;
import com.sun.data.provider.impl.ObjectListDataProvider;


public class TableBean2 implements Serializable {
    
    private TableDataProvider provider = null; // Data provider.
    private Group group = null;
    
        protected static final Name[] names = {
            new Name("George", "Washington"),
            new Name("Ben", "Franklin"),
            new Name("John", "Kennedy"),
            new Name("Dwight", "Eisenhower"),
            new Name("Ronald", "Reagan"),
            new Name("Bill", "Clinton"),
            new Name("Samuel", "Adams")
    };
    
    // Default constructor.
    public TableBean2() {
    }
    
        // Get data provider.
    public TableDataProvider getNames() {
        // Create List with all names.
        ArrayList newNames = new ArrayList();
        for (int i = 0; i<names.length ; i++) {
            newNames.add(names[i]);
        }
        provider = new ObjectListDataProvider(newNames);
        return provider;
    }
    
    public Group getGroup() {
        if ( group==null ) {
            ArrayList dataList = new ArrayList();
            // create some data
            Display display = new Display("name1","desc1","status1");
            dataList.add(display);
            display = new Display("name2","desc2","status2");
            dataList.add(display);
            display = new Display("name3","desc3","status3");
            dataList.add(display);
            display = new Display("name4","desc4","status4");
            dataList.add(display);
            group = new Group(dataList);
        }
        return group;
    }
    
    public String add() {
        System.out.println("Add called...");
        // reset
        group=null;
        return "";
    }

    public String edit() {
        System.out.println("Edit called...");
        return "";
    }    

    public String delete() {
        System.out.println("## Delete called...");
        
        // Since mutiple examples are using the same beans, the binding
        // simply tells us that checkbox state is maintained arcoss pages.
        /**
        if (group.getSelect().isKeepSelected()) {
            // If we got here, then we're maintaining state across pages.
            delete(group.getTableRowGroup().getSelectedRowKeys());
        } else {
         ***/
            // If we got here, then we're using the phase listener and must
            // take filtering, sorting, and pagination into account.
            TableRowGroup tableRowGroup = group.getTableRowGroup();
            System.out.println("## tableRowGroup: "+tableRowGroup);
            RowKey[] rowKeys = tableRowGroup.getRenderedSelectedRowKeys();
            System.out.println("## rowkeys from tableRowGroup= "+rowKeys);
            delete(rowKeys);
        //}
        
        return "";
    }    
    

    // Action to remove rows from ObjectListDataProvider.
    private void delete(RowKey[] rowKeys) {
        System.out.println("### delete: rowkeys="+rowKeys);
        if (rowKeys == null) {
            return;
        }
        TableDataProvider provider = group.getData();
        Select select = group.getSelect();
        for (int i = 0; i < rowKeys.length; i++) {
            RowKey rowKey = rowKeys[i];
            if (provider.canRemoveRow(rowKey)) {
                System.out.println("## removing rowkey:"+rowKey);
                String name= (String)provider.getValue(provider.getFieldKey("name"),rowKey);
                System.out.println("## name: "+name);
                boolean isSelected = select.getSelectedState(rowKey);
                System.out.println("## selected: "+isSelected);
                provider.removeRow(rowKey);
            }
        }
        ((ObjectListDataProvider) provider).commitChanges(); // Commit.
        group.getSelect().clear(); // Clear phase listener.
        
    }    
    
    
    
}
