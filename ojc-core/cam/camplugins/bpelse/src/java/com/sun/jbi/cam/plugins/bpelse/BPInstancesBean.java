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
 * @(#)BPInstancesBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse;

import com.sun.jbi.cam.plugins.bpelse.table.util.Group;
import com.sun.webui.jsf.component.Table;
import com.sun.webui.jsf.component.TableRowGroup;

import com.sun.data.provider.TableDataProvider;
import com.sun.data.provider.impl.ObjectListDataProvider;

import com.sun.jbi.cam.plugins.bpelse.datamodel.State;
import com.sun.jbi.cam.plugins.bpelse.StateController;
        
import com.sun.jbi.cam.plugins.bpelse.table.util.Dynamic;
import com.sun.jbi.cam.plugins.bpelse.table.util.Filter;
        
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author nnahata
 */
@SuppressWarnings("unchecked")
public class BPInstancesBean {
        
    private Dynamic dynamic = null; // Dynamic util.
    
    private Table table = null; // Table component.
    private TableRowGroup rowGroup1 = null;
    private TableDataProvider provider = null; // Data provider.
    private TableDataProvider statesProvider = null; // Data provider.
    
    private Filter filter = null;    
    
    public List<State> states = null;
    public List<BPInstance> instances = null;
    private List<String> listColNames=null;
    private List<String> listColValExprs=null;

    private Group groupA = null; 
    
    /** Creates a new instance of BPInstancesTableBean */
    public BPInstancesBean() {
         //filter = new Filter(this);
         dynamic = new Dynamic();
         
         initDummyData();
         provider = new ObjectListDataProvider(instances);
    }

    
    
    public Filter getFilter(){
        return filter;
    }
    
    public TableDataProvider getInstances(){
        return provider;
    }
     

    public TableRowGroup getTableRowGroup(){
        return rowGroup1;
    }
    
    // Get Table component.
    public Table getTable() {
        if (table == null) {
            rowGroup1 = dynamic.getTableRowGroup("BPInstancesRowGroup",
                "#{BPInstancesBean.instances}", null);
            
            //dynamic.setTableRowGroupChildren(rowGroup1, mapTableColsExprs);
            dynamic.setTableRowGroupChildren(rowGroup1, 
                    null,
                    null,
                    listColNames, listColValExprs);
            
            // Get table.
            table = dynamic.getTable("table1", "BPEL Instances");
            table.getChildren().add(rowGroup1);
        }
        return table;
    }

        // Get Table component.
    public Table getStatesTable() {
        if (table == null) {
            rowGroup1 = dynamic.getTableRowGroup("BPInstancesRowGroup",
                "#{state.states}", null);
//            
//            rowGroup1 = dynamic.getTableRowGroup("BPInstancesRowGroup",
//                "#{BPInstancesBean.states}", null);
            
            listColNames = new ArrayList<String>();
            listColValExprs = new ArrayList<String>();

            listColNames.add("ID");
            listColNames.add("ENGINE");
            listColNames.add("STATUS");
            listColNames.add("BPEL");

            listColValExprs.add("#{bpinstance.value.id}");
            listColValExprs.add("#{bpinstance.value.engineid}");
            listColValExprs.add("#{bpinstance.value.status}");
            listColValExprs.add("#{bpinstance.value.bpelid}");
            
            dynamic.setTableRowGroupChildren(rowGroup1, 
                    null,
                    null,
                    listColNames, listColValExprs);
            
            // Get table.
            table = dynamic.getTable("table1", "BPEL Instances");
            table.getChildren().add(rowGroup1);
        }
        return table;
    }
    // Set Table component.
    //
    // @param table The Table component.
    public void setTable(Table table) {
        this.table = table;
    } 
    
    public void setStatesTable(Table table) {
        this.table = table;
    } 
    
    public Group getGroupA() {
        if (groupA != null) {
            return groupA;
        }
        // Create List with all names.
        ArrayList newNames = new ArrayList();
        for (int i = instances.size() - 1; i >= 0; i--) {
            newNames.add(instances.get(i));
        }
        return (groupA = new Group(newNames));
    }
    
    // Reset values so next visit starts fresh.
    private void reset() {
        table = null;
        dynamic = new Dynamic();
    }
    
    private List<BPInstance> getBPInstances(){
        //dummy BP instances
        List<BPInstance> dummyInstances = new ArrayList<BPInstance>();        
        String[] varValues1 = new String[] {"1_value1","1_value2", "1_value3"};
        String[] varValues2 = new String[] {"2_value1","2_value2", "2_value3"};
        
        BPInstance bp1 = new BPInstance("1", "1_attr1", "1_attr2", varValues1);
        BPInstance bp2 = new BPInstance("2", "2_attr1", "2_attr2", varValues2);
        
        dummyInstances.add(bp1);
        dummyInstances.add(bp2);
        
        return dummyInstances;
    }
    
    private void initDummyData(){
        instances = getBPInstances();

        listColNames = new ArrayList<String>();
        listColValExprs = new ArrayList<String>();
        
        listColNames.add("id");
        listColNames.add("attr1");
        listColNames.add("attr2");
        
        listColValExprs.add("#{bpinstance.value.id}");
        listColValExprs.add("#{bpinstance.value.attr1}");
        listColValExprs.add("#{bpinstance.value.attr2}");
        
        //number of dynamic cols
        int numVars = instances.get(0).getVarValues().length;
        
        for(int i =0; i<numVars; i++){
            String varName = "dynamicVar" + (i +1);
            listColNames.add(varName);
            String varValExpr = "#{bpinstance.value.varValues[" + i +"]}";
            listColValExprs.add(varValExpr);
        }
        
    }
    
}
