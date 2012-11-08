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
 * @(#)BPStatusCharts.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.bpvisualizer;

import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.manager.framework.common.BarChartDataBase;
import com.sun.jbi.cam.manager.framework.common.ChartType;
import com.sun.jbi.cam.plugins.bpelse.DBManager;
import com.sun.webui.jsf.model.Option;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.jfree.data.general.Dataset;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.data.general.DefaultPieDataset;

/**
 *
 * @author rdamir
 */
public final class BPStatusCharts extends ChartSupport {
 
    static String CATEGORY_BPINSTNACE_DONE;
    static String CATEGORY_BPINSTNACE_RUNNING;
   
    static {
        CATEGORY_BPINSTNACE_DONE = 
                 Messages.getString("bpvisualizer_BpelInstnace_done");
        CATEGORY_BPINSTNACE_RUNNING  = 
                Messages.getString("bpvisualizer_BpelInstnace_running");
    }
    private DBHelper dbHelper;
    
    /** Creates a new instance of BPStatusCharts */
    public BPStatusCharts() {
        dbHelper =  new DBHelper();
        setChartTypeOptions(true);
    }

    public void setChartTypeOptions(boolean includePieCharts) {
        Option[] chartMenuTypeOptions = null;
        if(includePieCharts) {
            chartMenuTypeOptions = new Option[4];
            chartMenuTypeOptions[0] = new Option(ChartType.Bar.getChartType(), 
                    ChartType.Bar.getDescription());
            chartMenuTypeOptions[1] = new Option(ChartType.StackedBar.getChartType(), 
                    ChartType.StackedBar.getDescription());
            chartMenuTypeOptions[2] = new Option(ChartType.Pie.getChartType(), 
                    ChartType.Pie.getDescription());
            chartMenuTypeOptions[3] = new Option(ChartType.Ring.getChartType(), 
                    ChartType.Ring.getDescription());
        }else {
            chartMenuTypeOptions = new Option[2];
            chartMenuTypeOptions[0] = new Option(ChartType.Bar.getChartType(), 
                    ChartType.Bar.getDescription());
            chartMenuTypeOptions[1] = new Option(ChartType.StackedBar.getChartType(), 
                    ChartType.StackedBar.getDescription());
            
        }
        setChartMenuTypeOptions(chartMenuTypeOptions);
        
    }

   public Dataset getChartData() {
       if(aggregatedView && isPieChart()) {
            return dbHelper.getAggregateStatusdPieChartData();
       } else        if(aggregatedView && isBarChart()) {
           return getAggregateStatusdBarChartData(suNameList);
       } else if(!aggregatedView) {
           return dbHelper.getSUStatusdPBarChartData(serviceUnitSelected);
       }
           
       return new DefaultCategoryDataset();
       
   }

   private DefaultCategoryDataset getAggregateStatusdBarChartData(List<String> suNameList) {
    List<BPStatusBarChartData> dataList = 
               dbHelper.getAggregateStatusdPBarChartData();
       DefaultCategoryDataset dataSet = fixAggregateDataSet(dataList,suNameList);
       return dataSet;
   }

   @SuppressWarnings("unchecked")
   // the data return from the database if group by bpelid
   // since service unit can have multiple bpel we need to 
   // sum up the state of all the bpel in each service unit
   private DefaultCategoryDataset fixAggregateDataSet(List<BPStatusBarChartData> datalist,
           List<String> suNameList) {
       Map<String,Double> categoryMap = null;
       Map<String,Map> suMap = new HashMap<String,Map>();
       int dataSetEntryCount = datalist.size();
       for (String  suName : suNameList) {
           for (int index = 0; index < dataSetEntryCount; index++) {
               BPStatusBarChartData data = datalist.get(index);
               if(data.containInName(suName)) {
                   if(suMap.containsKey(suName)) {
                       categoryMap = suMap.get(suName);
                       String category = data.getCategory();
                       if(categoryMap.containsKey(category)) {
                         Double aggregatedValue = categoryMap.get(category);
                         // update count
                         categoryMap.put(category,aggregatedValue.doubleValue() +
                               data.getValue());  
                       } else {
                          categoryMap.put(category,new Double(data.getValue()));
                       }
                       
                   } else {
                       categoryMap = new HashMap<String,Double>();
                       categoryMap.put(data.getCategory(),
                               new Double(data.getValue()));
                       suMap.put(suName,categoryMap);
                   }
               } else {
                   continue;
               }
           }
           
       }
       
       return createBarDataSet(suMap);
   }
  
   private DefaultCategoryDataset getSUStatusdBarChartData(String suName) {
       DefaultCategoryDataset dataSet = 
               dbHelper.getSUStatusdPBarChartData(suName);
       return dataSet;
   }

  
   @SuppressWarnings("unchecked")
   private DefaultCategoryDataset createBarDataSet( Map<String,Map> suMap) {
       DefaultCategoryDataset dataSet = new DefaultCategoryDataset();
       Set<String> suKeys = suMap.keySet();
       for (String suKey : suKeys) {
           Map<String,Double> catMap = suMap.get(suKey);
           Set<String> catKeys = catMap.keySet();
           for (String category : catKeys) {
               dataSet.addValue(catMap.get(category).doubleValue(),
                       category,suKey);
           }
      }
    
       return dataSet;
   }
   
   private class DBHelper {
     private final static String GET_AGGREGATED_STATUS_QUERY = 
        "select status,count(status) as statecount from" +
            " BPELSE_SCHEMA.STATE group by status";
    private final static String GET_SERVICEUNITS_AGGREGATED_STATUS_QUERY =
     "select BPELID as suid ,status,count(status) as statecount  " +
         "from BPELSE_SCHEMA.STATE  group by status,BPELID order by BPELID"; 
    private final static String GET_GIVEN_SERVICEUNIT_AGGREGATED_STATUS_QUERY =
     "select bpelid as suid,status,count(status) as statecount " +
     "from BPELSE_SCHEMA.STATE  where bpelid like '%suName%' group by status,bpelid"; 
    
    private final static String STATUS_COLUMN = "status";
    private final static String COUNT_COLUMN = "statecount";
    private final static String SUID_COLUMN = "suid";
    private final static String STATE_DONE = "DONE";
    private final static String STATE_RUNNING = "RUNNING";
    private DBManager dbManager;

     public DBHelper() {
        dbManager =  new DBManager();
     }
    
     private DefaultPieDataset getAggregateStatusdPieChartData() {
       DefaultPieDataset dataSet = new DefaultPieDataset();
       ResultSet resultset = 
               dbManager.execGenericQuery(GET_AGGREGATED_STATUS_QUERY);
       if(resultset == null) {
           return dataSet;
       }
        try {
            
            while (resultset.next() ){
                 String state = resultset.getString(STATUS_COLUMN);
                 int  stateCount = resultset.getInt(COUNT_COLUMN);
                 // i18 the state value
                 if(state.equals(STATE_DONE)) {
                     state = BPStatusCharts.CATEGORY_BPINSTNACE_DONE;
                 } else if (state.equals(STATE_RUNNING)) {
                      state = BPStatusCharts.CATEGORY_BPINSTNACE_RUNNING;
                 }
                 dataSet.setValue(state,stateCount);
            }
        } catch (SQLException ex) {
            ex.printStackTrace();
            return dataSet;
       }finally {
           dbManager.closeGenericConnnection();
       }
       return dataSet;
   }

   private  List<BPStatusBarChartData> getAggregateStatusdPBarChartData() {
       List<BPStatusBarChartData> dataList = new ArrayList<BPStatusBarChartData>();
       ResultSet resultset = 
               dbManager.execGenericQuery(GET_SERVICEUNITS_AGGREGATED_STATUS_QUERY);
       if(resultset == null) {
           return dataList;
       }
        try {
            
            while (resultset.next() ){
                 String suid = resultset.getString(SUID_COLUMN);
                 String state = resultset.getString(STATUS_COLUMN);
                 int  stateCount = resultset.getInt(COUNT_COLUMN);
                 // i18 the state value
                 if(state.equals(STATE_DONE)) {
                     state = BPStatusCharts.CATEGORY_BPINSTNACE_DONE;
                 } else if (state.equals(STATE_RUNNING)) {
                      state = BPStatusCharts.CATEGORY_BPINSTNACE_RUNNING;
                 }
                 dataList.add(new BPStatusBarChartData(stateCount,state,suid));
            }
        } catch (SQLException ex) {
            ex.printStackTrace();
            return dataList;
       }finally {
           dbManager.closeGenericConnnection();
       }
       return dataList;
     }

   
   private  DefaultCategoryDataset getSUStatusdPBarChartData(String suName) {
       DefaultCategoryDataset dataSet = new DefaultCategoryDataset();
       if(suName == null || suName.length() == 0) {
           return dataSet;
       }
       String query = GET_GIVEN_SERVICEUNIT_AGGREGATED_STATUS_QUERY.replace("suName",
               suName);
       ResultSet resultset = dbManager.execGenericQuery(query);
       if(resultset == null) {
           return dataSet;
       }
        try {
            
            while (resultset.next() ){
                 String suidpath = resultset.getString(SUID_COLUMN);
                 String bpelName = suidpath.substring(suidpath.lastIndexOf("/")+1);
                 String state = resultset.getString(STATUS_COLUMN);
                 int  stateCount = resultset.getInt(COUNT_COLUMN);
                 // i18 the state value
                 if(state.equals(STATE_DONE)) {
                     state = BPStatusCharts.CATEGORY_BPINSTNACE_DONE;
                 } else if (state.equals(STATE_RUNNING)) {
                      state = BPStatusCharts.CATEGORY_BPINSTNACE_RUNNING;
                 }
                 dataSet.addValue(stateCount,state,bpelName);
            }
        } catch (SQLException ex) {
            ex.printStackTrace();
            return dataSet;
       }finally {
           dbManager.closeGenericConnnection();
       }
       return dataSet;
     }
  
       
   }
   
   private class BPStatusBarChartData extends BarChartDataBase {
       
    private BPStatusBarChartData(int xValue, String type, String yValue) {
        super(xValue,type,yValue);
    }

    private BPStatusBarChartData(String xValue, String type, String yValue) {
        super(xValue,type,yValue);
    }

    private boolean containInName(String suName) {
      return  getName().indexOf(suName) != -1 ? true : false;
    }
    
   }
}
