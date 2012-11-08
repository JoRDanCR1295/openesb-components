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
 * @(#)BPCustomCharts.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.bpvisualizer;

import com.sun.jbi.cam.manager.framework.common.ChartType;
import com.sun.jbi.cam.plugins.bpelse.DBManager;
import com.sun.webui.jsf.model.Option;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.data.general.Dataset;
import org.jfree.data.general.DefaultPieDataset;
import org.jfree.data.general.PieDataset;
import org.jfree.data.general.Series;
import org.jfree.data.time.Day;
import org.jfree.data.time.Month;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;
import org.jfree.data.xy.DefaultXYDataset;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.TimeZone;
import java.util.logging.Logger;


/**
 *
 * @author rdamir
 */
public final class BPCustomCharts extends ChartSupport {
    
    private Logger logger = Logger.getLogger(BPCustomCharts.class.getName());
    private BPCustomChartBean bpCustomBean;
    private BPVisualizerDBHelper dbHelper;
    private DBManager dbManager;
    
    private enum CHART_GROUP { TIMESERIES_CHARTS,LINE_CHARTS,PIE_CHARTS,BAR_CHARTS};
    private Option[] timeSeriesChartOptions = { 
            new Option(ChartType.Timeseries.getChartType(), ChartType.Timeseries.getDescription())
    };
    private Option[] lineChartOptions = { 
            new Option(ChartType.Line.getChartType(), ChartType.Line.getDescription()),
            new Option(ChartType.XYline.getChartType(), ChartType.XYline.getDescription())
    };
    private Option[] pieChartOptions = { 
            new Option(ChartType.Pie.getChartType(), ChartType.Pie.getDescription()),
            new Option(ChartType.Ring.getChartType(), ChartType.Ring.getDescription())
    };
    private Option[] barChartOptions = { 
            new Option(ChartType.Bar.getChartType(), ChartType.Bar.getDescription()),
            new Option(ChartType.StackedBar.getChartType(), ChartType.StackedBar.getDescription())
    };
            
    /** Creates a new instance of BPCustomCharts */
    public BPCustomCharts() {
      dbHelper = new BPVisualizerDBHelper();
      dbManager = new DBManager();
      dbHelper.setDBManager(dbManager);
      
      FacesContext BPCustomcontext =  FacesContext.getCurrentInstance();
      ValueBinding bpCustomValueBinding = 
            BPCustomcontext.getApplication().createValueBinding(BPCustomChartBean.BEAN_NAME);
       bpCustomBean = 
         (BPCustomChartBean) bpCustomValueBinding.getValue(BPCustomcontext);        
   }
    
     public Option[] getChartMenuTypeOptions() {
         BPCustomChartBean.VAR_COMBINATION combination = bpCustomBean.getCurrentVarTypeCombination();
 
        Option[] chartMenuTypeOptions = null;
        CHART_GROUP[] charts = null; 
        
        switch (combination) {
        case NUMERIC_NUMERIC:
            charts = new CHART_GROUP[] {CHART_GROUP.LINE_CHARTS};
            chartMenuTypeOptions = getChartOptions(charts);
            break;
        case NUMERIC_BOOLEAN:
            charts = new CHART_GROUP[] {CHART_GROUP.PIE_CHARTS};
            chartMenuTypeOptions = getChartOptions(charts);
            break;
        case NUMERIC_STRING:
            charts = new CHART_GROUP[] {CHART_GROUP.PIE_CHARTS};
            chartMenuTypeOptions = getChartOptions(charts);
            break;
        case NUMERIC_DATETIME:
            charts = new CHART_GROUP[] {CHART_GROUP.TIMESERIES_CHARTS};
            chartMenuTypeOptions = getChartOptions(charts);
            this.setChartTypeMenuSelectedOption(ChartType.Timeseries.getChartType());
            break;

        default:
            // invalid 
            chartMenuTypeOptions = new Option[1];
            chartMenuTypeOptions[0] = new Option(SELECT_CHART_STRING);
            break;
        }
        return chartMenuTypeOptions;
    }

    
    
    
    
    public Dataset getChartData() {
        // get selections from the bean.
        BPCustomChartBean.VAR_TYPE xType = 
            bpCustomBean.getCurrentXVaribleType();
        BPCustomChartBean.VAR_TYPE yType = 
            bpCustomBean.getCurrentYVaribleType();
        // xid & yid are composite values of the bpelid.variableid
        String xId =  bpCustomBean.getAxis_X_SelectedOption();
        String yId =  bpCustomBean.getAxis_Y_SelectedOption();
        String chartType = getChartMenuType();
        BPCustomChartQuery queryClass = new BPCustomChartQuery();
        queryClass.setXID(xId);
        queryClass.setYID(yId);
        queryClass.setXType(xType);
        queryClass.setYType(yType);
        
        addLimitsToQueryClass(queryClass);
        FacesContext BPCustomcontext =  FacesContext.getCurrentInstance();

        try {
            ResultSet resultSet =  dbHelper.getCustomDataSet(queryClass);
            return processResultSet(resultSet,queryClass,chartType);
        } catch (Exception e) {
            e.printStackTrace();
            return getDefaultDateSet(chartType);
        }finally {
            dbManager.closeGenericConnnection();
        }
        
    }
 
    private void addLimitsToQueryClass(BPCustomChartQuery queryClass) {
        BPCustomChartBean.VAR_TYPE xType = 
            bpCustomBean.getCurrentXVaribleType();
        BPCustomChartBean.VAR_TYPE yType = 
            bpCustomBean.getCurrentYVaribleType();
        String xLowerLimit = null;
        if(xType ==  BPCustomChartBean.VAR_TYPE.DATETIME && 
                bpCustomBean.getDateLowerLimit_X_Axis() != null) {
            xLowerLimit = new Timestamp(bpCustomBean.getDateLowerLimit_X_Axis().getTime()).toString();
        } else if (xType ==  BPCustomChartBean.VAR_TYPE.NUMERIC ) {
            xLowerLimit = bpCustomBean.getLowerLimit_X_Axis();
        }
        
        queryClass.setXLowValue(xLowerLimit);

        String xUpperLimit = null;
        if(xType ==  BPCustomChartBean.VAR_TYPE.DATETIME  && 
                bpCustomBean.getDateUpperLimit_X_Axis() != null ) {
            xUpperLimit = new Timestamp(bpCustomBean.getDateUpperLimit_X_Axis().getTime()).toString();
        } else if (xType ==  BPCustomChartBean.VAR_TYPE.NUMERIC ) {
            xUpperLimit = bpCustomBean.getLowerLimit_X_Axis();
        }
        
        queryClass.setXHighValue(xUpperLimit);
        
        String yLowerLimit = null;
        if(yType ==  BPCustomChartBean.VAR_TYPE.DATETIME  && 
                bpCustomBean.getDateLowerLimit_Y_Axis() != null) {
            yLowerLimit = new Timestamp(bpCustomBean.getDateLowerLimit_Y_Axis().getTime()).toString();
        } else if (yType ==  BPCustomChartBean.VAR_TYPE.NUMERIC ) {
            yLowerLimit = bpCustomBean.getLowerLimit_Y_Axis();
        }
        
        queryClass.setYLowValue(yLowerLimit);

        String yUpperLimit = null;
        if(yType ==  BPCustomChartBean.VAR_TYPE.DATETIME  && 
                bpCustomBean.getDateUpperLimit_Y_Axis() != null) {
            yUpperLimit = new Timestamp(bpCustomBean.getDateUpperLimit_Y_Axis().getTime()).toString();
        } else if (yType ==  BPCustomChartBean.VAR_TYPE.NUMERIC ) {
            yUpperLimit = bpCustomBean.getUpperLimit_Y_Axis();
        }
        
        queryClass.setYHighValue(yUpperLimit);
        
        
        
    }
    
    
    private Option[] getChartOptions(CHART_GROUP[] charts ) {
        Option[] currentChartOptions = null;
        // calc the number of options needed
        int chartsCount = 0;
        for (int index = 0; index < charts.length; index++) {
            switch (charts[index]) {
            case TIMESERIES_CHARTS:
                chartsCount += timeSeriesChartOptions.length;
                break;
            case LINE_CHARTS:
                chartsCount += lineChartOptions.length;
                break;
            case PIE_CHARTS:
                chartsCount += pieChartOptions.length;
                break;
            case BAR_CHARTS:
                chartsCount += barChartOptions.length;
                break;

            default:
                break;
            }
            
        }
        int chartIndex = 0;
        if(chartsCount > 1) {
            // create the options array add 1 for the default
            currentChartOptions = new Option[chartsCount+1];
            // add default option
            currentChartOptions[chartIndex++] = new Option(SELECT_CHART_STRING);
        }else{
            currentChartOptions = new Option[chartsCount];
        }
        // now add the charting options
        for (int index = 0; index < charts.length; index++) {
            switch (charts[index]) {
            case TIMESERIES_CHARTS:
                addChartsToOptions(timeSeriesChartOptions,
                        currentChartOptions,chartIndex);
                chartIndex += lineChartOptions.length;
               break;
            case LINE_CHARTS:
                addChartsToOptions(lineChartOptions,
                        currentChartOptions,chartIndex);
                chartIndex += lineChartOptions.length;
               break;
            case PIE_CHARTS:
                addChartsToOptions(pieChartOptions,
                         currentChartOptions,chartIndex);
                chartIndex += pieChartOptions.length;
                break;
            case BAR_CHARTS:
                addChartsToOptions(barChartOptions,
                        currentChartOptions,chartIndex);
                chartIndex += pieChartOptions.length;
                break;

            default:
                break;
            }
            
        }
        return currentChartOptions;

    }

    private void addChartsToOptions(Option[] chartOptionGroup,
            Option[] currentChartOptions, int nextOptionIndex) {
        
        for (int index = 0; index < chartOptionGroup.length; index++) {
            currentChartOptions[nextOptionIndex++] = chartOptionGroup[index];
        }
    }
    
    private Dataset getDefaultDateSet(String chartType) {
        if(chartType.equalsIgnoreCase(ChartType.Timeseries.getChartType()) || 
            chartType.equalsIgnoreCase(ChartType.Line.getChartType())) {
            return new DefaultXYDataset();
        } else {
            if(chartType.equalsIgnoreCase(ChartType.Pie.getChartType()) || 
                    chartType.equalsIgnoreCase(ChartType.Ring.getChartType())) {
              return new DefaultPieDataset();
            }
        }
        return new DefaultCategoryDataset();
    }
    
    private Dataset processResultSet(ResultSet resultset,
            BPCustomChartQuery customQueryData,String chartType) throws SQLException{

        BPCustomChartBean.VAR_COMBINATION combination = bpCustomBean.getCurrentVarTypeCombination();
        TimeSeries timeSeries = null;
        XYSeries xySeries = null;
        DefaultPieDataset pieDataset = null;
        DefaultCategoryDataset barDataset = null;
        switch (combination) {
            case NUMERIC_BOOLEAN:
                if(isPieChart()) {
                  pieDataset = new DefaultPieDataset();
                }else {
                  barDataset = new DefaultCategoryDataset();
                }
                break;
            case NUMERIC_DATETIME:
                timeSeries = new TimeSeries(" date ");
                break;
            case NUMERIC_NUMERIC:
                xySeries = new XYSeries(" test numeric");
                break;
            case NUMERIC_STRING:
                pieDataset = new DefaultPieDataset();
                break;
        }
        
        List<RowData> dataList = new ArrayList<RowData>();
        // since column 3 & 4 can change based on the var. type selected
        // we using colum index and not column name to get the data.
        while (resultset.next() ){
            String stateId = resultset.getString(1);
            String  varibleId= resultset.getString(2);
            Object xValue = getValue(resultset,customQueryData.getXType(),3);
            Object yValue = getValue(resultset,customQueryData.getYType(),4);
            logger.finest("StateID=" + stateId + " varibleId=" + varibleId +
                    " xValue=" + xValue + " yValue=" + yValue);
            // collect the data for later processing
            dataList.add(new RowData(stateId,varibleId,xValue,yValue,
                    customQueryData.getXType(),customQueryData.getYType()));
        }
        List<RowData> filteredDataList = filterRowData(dataList);
        // the return list alway has even number of entries or it is
        // an empty collection
        if(filteredDataList.size() == 0) {
            return getDefaultDateSet(chartType);
        }
        // identify which axis var is first - the var id are defined
        // in the bpel and not correlate to the selection made
        // in the web UI
        boolean isXVarFirst = identifyFirstEntry(filteredDataList.get(0));
        // populate the series
        for (int index= 0; index < filteredDataList.size(); index++) {
            Object xValue = null;
            Object yValue = null;
            BPCustomChartBean.VAR_TYPE xType = null;
            if(isXVarFirst) {
                xType = filteredDataList.get(index).getEntryType();
                xValue = filteredDataList.get(index++).getXValue();
                yValue = filteredDataList.get(index).getYValue();
            }else {
                yValue = filteredDataList.get(index++).getYValue();
                xValue = filteredDataList.get(index).getXValue();
                xType = filteredDataList.get(index).getEntryType();
            }
            Object numberObject = null;
            switch (combination) {
                case NUMERIC_BOOLEAN:
                    Object booelanObject = xType == BPCustomChartBean.VAR_TYPE.BOOLEAN ?
                            xValue : yValue;  
                    numberObject = xType != BPCustomChartBean.VAR_TYPE.BOOLEAN ?
                            xValue : yValue;
                    if(isPieChart()) {
                        pieDataset.setValue(booelanObject.toString(), (Number)numberObject); 
                    } else {
                        barDataset.addValue((Number)numberObject, booelanObject.toString(), new String());                     
                    }
                    break;
                case NUMERIC_DATETIME:
                    Object dateObject = xType == BPCustomChartBean.VAR_TYPE.DATETIME ?
                            xValue : yValue;
                    Day dayObject = new Day((Date)dateObject);
                    numberObject = xType != BPCustomChartBean.VAR_TYPE.DATETIME ?
                            xValue : yValue;
                    timeSeries.add(dayObject, (Number)numberObject);
                    break;
                case NUMERIC_NUMERIC:
                    xySeries.add((Number)xValue, (Number)yValue);
                    break;
                case NUMERIC_STRING:
                    Object stringObject = xType == BPCustomChartBean.VAR_TYPE.STRING ?
                            xValue : yValue;  
                    numberObject = xType != BPCustomChartBean.VAR_TYPE.STRING ?
                            xValue : yValue;
                    pieDataset.setValue((String)stringObject, (Number)numberObject);
                    break;
            }
            
        }
        Dataset chartSet = null;
        switch (combination) {
            case NUMERIC_BOOLEAN:
                if(isPieChart()) {
                    chartSet = pieDataset; 
                } else {
                    chartSet = barDataset; 
                }
                
                break;
            case NUMERIC_DATETIME:
                TimeZone timezone = TimeZone.getTimeZone("GMT");
                TimeSeriesCollection timeseriescollection = new TimeSeriesCollection(timezone);
                timeseriescollection.addSeries(timeSeries);
                chartSet = timeseriescollection;
                break;
            case NUMERIC_NUMERIC:
                chartSet = new XYSeriesCollection( xySeries );
                break;
            case NUMERIC_STRING:
                chartSet = pieDataset;
                break;
        }
        
       return chartSet;
    }
    
    private List<RowData> filterRowData(List<RowData> dataList){
        List<RowData> filteredDataList = new ArrayList<RowData>();
        int rowCount = dataList.size();
        int index= 0;
        RowData rowData1 =  null;
        RowData rowData2 =  null;
        
        while (index <= rowCount-1) {
            rowData1 =  dataList.get(index++);
            rowData2 =  dataList.get(index++);
            if(rowData1.getStateId().equals(rowData2.getStateId()) &&
               !rowData1.getVaribleId().equals(rowData2.getVaribleId()) &&
               (rowData1.xValue != null || rowData1.yValue != null) &&
               (rowData2.xValue != null || rowData2.yValue != null)) {
                // valid pair add the rows to the filtered list
                addRowsToFilteredList(filteredDataList,rowData1,rowData2);
                continue;
            }else {
                while(true) {
                    // the 1st row does not match the 2nd
                    // drop 1st row and continue to search for a match
                    rowData1 =  rowData2;
                    // isit the last row in the list?
                    if(index > rowCount -1) {
                        break;  // exit the inner loop
                    }
                    rowData2 =  dataList.get(index++);
                    // loop through the list in search of a state match
                    if(rowData1.getStateId().equals(rowData2.getStateId()) &&
                        !rowData1.getVaribleId().equals(rowData2.getVaribleId()) &&
                        (rowData1.xValue != null || rowData1.yValue != null) &&
                        (rowData2.xValue != null || rowData2.yValue != null)) {
                        // valid pair add the rows to the filtered list
                        addRowsToFilteredList(filteredDataList,rowData1,rowData2);
                        // exit the internal loop and return to the external
                        // loop in search of the next pair match
                        break;
                    }
                 }
            }
        }
        
        
        return filteredDataList;
    }
    
    private void addRowsToFilteredList( List<RowData> filteredDataList,
            RowData row1,RowData row2) {
        filteredDataList.add(row1);
        filteredDataList.add(row2);
    }
    
    private boolean identifyFirstEntry(RowData rowData) {
        boolean isXVarFirst = true;
        String varId = rowData.getVaribleId();
        String selectedXVarId = bpCustomBean.getAxis_X_SelectedOption();
        if(selectedXVarId.indexOf(varId) == -1) {
            isXVarFirst = false;
        }
        return isXVarFirst;
    }
    
    private Object getValue(ResultSet resultset,BPCustomChartBean.VAR_TYPE type,
            int columnIndex) {
        Object value = null;
        try {
            switch (type) {
            case BOOLEAN:
                value = resultset.getBoolean(columnIndex);
                break;
            case DATETIME:
                value = resultset.getDate(columnIndex);
                break;
            case NUMERIC:
                value = resultset.getDouble(columnIndex);
                break;
             case STRING:
                value = resultset.getString(columnIndex);
                break;
            }
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return value;
    }
 
    private class RowData {
        String stateId;
        String varibleId;
        Object xValue;
        Object yValue;
        BPCustomChartBean.VAR_TYPE xType;
        BPCustomChartBean.VAR_TYPE yType;
        
        private RowData(String stateId, String varibleId, Object value, Object value2,
                BPCustomChartBean.VAR_TYPE xType, BPCustomChartBean.VAR_TYPE yType) {
            this.stateId = stateId;
            this.varibleId = varibleId;
            xValue = value;
            yValue = value2;
            this.xType = xType;
            this.yType = yType;
        }
        
        private String getStateId() {
            return stateId;
        }
        
        private String getVaribleId() {
            return varibleId;
        }
        
        private Object getXValue() {
            return xValue;
        }
        
        private Object getYValue() {
            return yValue;
        }
        
        private BPCustomChartBean.VAR_TYPE getEntryType() {
            if(xValue != null) {
                return xType;
            }
            return yType;
        }
        
    }
}
