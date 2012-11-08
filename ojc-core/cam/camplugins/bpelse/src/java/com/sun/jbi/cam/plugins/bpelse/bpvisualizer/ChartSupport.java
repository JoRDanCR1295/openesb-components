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
 * @(#)ChartSupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.bpvisualizer;

import com.sun.jbi.cam.plugins.bpelse.common.BpelsePluginMessages;
import com.sun.jbi.cam.manager.framework.common.ChartType;
import com.sun.webui.jsf.model.Option;
import com.sun.webui.jsf.component.Checkbox;
import com.sun.webui.jsf.component.DropDown;
import java.util.ArrayList;
import java.util.List;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.faces.event.ValueChangeEvent;
import org.jfree.data.general.Dataset;

/**
 *
 * @author rdamir
 */
public abstract class ChartSupport extends ChartingConstants{
    
    private static String THREE_DIMENSIONAL;
    private static String TWO_DIMENSIONAL;
    
    private static String VERTICAL_ORIENTATION;
    private static String HORIZONTAL_ORIENTATION;
    
    private String chartMenuType;
    protected  Option[] chartMenuTypeOptions;
    protected String chartTypeMenuSelectedOption;
    protected String isThreeDimensional;
    protected String graphOrientation;  
    protected boolean isThreeDimensionalEffect;
    protected boolean isHorizontalOrientation;
    
    protected List<String> suNameList;
    protected boolean aggregatedView;
    protected String serviceUnitSelected;
            
    
    /** Creates a new instance of BPVisualizerChartHelper */
    public ChartSupport() {
        THREE_DIMENSIONAL    = BpelsePluginMessages.getString("chart_effect_threeDimensional");
        TWO_DIMENSIONAL    = BpelsePluginMessages.getString("chart_effect_twoDimensional");
        
        VERTICAL_ORIENTATION    = BpelsePluginMessages.getString("chart_orientation_vertical");
        HORIZONTAL_ORIENTATION    = BpelsePluginMessages.getString("chart_orientation_horizontal");
        
        suNameList = new ArrayList<String>();
        
        chartMenuTypeOptions = new Option[23];
        chartMenuTypeOptions[0] = new Option(ChartType.Bar.getChartType(), ChartType.Bar.getDescription());
        chartMenuTypeOptions[1] = new Option(ChartType.StackedBar.getChartType(), ChartType.StackedBar.getDescription());
        chartMenuTypeOptions[2] = new Option(ChartType.Pie.getChartType(), ChartType.Pie.getDescription());
        chartMenuTypeOptions[3] = new Option(ChartType.Ring.getChartType(), ChartType.Ring.getDescription());
        chartMenuTypeOptions[4] = new Option(ChartType.Line.getChartType(), ChartType.Line.getDescription());
        chartMenuTypeOptions[5] = new Option(ChartType.Area.getChartType(), ChartType.Area.getDescription());
        chartMenuTypeOptions[6] = new Option(ChartType.StackedArea.getChartType(), ChartType.StackedArea.getDescription());
        chartMenuTypeOptions[7] = new Option(ChartType.Waterfall.getChartType(), ChartType.Waterfall.getDescription());
        chartMenuTypeOptions[8] = new Option(ChartType.Gantt.getChartType(), ChartType.Gantt.getDescription());
        chartMenuTypeOptions[9] = new Option(ChartType.Timeseries.getChartType(), ChartType.Timeseries.getDescription());
        chartMenuTypeOptions[10] = new Option(ChartType.XYline.getChartType(), ChartType.XYline.getDescription());
        chartMenuTypeOptions[11] = new Option(ChartType.Polar.getChartType(), ChartType.Polar.getDescription());
        chartMenuTypeOptions[12] = new Option(ChartType.Scatter.getChartType(), ChartType.Scatter.getDescription());
        chartMenuTypeOptions[13] = new Option(ChartType.XYArea.getChartType(), ChartType.XYArea.getDescription());
        chartMenuTypeOptions[14] = new Option(ChartType.XYStepArea.getChartType(), ChartType.XYStepArea.getDescription());
        chartMenuTypeOptions[15] = new Option(ChartType.XYStep.getChartType(), ChartType.XYStep.getDescription());
        chartMenuTypeOptions[16] = new Option(ChartType.Bubble.getChartType(), ChartType.Bubble.getDescription());
        chartMenuTypeOptions[17] = new Option(ChartType.Candlestick.getChartType(), ChartType.Candlestick.getDescription());
        chartMenuTypeOptions[18]= new Option(ChartType.BoxandWhisker.getChartType(), ChartType.BoxandWhisker.getDescription());
        chartMenuTypeOptions[19] = new Option(ChartType.HighLow.getChartType(), ChartType.HighLow.getDescription());
        chartMenuTypeOptions[20] = new Option(ChartType.Histogram.getChartType(), ChartType.Histogram.getDescription());
        chartMenuTypeOptions[21] = new Option(ChartType.Signal.getChartType(), ChartType.Signal.getDescription());
        chartMenuTypeOptions[22] = new Option(ChartType.Wind.getChartType(), ChartType.Wind.getDescription());
        
        // default to a StackedBar graph
        chartMenuType = chartMenuTypeOptions[0].getValue().toString();
        chartTypeMenuSelectedOption = SELECT_CHART_STRING;
   }
     public String getChartMenuType() {
        return chartMenuType;
    }
    
    public void setChartMenuType(String chartMenuType) {
        this.chartMenuType = chartMenuType;
    }
    
    public Option[] getChartMenuTypeOptions() {
        return chartMenuTypeOptions;
    }
    
    public void setChartMenuTypeOptions(Option[] chartMenuTypeOptions) {
        this.chartMenuTypeOptions = chartMenuTypeOptions;
    }
    
    public String getChartTypeMenuSelectedOption() {
        return chartTypeMenuSelectedOption;
    }
    
    public void setChartTypeMenuSelectedOption(String chartTypeMenuSelectedOption) {
        this.chartTypeMenuSelectedOption = chartTypeMenuSelectedOption;
        setChartMenuType(chartTypeMenuSelectedOption);
    }
    
    public String getIsThreeDimensional() {
        return isThreeDimensional;
    }
    
    public void setIsThreeDimensional(String isThreeDimensional) {
        this.isThreeDimensional = isThreeDimensional;
    }
    
    public String getGraphOrientation() {
        return graphOrientation;
    }
    
    public void setGraphOrientation(String orientationOfGraph) {
        this.graphOrientation = orientationOfGraph;
    }
    
    public boolean isThreeDimensionalEffect() {
        return isThreeDimensionalEffect;
    }
    
    public boolean getIsThreeDimensionalEffect() {
        return isThreeDimensionalEffect;
    }    
    
    public void setIsThreeDimensionalEffect(boolean isThreeDimensionalEffect) {
        this.isThreeDimensionalEffect = isThreeDimensionalEffect;
    }
    
    public boolean isHorizontalOrientation() {
        return isHorizontalOrientation;
    }
    
    public boolean getIsHorizontalOrientation() {
        return isHorizontalOrientation;
    }
    
    public void setIsHorizontalOrientation(boolean isHorizontalOrientation) {
        this.isHorizontalOrientation = isHorizontalOrientation;
    }
    
    public void effectChanged(ValueChangeEvent valueChangeEvent) {
        FacesContext context = FacesContext.getCurrentInstance();
        Checkbox checkbox = (Checkbox) valueChangeEvent.getComponent();
        Boolean selected = (Boolean) checkbox.getSelected();
        if(selected.booleanValue() == true) {
            this.isThreeDimensionalEffect = true;
            this.isThreeDimensional = Boolean.TRUE.toString();
        } else {
            this.isThreeDimensionalEffect = false;
            this.isThreeDimensional = Boolean.FALSE.toString();
        }
    }
    
    public void orientationChanged(ValueChangeEvent valueChangeEvent) {
        FacesContext context = FacesContext.getCurrentInstance();
        Checkbox checkbox = (Checkbox) valueChangeEvent.getComponent();
        Boolean selected = (Boolean) checkbox.getSelected();
        if(selected.booleanValue() == true) {
            this.isHorizontalOrientation = true;
            this.graphOrientation = "horizontal";
        } else {
            this.isHorizontalOrientation = false;
            this.graphOrientation = "vertical";
        }
    }
    
    public void checkBoxChanged(ValueChangeEvent valueChangeEvent) {
        FacesContext context = FacesContext.getCurrentInstance();
        Checkbox checkbox = (Checkbox) valueChangeEvent.getComponent();
        if(checkbox.getLabel().equals(this.getThreeDimensional()) == true) {
            this.effectChanged(valueChangeEvent);
        }
        if(checkbox.getLabel().equals(this.getOrientationHorizontal()) == true) {
            this.orientationChanged(valueChangeEvent);
        }
    }
    
    public String getThreeDimensional() {
        return THREE_DIMENSIONAL;
    }
    
    public String getTwoDimensional() {
        return TWO_DIMENSIONAL;
    }
    
    public String getOrientationVertical() {
        return VERTICAL_ORIENTATION;
    }
    
    public String getOrientationHorizontal() {
        return HORIZONTAL_ORIENTATION;
    }
    /** Action listener for the test case dropdown menu. */
    public void processChartTypeMenuSelection(ActionEvent actionEvent) {
        // Since the action is immediate, the components won't go through the
        // Update Model phase.  So, we need to explicitly set the values and
        // update the model object for the given action selected.
        FacesContext context = FacesContext.getCurrentInstance();
        DropDown dropDown = (DropDown) actionEvent.getComponent();
        String selected = (String) dropDown.getSelected();
        this.setChartMenuType(selected);
        this.setChartTypeMenuSelectedOption(selected);
    }  
    
    protected boolean isBarChart() {
        if(chartMenuType.equals(ChartType.Bar.getChartType()) || 
           chartMenuType.equals(ChartType.StackedBar.getChartType())) {
            return true;
        }
        return false;
    }
    
    protected boolean isPieChart() {
        if(chartMenuType.equals(ChartType.Pie.getChartType()) || 
           chartMenuType.equals(ChartType.Ring.getChartType())) {
            return true;
        }
        return false;
    }
    
    protected boolean isLineChart() {
        if(chartMenuType.equals(ChartType.Line.getChartType()) || 
           chartMenuType.equals(ChartType.XYline.getChartType())) {
            return true;
        }
        return false;
    }
    
    protected List<String> getServiceUnitList() {
        return suNameList;
    }
    
    protected void setServiceUnitsList(List<String> suNameList) {
        this.suNameList = suNameList;
    }
    
    public void setAggregatedView(boolean aggregatedView) {
        this.aggregatedView = aggregatedView;
    }
    
    public void setServiceUnitSelected(String serviceUnitSelected) {
        this.serviceUnitSelected = serviceUnitSelected;
    }
    
    protected abstract Dataset getChartData();
}
