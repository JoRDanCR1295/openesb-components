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
 * @(#)ChartStatistics.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.generic;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.manager.framework.generic.statistics.charts.BarChartData;
import com.sun.jbi.cam.manager.framework.generic.statistics.charts.ChartType;
import com.sun.jbi.cam.manager.framework.generic.statistics.charts.PieChartData;
import com.sun.jbi.cam.manager.framework.generic.statistics.charts.SourceStatisticsProvider;
import com.sun.jbi.cam.manager.framework.generic.statistics.charts.SourceStatisticsProviderImpl;
import com.sun.webui.jsf.component.Checkbox;
import com.sun.webui.jsf.component.DropDown;
import com.sun.webui.jsf.component.Listbox;
import com.sun.webui.jsf.model.Option;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.faces.event.ValueChangeEvent;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.data.general.DefaultPieDataset;

/**
 *
 * @author graj
 */
public class ChartStatistics  extends StatisticsBean implements Serializable {
    
    private static String CATEGORY_RECEIVED_REQUESTS;
    private static String CATEGORY_RECEIVED_REPLIES;
    private static String CATEGORY_RECEIVED_ERRORS;
    private static String CATEGORY_RECEIVED_DONES;
    private static String CATEGORY_SENT_REQUESTS;
    private static String CATEGORY_SENT_REPLIES;
    private static String CATEGORY_SENT_ERRORS;
    private static String CATEGORY_SENT_DONES;
    
    private static String THREE_DIMENSIONAL;
    private static String TWO_DIMENSIONAL;
    
    private static String VERTICAL_ORIENTATION;
    private static String HORIZONTAL_ORIENTATION;
    
    private String chartMenuType;
    private String totalsChartMenuType;
    
    /** Holds value of property Chart type Menu options. */
    private Option[] chartMenuTypeOptions = null;
    
    /** Holds value of property Chart type Menu options. */
    private Option[] totalsChartMenuTypeOptions = null;
    
    
    /** Holds value of property listboxSelectedOption. */
    private String chartTypeMenuSelectedOption = null;

    /** Holds value of property listboxSelectedOption. */
    private String totalsChartTypeMenuSelectedOption = null;
    
    private String isThreeDimensional = Boolean.TRUE.toString();
    
    private String graphOrientation = "horizontal";
    
    private boolean isThreeDimensionalEffect = true;
    private boolean isHorizontalOrientation = true;
    
    static {
        
        CATEGORY_RECEIVED_REQUESTS = Messages.getString("statistics_receivedRequests");
        CATEGORY_RECEIVED_REPLIES  = Messages.getString("statistics_receivedReplies");
        CATEGORY_RECEIVED_ERRORS   = Messages.getString("statistics_receivedErrors");
        CATEGORY_RECEIVED_DONES    = Messages.getString("statistics_receivedDones");
        CATEGORY_SENT_REQUESTS = Messages.getString("statistics_sentRequests");
        CATEGORY_SENT_REPLIES  = Messages.getString("statistics_sentReplies");
        CATEGORY_SENT_ERRORS   = Messages.getString("statistics_sentErrors");
        CATEGORY_SENT_DONES    = Messages.getString("statistics_sentDones");
        
        THREE_DIMENSIONAL    = Messages.getString("chart_effect_threeDimensional");
        TWO_DIMENSIONAL    = Messages.getString("chart_effect_twoDimensional");
        
        VERTICAL_ORIENTATION    = Messages.getString("chart_orientation_vertical");
        HORIZONTAL_ORIENTATION    = Messages.getString("chart_orientation_horizontal");
        
    }
    
    /**
     * Creates a new instance of ChartStatistics
     */
    public ChartStatistics() {
        // Chart type Menu options.
        chartMenuTypeOptions = new Option[6];
        chartMenuTypeOptions[0] = new Option(ChartType.Bar.getChartType(), ChartType.Bar.getDescription());
        chartMenuTypeOptions[1] = new Option(ChartType.StackedBar.getChartType(), ChartType.StackedBar.getDescription());
        chartMenuTypeOptions[2] = new Option(ChartType.Line.getChartType(), ChartType.Line.getDescription());
        chartMenuTypeOptions[3] = new Option(ChartType.Area.getChartType(), ChartType.Area.getDescription());
        chartMenuTypeOptions[4] = new Option(ChartType.StackedArea.getChartType(), ChartType.StackedArea.getDescription());
        chartMenuTypeOptions[5] = new Option(ChartType.Waterfall.getChartType(), ChartType.Waterfall.getDescription());
        /*
        chartMenuTypeOptions[6] = new Option(ChartType.Gantt.getChartType(), ChartType.Gantt.getDescription());
        chartMenuTypeOptions[7] = new Option(ChartType.Pie.getChartType(), ChartType.Pie.getDescription());
        chartMenuTypeOptions[8] = new Option(ChartType.Ring.getChartType(), ChartType.Ring.getDescription());
        chartMenuTypeOptions[9] = new Option(ChartType.Timeseries.getChartType(), ChartType.Timeseries.getDescription());
        chartMenuTypeOptions[10] = new Option(ChartType.XYline.getChartType(), ChartType.XYline.getDescription());
        chartMenuTypeOptions[11] = new Option(ChartType.Polar.getChartType(), ChartType.Polar.getDescription());
        chartMenuTypeOptions[12] = new Option(ChartType.Scatter.getChartType(), ChartType.Scatter.getDescription());
        chartMenuTypeOptions[13] = new Option(ChartType.XYArea.getChartType(), ChartType.XYArea.getDescription());
        chartMenuTypeOptions[14] = new Option(ChartType.XYStepArea.getChartType(), ChartType.XYStepArea.getDescription());
        chartMenuTypeOptions[15] = new Option(ChartType.XYStep.getChartType(), ChartType.XYStep.getDescription());
        chartMenuTypeOptions[16] = new Option(ChartType.Bubble.getChartType(), ChartType.Bubble.getDescription());
        chartMenuTypeOptions[17] = new Option(ChartType.Candlestick.getChartType(), ChartType.Candlestick.getDescription());
        chartMenuTypeOptions[18] = new Option(ChartType.BoxandWhisker.getChartType(), ChartType.BoxandWhisker.getDescription());
        chartMenuTypeOptions[19] = new Option(ChartType.HighLow.getChartType(), ChartType.HighLow.getDescription());
        chartMenuTypeOptions[20] = new Option(ChartType.Histogram.getChartType(), ChartType.Histogram.getDescription());
        chartMenuTypeOptions[21] = new Option(ChartType.Signal.getChartType(), ChartType.Signal.getDescription());
        chartMenuTypeOptions[22] = new Option(ChartType.Wind.getChartType(), ChartType.Wind.getDescription());
         */
        // default to a Bar graph
        chartMenuType = chartMenuTypeOptions[0].getValue().toString();
        chartTypeMenuSelectedOption = chartMenuType;
        
        // Chart type Menu options.
        totalsChartMenuTypeOptions = new Option[2];
        totalsChartMenuTypeOptions[0] = new Option(ChartType.Pie.getChartType(), ChartType.Pie.getDescription());
        totalsChartMenuTypeOptions[1] = new Option(ChartType.Ring.getChartType(), ChartType.Ring.getDescription());
        // default to a Pie graph
        totalsChartMenuType = totalsChartMenuTypeOptions[0].getValue().toString();
        totalsChartTypeMenuSelectedOption = totalsChartMenuType;
        
    }
    
    public DefaultPieDataset getConsumingEndpointTotalsPieChartData() {
        PieChartData totalsChartDatum = null;
        List<PieChartData> pieChartData = new ArrayList<PieChartData>();
        Map<String, Double> consumingTotalsMap = this.getConsumingTotalsList();
        double totalReceivedRequests = 0D,
                totalReceivedReplies = 0D,
                totalReceivedErrors = 0D,
                totalReceivedDones = 0D,
                totalSentRequests = 0D,
                totalSentReplies = 0D,
                totalSentErrors = 0D,
                totalSentDones = 0D;
        totalReceivedRequests = ((Double)consumingTotalsMap.get(Messages.getString("statistics_receivedRequests"))).doubleValue();
        totalReceivedReplies = ((Double)consumingTotalsMap.get(Messages.getString("statistics_receivedReplies"))).doubleValue();
        totalReceivedErrors  =((Double)consumingTotalsMap.get(Messages.getString("statistics_receivedErrors"))).doubleValue();
        totalReceivedDones = ((Double)consumingTotalsMap.get(Messages.getString("statistics_receivedDones"))).doubleValue();
        totalSentRequests = ((Double)consumingTotalsMap.get(Messages.getString("statistics_sentRequests"))).doubleValue();
        totalSentReplies = ((Double)consumingTotalsMap.get(Messages.getString("statistics_sentReplies"))).doubleValue();
        totalSentErrors = ((Double)consumingTotalsMap.get(Messages.getString("statistics_sentErrors"))).doubleValue();
        totalSentDones = ((Double)consumingTotalsMap.get(Messages.getString("statistics_sentDones"))).doubleValue();
        
        DefaultPieDataset defaultPieDataset = new DefaultPieDataset();
        defaultPieDataset.setValue(Messages.getString("statistics_receivedRequests"), totalReceivedRequests);
        defaultPieDataset.setValue(Messages.getString("statistics_receivedReplies"), totalReceivedReplies);
        defaultPieDataset.setValue(Messages.getString("statistics_receivedErrors"), totalReceivedErrors);
        defaultPieDataset.setValue(Messages.getString("statistics_receivedDones"), totalReceivedDones);
        defaultPieDataset.setValue(Messages.getString("statistics_sentRequests"), totalSentRequests);
        defaultPieDataset.setValue(Messages.getString("statistics_sentReplies"), totalSentReplies);
        defaultPieDataset.setValue(Messages.getString("statistics_sentErrors"), totalSentErrors);
        defaultPieDataset.setValue(Messages.getString("statistics_sentDones"), totalSentDones);
        return defaultPieDataset;  
    }
    
    public DefaultCategoryDataset getConsumingEndpointsBarChartData() {
        BarChartData chartDatum = null;
        List<BarChartData> barChartData = new ArrayList<BarChartData>();
        List<DisplayStatistics> displayStatisticsList = this.getConsumingStatisticsList();
        if((displayStatisticsList == null)
        || (displayStatisticsList.size() == 0)) {
            return new DefaultCategoryDataset();
        }
        for (DisplayStatistics statistics : displayStatisticsList) {
            if(statistics != null) {
                chartDatum = new BarChartData(statistics.getReceivedRequests(),
                        CATEGORY_RECEIVED_REQUESTS,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedReplies(),
                        CATEGORY_RECEIVED_REPLIES,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedErrors(),
                        CATEGORY_RECEIVED_ERRORS,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedDones(),
                        CATEGORY_RECEIVED_DONES,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedRequests(),
                        CATEGORY_SENT_REQUESTS,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedReplies(),
                        CATEGORY_SENT_REPLIES,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedErrors(),
                        CATEGORY_SENT_ERRORS,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedDones(),
                        CATEGORY_SENT_DONES,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
            }
        }
        
        SourceStatisticsProvider provider = new SourceStatisticsProviderImpl();
        DefaultCategoryDataset consumingEndpointsBarChartData = provider.getCategoryDataset(barChartData);
        return consumingEndpointsBarChartData;
    }

    public DefaultPieDataset getProvisioningEndpointTotalsPieChartData() {
        PieChartData totalsChartDatum = null;
        List<PieChartData> pieChartData = new ArrayList<PieChartData>();
        Map<String, Double> provisioningTotalsMap = this.getProvisioningTotalsList();
        double totalReceivedRequests = 0D,
                totalReceivedReplies = 0D,
                totalReceivedErrors = 0D,
                totalReceivedDones = 0D,
                totalSentRequests = 0D,
                totalSentReplies = 0D,
                totalSentErrors = 0D,
                totalSentDones = 0D;
        totalReceivedRequests = ((Double)provisioningTotalsMap.get(Messages.getString("statistics_receivedRequests"))).doubleValue();
        totalReceivedReplies = ((Double)provisioningTotalsMap.get(Messages.getString("statistics_receivedReplies"))).doubleValue();
        totalReceivedErrors  =((Double)provisioningTotalsMap.get(Messages.getString("statistics_receivedErrors"))).doubleValue();
        totalReceivedDones = ((Double)provisioningTotalsMap.get(Messages.getString("statistics_receivedDones"))).doubleValue();
        totalSentRequests = ((Double)provisioningTotalsMap.get(Messages.getString("statistics_sentRequests"))).doubleValue();
        totalSentReplies = ((Double)provisioningTotalsMap.get(Messages.getString("statistics_sentReplies"))).doubleValue();
        totalSentErrors = ((Double)provisioningTotalsMap.get(Messages.getString("statistics_sentErrors"))).doubleValue();
        totalSentDones = ((Double)provisioningTotalsMap.get(Messages.getString("statistics_sentDones"))).doubleValue();
        
        DefaultPieDataset defaultPieDataset = new DefaultPieDataset();
        defaultPieDataset.setValue(Messages.getString("statistics_receivedRequests"), totalReceivedRequests);
        defaultPieDataset.setValue(Messages.getString("statistics_receivedReplies"), totalReceivedReplies);
        defaultPieDataset.setValue(Messages.getString("statistics_receivedErrors"), totalReceivedErrors);
        defaultPieDataset.setValue(Messages.getString("statistics_receivedDones"), totalReceivedDones);
        defaultPieDataset.setValue(Messages.getString("statistics_sentRequests"), totalSentRequests);
        defaultPieDataset.setValue(Messages.getString("statistics_sentReplies"), totalSentReplies);
        defaultPieDataset.setValue(Messages.getString("statistics_sentErrors"), totalSentErrors);
        defaultPieDataset.setValue(Messages.getString("statistics_sentDones"), totalSentDones);
        return defaultPieDataset;  
    }
    
    
    public DefaultCategoryDataset getProvisioningEndpointsBarChartData() {
        BarChartData chartDatum = null;
        List<BarChartData> barChartData = new ArrayList<BarChartData>();
        List<DisplayStatistics> displayStatisticsList = this.getProvisioningStatisticsList();
        if((displayStatisticsList == null)
        || (displayStatisticsList.size() == 0)) {
            return new DefaultCategoryDataset();
        }
        for (DisplayStatistics statistics : displayStatisticsList) {
            if(statistics != null) {
                chartDatum = new BarChartData(statistics.getReceivedRequests(),
                        CATEGORY_RECEIVED_REQUESTS,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedReplies(),
                        CATEGORY_RECEIVED_REPLIES,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedErrors(),
                        CATEGORY_RECEIVED_ERRORS,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedDones(),
                        CATEGORY_RECEIVED_DONES,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedRequests(),
                        CATEGORY_SENT_REQUESTS,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedReplies(),
                        CATEGORY_SENT_REPLIES,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedErrors(),
                        CATEGORY_SENT_ERRORS,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
                
                chartDatum = new BarChartData(statistics.getReceivedDones(),
                        CATEGORY_SENT_DONES,
                        statistics.getEndpointShort());
                barChartData.add(chartDatum);
            }
        }
        
        SourceStatisticsProvider provider = new SourceStatisticsProviderImpl();
        DefaultCategoryDataset provisioningEndpointsBarChartData = provider.getCategoryDataset(barChartData);
        return provisioningEndpointsBarChartData;
    }
    
    
    
    /** Action listener for the test case dropdown menu. */
    public void processChartMenuSelection(ActionEvent actionEvent) {
        // Since the action is immediate, the components won't go through the
        // Update Model phase.  So, we need to explicitly set the values and
        // update the model object for the given action selected.
        FacesContext context = FacesContext.getCurrentInstance();
        DropDown dropDown = (DropDown) actionEvent.getComponent();
        String selected = (String) dropDown.getSelected();
        this.setChartMenuType(selected);
        this.setChartTypeMenuSelectedOption(selected);
    }
    
    /** Action listener for the test case dropdown menu. */
    public void processTotalsChartMenuSelection(ActionEvent actionEvent) {
        // Since the action is immediate, the components won't go through the
        // Update Model phase.  So, we need to explicitly set the values and
        // update the model object for the given action selected.
        FacesContext context = FacesContext.getCurrentInstance();
        DropDown dropDown = (DropDown) actionEvent.getComponent();
        String selected = (String) dropDown.getSelected();
        this.setTotalsChartMenuType(selected);
        this.setTotalsChartTypeMenuSelectedOption(selected);
    }    
    
    public String getChartMenuType() {
        return chartMenuType;
    }
    
    public void setChartMenuType(String chartMenuType) {
        this.chartMenuType = chartMenuType;
    }
    
    public String getTotalsChartMenuType() {
        return this.totalsChartMenuType;
    }
    
    public void setTotalsChartMenuType(String chartMenuType) {
        this.totalsChartMenuType = chartMenuType;
    }
    
    
    public Option[] getChartMenuTypeOptions() {
        return chartMenuTypeOptions;
    }
    
    public void setChartMenuTypeOptions(Option[] chartMenuTypeOptions) {
        this.chartMenuTypeOptions = chartMenuTypeOptions;
    }
    
    public Option[] getTotalsChartMenuTypeOptions() {
        return totalsChartMenuTypeOptions;
    }
    
    public void setTotalsChartMenuTypeOptions(Option[] chartMenuTypeOptions) {
        this.totalsChartMenuTypeOptions = chartMenuTypeOptions;
    }
    
    
    public String getChartTypeMenuSelectedOption() {
        return chartTypeMenuSelectedOption;
    }
    
    public void setChartTypeMenuSelectedOption(String chartTypeMenuSelectedOption) {
        this.chartTypeMenuSelectedOption = chartTypeMenuSelectedOption;
    }
    
    public String getTotalsChartTypeMenuSelectedOption() {
        return totalsChartTypeMenuSelectedOption;
    }
    
    public void setTotalsChartTypeMenuSelectedOption(String chartTypeMenuSelectedOption) {
        this.totalsChartTypeMenuSelectedOption = chartTypeMenuSelectedOption;
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
    
    public String showTable() {
        // switch to tabular view   
        System.out.println(">>>> show tabular view");
        return GenericConstants.SUCCESS;         //$NON-NLS-1$
    }    
}
