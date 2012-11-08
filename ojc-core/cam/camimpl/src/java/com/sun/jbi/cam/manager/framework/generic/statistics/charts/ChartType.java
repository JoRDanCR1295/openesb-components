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
 * @(#)ChartType.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.generic.statistics.charts;

import com.sun.jbi.cam.common.resources.Messages;

/**
 *
 * @author graj
 */
public enum ChartType {
    // The 24 chart types
    Bar("bar"),
    StackedBar("stackedbar"),
    Line("line"),
    Area("area"),
    StackedArea("stackedarea"),
    Waterfall("waterfall"),
    Gantt("gantt"),
    Pie("pie"),
    Ring("ring"),
    Timeseries("timeseries"),
    XYline("xyline"),
    Polar("polar"),
    Scatter("scatter"),
    XYArea("xyarea"),
    XYStepArea("xysteparea"),
    XYStep("xystep"),
    Bubble("bubble"),
    Candlestick("candlestick"),
    BoxandWhisker("boxandwhisker"),
    HighLow("highlow"),
    Histogram("histogram"),
    Signal("signal"),
    Wind("wind");
    
    private String chartType;
    
    /** @param typeOfChart */
    private ChartType(String typeOfChart) {
        this.chartType = typeOfChart;
    }
    
    /** @return the chartType */
    public String getChartType() {
        return chartType;
    }
    
    /** @param typeOfChart */
    public void setChartType(String typeOfChart) {
        this.chartType = typeOfChart;
    }
    
    /** @return the description */
    public String getDescription() {
        switch (this) {
            case Bar: return Messages.getString("chart_type_bar");
            case StackedBar: return Messages.getString("chart_type_stackedBar");
            case Line: return Messages.getString("chart_type_line");
            case Area: return Messages.getString("chart_type_area");
            case StackedArea: return Messages.getString("chart_type_stackedArea");
            case Waterfall: return Messages.getString("chart_type_waterfall");
            case Gantt: return Messages.getString("chart_type_gantt");
            case Pie: return Messages.getString("chart_type_pie");
            case Ring: return Messages.getString("chart_type_ring");
            case Timeseries: return Messages.getString("chart_type_timeseries");
            case XYline: return Messages.getString("chart_type_xyLine");
            case Polar: return Messages.getString("chart_type_polar");
            case Scatter: return Messages.getString("chart_type_scatter");
            case XYArea: return Messages.getString("chart_type_xyArea");
            case XYStepArea: return Messages.getString("chart_type_xyStepArea");
            case XYStep: return Messages.getString("chart_type_xyStep");
            case Bubble: return Messages.getString("chart_type_bubble");
            case Candlestick: return Messages.getString("chart_type_candlestick");
            case BoxandWhisker: return Messages.getString("chart_type_boxandWhisker");
            case HighLow: return Messages.getString("chart_type_highLow");
            case Histogram: return Messages.getString("chart_type_histogram");
            case Signal: return Messages.getString("chart_type_signal");
            case Wind: return Messages.getString("chart_type_wind");
            default:
                return "Unknown";
        }
    }
}
