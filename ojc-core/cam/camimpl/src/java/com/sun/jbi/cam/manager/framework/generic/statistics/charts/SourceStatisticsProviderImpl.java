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
 * @(#)SourceStatisticsProviderImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.generic.statistics.charts;

import java.io.Serializable;
import java.util.List;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.data.general.DefaultPieDataset;
import org.jfree.data.time.TimeSeriesCollection;
import org.jfree.data.xy.XYDataset;

/**
 *
 * @author graj
 */
public class SourceStatisticsProviderImpl implements SourceStatisticsProvider, Serializable {
    
    /**
     * Creates a new instance of SourceStatisticsProviderImpl
     */
    public SourceStatisticsProviderImpl() {
    }
    
    public DefaultCategoryDataset getCategoryDataset(List<BarChartData> barChartData) {
        DefaultCategoryDataset defaultCategoryDataset = new DefaultCategoryDataset();
        if(barChartData == null) {
            return defaultCategoryDataset;
        }
        
        for (BarChartData chartDatum : barChartData) {
            if(chartDatum != null) {
                if((chartDatum.getCategory() != null)
                && (chartDatum.getEndpointName() != null)) {
                    defaultCategoryDataset.addValue(chartDatum.getHorizontalValue(),
                            chartDatum.getCategory(),
                            chartDatum.getEndpointName());
                }
            }
        }
        return defaultCategoryDataset;
    }
    
    
    public DefaultPieDataset getPieDataset() {
        DefaultPieDataset defaultPieDataset = new DefaultPieDataset();
        return defaultPieDataset;
    }
    
    public XYDataset getXYDataset() {
        XYDataset dataSet = new TimeSeriesCollection();
        return dataSet;
    }
}
