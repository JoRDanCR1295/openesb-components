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

import com.sun.webui.jsf.model.Option;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.data.general.Dataset;

/**
 *
 * @author rdamir
 */
public final class BPCustomCharts extends ChartSupport {
    
    BPCustomChartBean bpCustomBean;
    
    /** Creates a new instance of BPCustomCharts */
    public BPCustomCharts() {
      FacesContext BPCustomcontext =  FacesContext.getCurrentInstance();
      ValueBinding bpCustomValueBinding = 
            BPCustomcontext.getApplication().createValueBinding(BPCustomChartBean.BEAN_NAME);
       bpCustomBean = 
         (BPCustomChartBean) bpCustomValueBinding.getValue(BPCustomcontext);        
   }
    
     public Option[] getChartMenuTypeOptions() {
        Option[] currentChartOption = null;
         
        BPCustomChartBean.VAR_TYPE xType = bpCustomBean.getCurrentXVaribleType(); 
        BPCustomChartBean.VAR_TYPE yType = bpCustomBean.getCurrentYVaribleType(); 
       
        if(xType == BPCustomChartBean.VAR_TYPE.NUMERIC){
             if(yType == BPCustomChartBean.VAR_TYPE.NUMERIC) {
                   currentChartOption = getNumericNumericChartOptions();
             } else if(yType == BPCustomChartBean.VAR_TYPE.DATETIME) {      
                     currentChartOption = getNumericDateChartOptions();
             } else if(yType == BPCustomChartBean.VAR_TYPE.STRING) {      
                     currentChartOption = getNumericStringOptions();
             } else if(yType == BPCustomChartBean.VAR_TYPE.BOOLEAN) {      
                     currentChartOption = getNumericBooleanOptions();
             } else {
                 // invalid 
                currentChartOption = new Option[1];
                currentChartOption[0] = new Option(SELECT_CHART_STRING);
            }
        }
        return currentChartOption;
    }

    
    
    
    
    public Dataset getChartData() {
        return new DefaultCategoryDataset();
    }
 
    
    private Option[] getNumericNumericChartOptions() {
        Option[] currentChartOption = new Option[1];
        currentChartOption[0] = new Option(SELECT_CHART_STRING);
        return currentChartOption;

    }

    
    private Option[] getNumericDateChartOptions() {
        Option[] currentChartOption = new Option[1];
        currentChartOption[0] = new Option(SELECT_CHART_STRING);
        return currentChartOption;

    }

    private Option[] getNumericStringOptions() {
        Option[] currentChartOption = new Option[1];
        currentChartOption[0] = new Option(SELECT_CHART_STRING);
        return currentChartOption;

    }

    
    private Option[] getNumericBooleanOptions() {
        Option[] currentChartOption = new Option[1];
        currentChartOption[0] = new Option(SELECT_CHART_STRING);
        return currentChartOption;

    }

    
}
